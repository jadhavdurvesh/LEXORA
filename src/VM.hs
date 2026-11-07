module VM (RuntimeValue(..), VMError(..), runProgram, runProgramWithFuel) where

import qualified Data.Map.Strict as M
import IR

data RuntimeValue = RInt Integer | RBool Bool deriving (Eq, Show)
data VMError
  = MissingLabel String
  | MissingVariable String
  | TypeMismatch String
  | DivisionByZero
  | StackUnderflow
  | FuelExhausted
  | FellOffProgram
  deriving (Eq, Show)

type Stack = [RuntimeValue]
type Environment = M.Map String RuntimeValue

runProgram :: [Instr] -> Either VMError RuntimeValue
runProgram code = runProgramWithFuel 1000000 code

runProgramWithFuel :: Int -> [Instr] -> Either VMError RuntimeValue
runProgramWithFuel fuel code
  | fuel <= 0 = Left FuelExhausted
  | otherwise = execute fuel 0 [] M.empty labels
  where
    labels = M.fromList [(name, pc) | (pc, Label name) <- zip [0..] code]

    execute remaining pc stack env labelMap
      | remaining <= 0 = Left FuelExhausted
      | pc < 0 || pc >= length code = case stack of
          value : _ -> Right value
          [] -> Left FellOffProgram
      | otherwise =
          let next = execute (remaining - 1) (pc + 1)
          in case code !! pc of
            PushInt n -> next (RInt n : stack) env labelMap
            PushBool b -> next (RBool b : stack) env labelMap
            Load name -> case M.lookup name env of
              Nothing -> Left (MissingVariable name)
              Just value -> next (value : stack) env labelMap
            Store name -> case stack of
              value : rest -> next rest (M.insert name value env) labelMap
              [] -> Left StackUnderflow
            AddI -> integerBinary (+) stack next env labelMap
            SubI -> integerBinary (-) stack next env labelMap
            MulI -> integerBinary (*) stack next env labelMap
            DivI -> case stack of
              RInt 0 : RInt _ : _ -> Left DivisionByZero
              RInt b : RInt a : rest -> next (RInt (a `div` b):rest) env labelMap
              _:_:_ -> Left (TypeMismatch "DIV expects two integers")
              _ -> Left StackUnderflow
            EqI -> compareValues (==) stack next env labelMap
            LtI -> integerCompare (<) stack next env labelMap
            GtI -> integerCompare (>) stack next env labelMap
            Jump name -> jumpTo name remaining stack env labelMap
            JumpIfFalse name -> case stack of
              RBool False : rest -> jumpTo name remaining rest env labelMap
              RBool True : rest -> execute (remaining - 1) (pc + 1) rest env labelMap
              _: _ -> Left (TypeMismatch "conditional jump expects Bool")
              [] -> Left StackUnderflow
            Label _ -> next stack env labelMap
            MakeClosure _ -> Left (TypeMismatch "closures are not supported by this VM backend yet")
            Call -> Left (TypeMismatch "calls are not supported by this VM backend yet")
            Return -> case stack of
              value:_ -> Right value
              [] -> Left StackUnderflow

    jumpTo name remaining stack env labelMap =
      case M.lookup name labelMap of
        Nothing -> Left (MissingLabel name)
        Just target -> execute (remaining - 1) target stack env labelMap

    integerBinary operation stack next env labelMap =
      case stack of
        RInt b:RInt a:rest -> next (RInt (operation a b):rest) env labelMap
        _:_:_ -> Left (TypeMismatch "arithmetic instruction expects integers")
        _ -> Left StackUnderflow

    integerCompare operation stack next env labelMap =
      case stack of
        RInt b:RInt a:rest -> next (RBool (operation a b):rest) env labelMap
        _:_:_ -> Left (TypeMismatch "comparison instruction expects integers")
        _ -> Left StackUnderflow

    compareValues operation stack next env labelMap =
      case stack of
        b : a : rest -> next (RBool (operation a b):rest) env labelMap
        _ -> Left StackUnderflow
