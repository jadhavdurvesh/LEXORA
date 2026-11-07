module IRVerify (verifyLabels, verifyStack) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import IR

-- Validate unique labels and all branch destinations.
verifyLabels :: [Instr] -> Either String ()
verifyLabels instructions = do
  let labels = [name | Label name <- instructions]
      duplicates = duplicatesOf labels
      targets = [name | Jump name <- instructions] ++
                [name | JumpIfFalse name <- instructions]
      known = S.fromList labels
      missing = S.toList (S.difference (S.fromList targets) known)
  case duplicates of
    name:_ -> Left ("duplicate IR label: " ++ name)
    [] -> case missing of
      name:_ -> Left ("undefined IR branch target: " ++ name)
      [] -> Right ()
  where
    duplicatesOf = go S.empty S.empty
    go _ _ [] = []
    go seen emitted (x:xs)
      | S.member x seen =
          if S.member x emitted then go seen emitted xs
          else x : go seen (S.insert x emitted) xs
      | otherwise = go (S.insert x seen) emitted xs

-- Data-flow stack-height verification over the control-flow graph.
-- A merge is valid only when every incoming path has the same stack height.
verifyStack :: [Instr] -> Either String ()
verifyStack instructions = do
  verifyLabels instructions
  let code = M.fromList (zip [0..] instructions)
      labels = M.fromList [(name, pc) | (pc, Label name) <- M.toList code]
      end = length instructions
  walk code labels end (M.singleton 0 0) [0]
  where
    walk _ _ _ _ [] = Right ()
    walk code labels end depths (pc:work)
      | pc == end = walk code labels end depths work
      | otherwise = case M.lookup pc code of
          Nothing -> Left ("invalid IR program counter: " ++ show pc)
          Just instruction -> do
            let before = M.findWithDefault 0 pc depths
                (required, delta) = stackEffect instruction
            if before < required
              then Left ("IR stack underflow at instruction " ++ show pc)
              else do
                let after = before + delta
                    successors = case instruction of
                      Jump name -> maybe [] (:[]) (M.lookup name labels)
                      JumpIfFalse name -> maybe [pc+1] (\target -> [target, pc+1]) (M.lookup name labels)
                      Return -> []
                      _ -> [pc+1]
                (depths', added) <- foldl (insertDepth after) (Right (depths, [])) successors
                walk code labels end depths' (work ++ added)
    insertDepth depth acc pc = do
      (known, added) <- acc
      case M.lookup pc known of
        Just previous
          | previous == depth -> Right (known, added)
          | otherwise -> Left ("inconsistent IR stack height at instruction " ++ show pc)
        Nothing -> Right (M.insert pc depth known, added ++ [pc])

    stackEffect instruction = case instruction of
      PushInt _ -> (0, 1)
      PushBool _ -> (0, 1)
      Load _ -> (0, 1)
      Store _ -> (1, -1)
      AddI -> (2, -1)
      SubI -> (2, -1)
      MulI -> (2, -1)
      DivI -> (2, -1)
      EqI -> (2, -1)
      LtI -> (2, -1)
      GtI -> (2, -1)
      JumpIfFalse _ -> (1, -1)
      Jump _ -> (0, 0)
      Label _ -> (0, 0)
      MakeClosure _ -> (0, 1)
      Call -> (2, -1)
      Return -> (1, -1)
