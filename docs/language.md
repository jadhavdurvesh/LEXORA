# LEXORA language and compiler notes

## Syntax

The initial expression grammar supports:

    expression := let-expression
                | if-expression
                | lambda-expression
                | comparison

    let-expression := "let" IDENT "=" expression "in" expression
    if-expression := "if" expression "then" expression "else" expression
    lambda-expression := "fn" IDENT "=>" expression

Application binds tighter than multiplication; multiplication binds tighter than addition; comparisons bind looser than arithmetic.

## Types

The initial type language is:

    Type := Int | Bool | Type -> Type | TypeVariable

The inference engine uses fresh type variables, unification and an occurs check. Let-bound values are generalized against the current environment and instantiated at use sites. The implementation is an initial Hindley–Milner-style engine; the roadmap includes stronger substitution propagation and polymorphism regression tests.

## Evaluation

The evaluator uses lexical environments. Evaluating a lambda captures its current environment and produces a closure. Function application evaluates the argument and evaluates the body in the captured environment extended with the parameter binding.

## Intermediate representation

The initial IR is a stack-oriented instruction listing. It is useful for inspecting the compiler pipeline but is not yet executable bytecode and does not claim LLVM compatibility. The native backend milestone will define typed blocks, explicit control-flow edges, stack verification, and LLVM lowering.

## Error boundaries

Lexical and parse errors include source line and column. Type errors identify mismatched types, unbound variables, and occurs-check failures. Runtime errors include division by zero and invalid operations.
