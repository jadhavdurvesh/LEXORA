# LEXORA

**A typed functional language implementation and compiler laboratory in Haskell.**

LEXORA includes a lexer, recursive-descent expression parser, Hindley–Milner-style type inference, tree-walking evaluator with lexical closures, stack-oriented intermediate representation, and an interactive REPL.

## Language sample

    let identity = fn x => x in identity 42

    let twice = fn f => fn x => f (f x) in
      twice (fn n => n + 1) 20

    if 10 > 3 then 100 else 0

## Implemented

- Integer and boolean literals
- Variables and lexical let bindings
- Anonymous functions and function application
- Conditional expressions
- Arithmetic and comparison operators
- Lexical closures in the evaluator
- Type inference for integers, booleans, and function types
- Occurs-check to reject infinite types
- AST constant-folding optimization
- Stack-oriented IR generation
- Inspectable backend listing
- File execution and REPL
- Cabal test suite and CI

## Requirements

- GHC 9.6 or compatible
- Cabal 3.10 or compatible

## Build and run

    cabal build all
    cabal run lexora
    cabal run lexora -- examples/hello.lex
    cabal test --test-show-details=direct

Enter :quit to leave the REPL.

## Architecture

    Source
      |
      v
    Lexer -> Tokens
      |
      v
    Parser -> AST
      |
      +----> Type inference
      |
      +----> Evaluator
      |
      +----> Stack IR -> Backend listing

## Backend status

The current backend output is an inspectable stack-oriented IR listing. It is deliberately not described as LLVM IR or native machine-code generation. A real LLVM backend with verified control-flow and function lowering is a future milestone.

## Current limitations

- Recursive let bindings are not implemented.
- Pattern matching and algebraic data types are not implemented.
- Type inference is an initial implementation and needs broader substitution-propagation and polymorphism tests.
- The current IR is an educational stack representation, not yet a verified executable VM.
- Native LLVM code generation is not implemented.

See ROADMAP.md and docs/language.md.

## License

MIT.

The compiler pipeline includes an initial IR label verifier that rejects duplicate labels and branches to undefined targets before backend processing. Stack-effect and closure-region verification remain future work.
