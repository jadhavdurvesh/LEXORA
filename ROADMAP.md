# LEXORA roadmap

## M0 — Language foundation
- [x] Lexer with source positions
- [x] AST and expression parser
- [x] Integer and boolean values
- [x] Let bindings and conditionals
- [x] Lambda abstraction and application
- [x] Arithmetic and comparisons

## M1 — Static semantics and execution
- [x] Function types
- [x] Type variables and unification
- [x] Occurs check
- [x] Let generalization and instantiation
- [x] Lexical closure evaluator
- [x] Runtime division-by-zero handling
- [x] Initial test suite

## M2 — Compiler pipeline
- [x] Stack-oriented intermediate representation
- [x] Branch labels for conditionals
- [x] Inspectable backend listing
- [x] Safe constant folding and boolean branch elimination
- [ ] Correct substitution propagation across all inference branches
- [ ] Recursive bindings and polymorphic regression suite
- [ ] IR verifier and stack-effect analysis
- [ ] Bytecode VM

## M3 — Advanced language
- [ ] Algebraic data types
- [ ] Pattern matching
- [ ] Exhaustiveness checking
- [ ] Source-level modules and imports
- [ ] Structured diagnostics
- [ ] Tail-call optimization

## M4 — Native backend
- [ ] Real LLVM IR generation
- [ ] LLVM verifier integration
- [ ] Native object and executable emission
- [ ] Optimization passes and benchmarks

- [x] Apply condition-expression substitutions before boolean unification
- [x] Regression coverage for substituted conditional types and higher-order application

- [x] Initial IR control-flow verifier for duplicate labels and undefined branch targets

- [x] Control-flow-aware IR stack-height analysis with underflow and merge validation

- [x] Initial control-flow-aware IR verifier: label validity, stack underflow, and stack-height merge checks


- [x] Correct substitution propagation in conditional and comparison inference paths
