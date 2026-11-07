# LEXORA — Independent Product Strategy

## Product identity
LEXORA is an independently usable typed functional language and compiler toolkit, with an educational and experimental implementation that can grow into a reusable language platform.

## Product boundary
- **Core:** lexer, parser, AST, type inference, evaluator, intermediate representation, diagnostics, and compiler passes.
- **Adapters:** source loaders, editor tooling, bytecode/native backends, and embedding APIs.
- **Applications:** language REPL, standalone programs, configuration/policy language experiments, and compiler education examples.

The core must not depend on FireBot or encode FireBot mission concepts. FireBot/RUDRA interoperability is a separate research adapter only.

## Independent use cases
1. Educational compiler and programming-language experiments.
2. Typed configuration or policy language prototypes.
3. Embedded DSL experiments for application-specific rules.

## Product requirements
- Correctness-focused type inference and substitution propagation.
- Source-located diagnostics and a documented language specification.
- Verified IR invariants and a bytecode VM before native backend claims.
- Regression tests for polymorphism, recursion, evaluation, and optimization.
- Stable CLI and a documented embedding/API boundary.
- Clearly distinguish implemented backends from inspectable IR output.

## Non-goals
LEXORA is not a FireBot mission language, and its current stack IR is not native machine code or a verified VM.

## FireBot adapter
Any RUDRA-related experiment must be isolated, optional, and license-reviewed. RUDRA remains independently owned and authoritative for FireBot.

## Readiness gates
- [ ] Language reference and grammar.
- [ ] Complete inference regression suite.
- [ ] IR verifier and executable VM.
- [ ] Standalone examples outside robotics.
- [ ] Independent release and compatibility policy.
