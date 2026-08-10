# Wolfram textual subset

`DIA_WOLFRAM` is a bounded, clean-room textual frontend for the Wolfram
Language forms present in the consumer derivations. It is a parser and printer,
not a Mathematica implementation, notebook reader, evaluator service, or
product affiliation. Unknown heads remain opaque `expr_t` applications so the
parser does not silently discard a derivation; command evaluation is a separate
bounded layer in `fortsym_wl`.

## Published subset

| Form | Status and representation |
|---|---|
| Names, `$` names, and context marks such as `Global\`x` | accepted as symbol names |
| Integers, exact `p/q`, decimal/scientific reals, and Wolfram precision annotations | exact integer/rational nodes where applicable; approximate values are bounded to the native real node |
| `Pi`, `E`, `I`, and ordinary names | canonical constants for the three documented constants; other names remain symbols |
| `+`, unary/binary `-`, `*`, `/`, `^`, `.` | canonical add, multiply, reciprocal/power, or opaque `Dot` application |
| implicit multiplication, parentheses, and standard Wolfram precedence | canonical multiplication and precedence-preserving grouping |
| `Head[arg, ...]` and prefix `@` | canonical applied-function nodes |
| postfix `expr'`, `//`, `/@`, `@@`, and `@@@` | structural heads used by the bounded Wolfram session |
| `{a, b, ...}` and `<|a -> b, ...|>` | `List` and `Association` structural heads |
| `lhs -> rhs`, `lhs :> rhs`, `==`, `=`, `:=`, and `/.` | structural rule, equation, assignment, and replacement heads; execution is handled only by the bounded session |
| slots `#`, `#n`, `##`, pure-function `&`, patterns, spans, and named characters `\[Name]` | structural heads where the bounded session has a documented consumer use; unsupported matching is refused by name |
| comments `(* ... *)`, including nesting | discarded lexically; whitespace and newlines are insignificant outside literals |

The printer emits a stable spelling in the same dialect. The Wolfram test
constructs expected nodes directly and also parses the printer output back into
the same arena; hash-consed node identity is the structural oracle. Exact
rationals are never converted to binary floating point.

## Refusal boundary

The frontend does not read `.nb` notebook boxes, graphics, dynamic constructs,
packages, service calls, or general pattern/evaluation state. It does not call
Mathematica, `wolframscript`, Wolfram Cloud, or copy a GPL implementation.
Malformed or unsupported text returns `ok = .false.` and a diagnostic naming
the first failure together with its one-based source position. The parser never
evaluates commands while constructing the expression graph.

`fortsym_wl` adds only explicitly bounded command semantics. Its current
coverage is measured against the real corrugation-resistance derivation in
[`wolfram-coverage-39.md`](wolfram-coverage-39.md); the report records every
evaluated binding and every named refusal. Mathics is used as a separate
behavioural oracle for the documented textual forms and corpus measurements;
its source and tests are not copied into fortsym.

## Independent checks

```sh
ctest --test-dir build --output-on-failure -R test_fortsym_wolfram
```

The test covers bracket application, exactness, nested comments, whitespace,
implicit multiplication, precedence, lists, associations, rules, postfix and
pattern syntax, named-character suffixes, malformed-input refusal, and
parse/print structural round trips. Representative forms can be checked
against the installed Mathics oracle with the same source spelling; a
difference is recorded as a finding rather than treated as agreement.
