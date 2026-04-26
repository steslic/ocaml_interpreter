# ocaml_interpreter

A stack-based interpreter for a custom programming language, written in OCaml. Features a parser combinator library and a recursive evaluator supporting first-class functions, lexical scoping, and exception handling. This project was built to better understand how interpreters work, especially parsing and environment handling.

## Features
- **Parser combinator library** — built using monadic composition (`let*`) with automatic failure propagation
- **First-class functions** — `Fun/End` defines closures that can be stored, passed, and invoked
- **Lexical scoping** — closures capture their definition-time environment
- **Local and global environments** — variable binding via `Local`, `Global`, and `Lookup`
- **Scoped blocks** — `Begin/End` executes in an isolated stack and returns the top value
- **Control flow** — `If/Else/End` and `Switch/Case` branching
- **Error handling** — `Try/End` enables recovery from runtime errors
- **Arithmetic and logic** — `Add`, `Sub`, `Mul`, `Div`, `Equal`, `Lte`, `And`, `Or`, `Not`

## Build
ocamlopt interpreter.ml -o interp

## Usage
This project exposes an `interp` function that takes source code as a string and returns a list of trace outputs.

### Using the OCaml REPL

```ocaml
# #use "interpreter.ml";;
# interp "Push 5 Push 3 Add 2 Trace 1";;
```
**Output:**
```ocaml
- : string list = ["8"]
```

## Language Overview

### Types

| Type | Example |
|------|---------|
| Integer | `42`, `7` |
| Boolean | `True`, `False` |
| Unit | `()` |
| Name | `x`, `my_var` |
| Closure | created by `Fun` |

### Commands

| Command | Description |
|---------|-------------|
| `Push const` | Push a constant onto the stack |
| `Pop int` | Pop n values off the stack |
| `Trace int` | Log top n stack values to output |
| `Add/Sub/Mul/Div int` | Arithmetic on top n integer values |
| `Equal` | Push `True` if top two integers are equal |
| `Lte` | Push `True` if top integer <= second integer |
| `And`, `Or`, `Not` | Boolean logic on top values |
| `Local` | Bind top name to value below it in local scope |
| `Global` | Bind top name to value below it in global scope |
| `Lookup` | Replace top name with its bound value |
| `Begin ... End` | Isolated block — pushes top of inner stack on return |
| `If ... Else ... End` | Branch on top boolean |
| `Fun f x ... End` | Define recursive function `f` with parameter `x` |
| `Call` | Apply closure on top of stack to argument below it |
| `Try ... End` | Recover from runtime errors silently |
| `Switch Case int ... End` | Integer pattern matching |

### Example — Recursive Factorial
Defines a recursive function `fact(n)` that computes factorial using closures and lexical scoping.

```
Fun fact n
  Push n
  Push 0
  Equal
  If
    Push 1
  Else
    Push fact
    Lookup
    Push n
    Lookup
    Push 1
    Sub 2
    Local n
    Push n
    Lookup
    Call
    Push n
    Lookup
    Mul 2
  End
End
Push fact
Lookup
Push 5
Call
Trace 1
```
**Output:**
```ocaml
- : string list = ["120"]
```

## Implementation

**Parser**
- `type 'a parser = char list -> ('a * char list) option`
- Combinators: `pure`, `bind` (`let*`), `alt` (`<|>`), `many`, `map`
- Parsing steps are chained together so that a failure at any step stops the entire parse.
- Recursive descent — `parse_command` calls `parse_beginend`, `parse_funend`,
  etc. which recursively call `parse_cmdlst`

**Evaluator**
- Single recursive `eval` function with signature:
  `stack -> prog -> log -> log -> local_env -> global_env -> ...`
- Closures store their definition-time environment:
  `Closure(local, global, name, param, body)`
- At `Call`, the closure’s saved environment is restored, and the function is bound to its own name within that environment, enabling recursion
- Errors propagate as `["Error"]` in the log tuple and are caught by `Try/End`
- `Begin/End` runs `eval` on a fresh stack and pushes the result back
