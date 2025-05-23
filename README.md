<div align="center">

# mallee

Hindley-Milner type inference for a tiny lambda calculus, written in OCaml. Algorithm W with let-polymorphism, a hand-written lexer and parser, and a REPL.

![License](https://img.shields.io/badge/LICENSE-MIT-blue?style=for-the-badge)  ![Language](https://img.shields.io/badge/OCAML-EC6A00?style=for-the-badge&logo=ocaml&logoColor=white)  ![Tests](https://img.shields.io/badge/TESTS-34%20CHECKS-brightgreen?style=for-the-badge)

Refer to [LICENSE](LICENSE) for the legal bits.

</div>

## What's in it

- A small functional language with integers, booleans, `if`, `let`, `fun` and application. `fun x y -> e` and `let f x y = e` work as shorthand.
- Algorithm W with explicit substitutions and let-polymorphism, so `let id = fun x -> x in ...` can be used at several types.
- An occurs check, with an error that names the infinite type it found.
- A lexer, a recursive-descent parser, and a REPL that prints the inferred type of each line.
- An initial environment with `add`, `sub`, `mul`, `eq`, `lt` and `not`.

```
mallee> fun f g x -> f (g x)
- : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
mallee> let id = fun x -> x in if id true then id 1 else id 2
- : int
mallee> let k x y = x
val k : 'a -> 'b -> 'a
mallee> k 1 true
- : int
mallee> fun x -> x x
type error: cannot construct the infinite type 'a = 'a -> 'b
```

A line starting with `let` and no `in` defines a name for later lines.

## 🏗️ Architecture

```
 bin/main.ml           REPL loop ─ reads a line ─▶ Check.run_line env
        │
 src/check.ml          initial_env, run_line : env -> string -> (env * string, string) result
        │  Lexer.tokenize ─▶ Parser.parse_tokens ─▶ Infer.infer
        ├─ src/lexer.ml    tokens: INT IDENT TRUE FALSE IF THEN ELSE LET IN FUN ARROW EQUALS ( ) EOF
        ├─ src/parser.ml   recursive descent ─▶ Ast.toplevel  (Def | Expr)
        ├─ src/ast.ml      Int Bool Var Fun App Let If
        └─ src/infer.ml    ty = TInt | TBool | TVar | TArrow
                           unify / bind (occurs check) / compose
                           generalise / instantiate / infer
 test/test_mallee.ml   run_line on source strings, compared with expected type text
```

- `Infer` keeps substitutions as an `IntMap` from type variable to type and threads them through `infer`, which returns the substitution and the type of each expression. Type variables are numbered by a global counter.
- `generalise` quantifies over variables that are free in the type but not in the environment. `instantiate` replaces them with fresh ones at each use.
- `show` names type variables `'a`, `'b`, ... by order of appearance, so printed types are stable.
- `Check.run_line` turns lexer, parser and type errors into three kinds of message: `lexical error at column N`, `syntax error`, and `type error`.
- Application is left associative and `fun` bodies extend as far to the right as they can.

## 🚀 Quick start

```
dune build
dune test
dune exec bin/main.exe
```

Needs OCaml and dune 3.0 or later. Typing `:quit` leaves the REPL. Piping lines in gives the same output as the session above:

```
$ printf 'let k x y = x\nk 1 true\nfun x -> x x\n' | dune exec bin/main.exe
mallee: type inference REPL (:quit to leave)
mallee> val k : 'a -> 'b -> 'a
mallee> - : int
mallee> type error: cannot construct the infinite type 'a = 'a -> 'b
```

## 🧪 Testing

34 checks, all passing (`dune test`; the suite is a plain executable that counts its own failures, so it prints nothing when it passes, and `dune exec test/test_mallee.exe` prints `all 34 checks passed`):

- Types (19): literals, identity, `fun f x -> f (f x)`, function composition, constants, arithmetic builtins, `if`, `let`, shorthand definitions, let-polymorphism (an `id` used at `bool` and `int`), and inner `let` generalisation.
- Errors (12): a function argument used at two types, the occurs check on `fun x -> x x`, a non-boolean `if` condition, mismatched branches, wrong argument types, applying an integer, an unbound variable, a lexical error with its column, an unclosed parenthesis, a missing `else`, `fun` with no parameter, and an integer literal that's out of range.
- Definitions (3): a top-level `let` prints a `val` line, the defined name stays polymorphic when used later, and a plain `let x = 1` gives `val x : int`.

## ⚠️ Honest limits

- Substitutions are explicit (Algorithm W) rather than mutable variables with levels. It's easy to check against the textbook but does more copying.
- No `let rec`, so no recursion, and no evaluator. The language is only type-checked.
- Type errors report the types that failed to unify but not source locations.

## Licence

MIT.

