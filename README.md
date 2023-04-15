# letloop cli

<div align=center>
  <img src="https://raw.githubusercontent.com/letloop/letloop-cli/main/letloop-logo-wide.png" />
</div>

A neat Chez Scheme command line tool, with all the goodies packed in.

Usage:

- `letloop check [--fail-fast] LIBRARY`
- `letloop compile PROGRAM.SCM A.OUT`
- `letloop exec PROGRAM.SCM [ -- ARGUMENT ...]`
- `letloop repl`

All subcommands take into consideration `EXTENSION ...` as part of the 
library discovery machinery that rely on `DIRECTORY ...`. If no directory
are provided, the current working directory is the only directory where
libraries will be looked into.

## `letloop check [--fail-fast] LIBRARY`

Discover libraries, and execute their checks.

A procedure check must be a thunk exported by a library, with a
procedure name that starts with `~check`, such as
`~check-earth-is-not-flat-000`:

```scheme
(library (earth)
  (export ~check-earth-is-not-flat-000)
  (import (chezscheme))

  (define ~check-earth-is-not-flat-000 (lambda () (assert #t))))
```

You can check a single library with the following syntax:

```sh
#;sh> letloop check $(pwd) srfi-167.scm
```

You can add directories to the library path as argument, e.g. in the
above line the current directory was added with the shell device
`$(pwd)`.

If no library is provided as command line argument, `loop check` will
fallback to discovery based on directories, and will look for all
libraries, for all available checks, and execute them.

Use the flag `--fail-fast` to exit as soon as there is a failure.

## `letloop compile PROGRAM.SCM`

Compile into a binary the file `PROGRAM.SCM`, and produce an
executable named `a.out`.

## `letloop exec PROGRAM.SCM`

Execute `PROGRAM.SCM` pass any extra argument to `PROGRAM.SCM`. Extra
arguments comes after two dashes, such as:

```
letloop exec hello.scm -- --french --date=now Amir
```

Both `--french`, `--date=now` and `Amir` are an arguments of
`hello.scm`.

## `letloop repl`

Basic Read-Eval-Print-Loop (REPL). The use of a tool similar to
`rlwrap` may be necessary.
