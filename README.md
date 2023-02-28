# letloop, ze cli

<div align=center>
  <img src="https://avatars.githubusercontent.com/u/126236863" />
</div>

Opiniated Chez Scheme command line tool, with all the goodies packed in one binary.

```sh
Usage:

  letloop check [--dev] [--fail-fast] [EXTENSION ...] DIRECTORY ... [ -- LIBRARY-OR-PROCEDURE ...]
  letloop compile [--dev] [--optimize-level=0-3] [EXTENSION ...] [DIRECTORY ...] PROGRAM.SCM A.OUT
  letloop exec [--dev] [EXTENSION ...] [DIRECTORY ...] PROGRAM.SCM [ -- ARGUMENT ...]
  letloop repl [--dev] [EXTENSION ...] [DIRECTORY ...]
```

## `letloop check`

Execute checks from the libraries founds in `DIRECTORY`. Extensions
that must be taken into account can be passed as argument. The default
is to look into `.chezscheme.sls` and `.sls`.

Checks must be thunks exported by libraries, the procedure name must
start with `~check`, such as `~check-srfi-167-000`:

```scheme
(library (srfi-167)
  (export ~check-srfi-167-000)
  (import (chezscheme))

  (define ~check-srfi-167-000 (lambda () (assert #t))))
```

## `letloop compile`

Compile into a binary the file `PROGRAM.SCM`, and produce an
executable named `A.OUT`.

## `letloop exec PROGRAM.SCM`

Execute `PROGRAM.SCM` pass any extra argument to `PROGRAM.SCM`. Extra
arguments comes after two dashes, such as:

```
letloop exec hello.scm -- --french --date=now Amir
```

Both `--french`, `--date=now` and `Amir` are an arguments of `hello.scm`.

## `letloop repl`

Basic Read-Eval-Print-Loop. The use of `rlwrap` or similar tool may be
necessary.