# `letloop/cli`

<div align=center>
  <img src="https://raw.githubusercontent.com/letloop/letloop-cli/main/letloop-logo-wide.png" />
</div>

A Chez Scheme distribution with all goodies packed in.

## Getting started

`letloop` comes with a handy command line tool to ease your
parenthetical journey. Here is a usage summary:

- `letloop check [--fail-fast] LIBRARY`
- `letloop compile PROGRAM.SCM A.OUT`
- `letloop exec PROGRAM.SCM [ -- ARGUMENT ...]`
- `letloop repl`

All subcommands take into consideration `EXTENSION ...` as part of the
library discovery machinery that rely on `DIRECTORY ...`. If no directory
are provided, the current working directory is the only directory where
libraries will be looked into.

## Change Log

### v7

- Embed scheme libraries into letloop, and elf binaries produced by
  `letloop compile`;
- More robust, more efficient, and less noisy pre-release checks;
- Release debian bookworm, mint vera, and more fedora binaries;
- Add some libraries from SRFI process;
- Add some libraries from R7RS aliasing libraries from SRFI;

## Binary Installation

1. [Download the latest release for your favorite Linux distribution](https://github.com/letloop/cli/releases/latest/)
2. Install system dependencies:

    - alpine:

      ```shell
      apk add build-base git lz4-dev libuuid util-linux-dev zlib-dev
      ```

    - arch:

      ```shell
      pacman -Sy --noconfirm base-devel git lz4 util-linux-libs zlib
      ```

    - fedora:

      ```shell
      yum group install -y "C Development Tools and Libraries" && yum install -y git lz4-devel libuuid-devel zlib-devel
      ```

    - redhat:
      ```shell
      yum group install -y "Development Tools" && yum install -y git lz4-devel libuuid-devel zlib-devel
      ```
    - ubuntu:

      ```shell
      apt install build-essential uuid-dev liblz4-dev zlib1g-dev
      ```

3. Enjoy all around best scheme
4. [Star the project](https://github.com/letloop/cli/stargazers)

## `letloop check [--fail-fast] LIBRARY`

Discover libraries, and execute their checks with code coverage.

A procedure check must be a thunk exported by a library, with a
procedure name that starts with `~check`, such as
`~check-000-earth-is-not-flat`:

```scheme
(library (earth)
  (export ~check-000-earth-is-not-flat)
  (import (chezscheme))

  (define ~check-000-earth-is-not-flat (lambda () (assert #t))))
```

You can check one library with the following invokation:

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

If `PROGRAM.SCM` exit successfully, exit code is zero, an html profile
of the execution will be generated in `/tmp`.

## `letloop compile PROGRAM.SCM`

Compile into a binary the file `PROGRAM.SCM`, and produce an
executable named `a.out`.

You can adjust optimization with the `--optimize-level=n` where `n`
can be `0`, `1`, `2`, or `3`. Higher is harder to debug, unsafe, but
faster.

## `letloop exec PROGRAM.SCM`

Execute `PROGRAM.SCM`; argument to `PROGRAM.SCM` must come avec two
dashes `--` like:

```
letloop exec --dev hello.scm -- --french --date=now Amir
```

The flag `--dev` is an argument of `letloop exec`. Arguments of
`hello.scm` are `--french`, `--date=now` and `Amir`.

When the flag `--dev` is provided `PROGRAM.SCM` will be executed
in "development mode" with the following Chez parameters set:

```scheme
(generate-allocation-counts active?)
(generate-covin-files active?)
(generate-inspector-information active?)
(generate-instruction-counts active?)
(generate-interrupt-trap active?)
(generate-procedure-source-information active?)
(generate-profile-forms active?)
(debug-on-exception active?)
```

Also, if `PROGRAM.SCM` exit successfully, exit code is zero, an html
profile of the execution will be generated in `/tmp`.

You can adjust optimization with the `--optimize-level=n` where `n`
can be `0`, `1`, `2`, or `3`. Higher is harder to debug, unsafe, but
faster.

## `letloop repl`

Basic Read-Eval-Print-Loop (REPL). The use of a tool similar to
`rlwrap` may be necessary.
