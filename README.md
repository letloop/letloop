# letloop

<div align=center>
  <img src="https://raw.githubusercontent.com/letloop/letloop/main/letloop-wave-wide.png" />
</div>

A Chez Scheme distribution with many goodies packed in.

## Available Libraries

<div align=center>
· `(letloop argon2)` · `(letloop assembly)` · `(letloop blake3)` · `(letloop byter)` · `(letloop byter)` · `(letloop bytevector)` · `(letloop cairo)` · `(letloop commonmark)` · `(letloop dxdb eavt)` · `(letloop dxdb nstore)` · `(letloop dxdb)` · `(letloop epoll)` · `(letloop flow)` · `(letloop gamma)` · `(letloop hook)` · `(letloop html)` · `(letloop http)` · `(letloop json)` · `(letloop lsm1)` · `(letloop match)` · `(letloop okvs)` · `(letloop r999)` · `(letloop seed)` · `(letloop sqlite3)` · `(letloop sxpath)` · `(letloop termbox2)` · `(letloop unknown)` · `(letloop www)` · `(scheme base)` · `(scheme bitwise)` · `(scheme box)` · `(scheme bytevector)` · `(scheme case-lambda)` · `(scheme char)` · `(scheme charset)` · `(scheme comparator)` · `(scheme complex)` · `(scheme cxr)` · `(scheme division)` · `(scheme eval)` · `(scheme file)` · `(scheme fixnum)` · `(scheme generator)` · `(scheme hash-table)` · `(scheme ideque)` · `(scheme ilist)` · `(scheme inexact)` · `(scheme lazy)` · `(scheme list-queue)` · `(scheme load)` · `(scheme lseq)` · `(scheme mapping hash)` · `(scheme mapping)` · `(scheme process-context)` · `(scheme r5rs)` · `(scheme read)` · `(scheme repl)` · `(scheme rlist)` · `(scheme set)` · `(scheme stream)` · `(scheme time)` · `(scheme write)` · `(srfi srfi-1)` · `(srfi srfi-1)` · `(srfi srfi-2)` · `(srfi srfi-4)` · `(srfi srfi-5)` · `(srfi srfi-6)` · `(srfi srfi-8)` · `(srfi srfi-9)` · `(srfi srfi-11)` · `(srfi srfi-13)` · `(srfi srfi-14)` · `(srfi srfi-16)` · `(srfi srfi-17)` · `(srfi srfi-19)` · `(srfi srfi-23)` · `(srfi srfi-26)` · `(srfi srfi-28)` · `(srfi srfi-29)` · `(srfi srfi-31)` · `(srfi srfi-34)` · `(srfi srfi-35)` · `(srfi srfi-37)` · `(srfi srfi-37)` · `(srfi srfi-39)` · `(srfi srfi-41)` · `(srfi srfi-42)` · `(srfi srfi-43)` · `(srfi srfi-45)` · `(srfi srfi-48)` · `(srfi srfi-51)` · `(srfi srfi-60)` · `(srfi srfi-61)` · `(srfi srfi-67)` · `(srfi srfi-69)` · `(srfi srfi-98)` · `(srfi srfi-99)` · `(srfi srfi-101)` · `(srfi srfi-111)` · `(srfi srfi-113)` · `(srfi srfi-115)` · `(srfi srfi-116)` · `(srfi srfi-117)` · `(srfi srfi-125)` · `(srfi srfi-127)` · `(srfi srfi-128)` · `(srfi srfi-134)` · `(srfi srfi-141)` · `(srfi srfi-143)` · `(srfi srfi-145)` · `(srfi srfi-146)` · `(srfi srfi-146)` · `(srfi srfi-151)` · `(srfi srfi-158)` · `(srfi srfi-167)` · `(srfi srfi-173)` · `(srfi srfi-224)` · </div>

## Usage

```
Usage:

  letloop benchmark LIBRARY [THUNK [N]]
  letloop check [--fail-fast] LIBRARY ...
  letloop compile PROGRAM.SCM
  letloop exec PROGRAM.SCM [ -- ARGUMENT ...]
  letloop literally LIBRARY.MD
  letloop repl

The following flags are available:

  --dev Generate allocation, and instruction counts, debug on
        exception, and dump profile information.

  --optimize-level=0-3 Configure optimization level, higher is less
                       safe, but faster
```

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
4. [Star the project](https://github.com/letloop/letloop/stargazers)

## `letloop benchmark LIBRARY.SCM THUNK [N]`

Display average nanoseconds spent running `THUNK` from
`LIBRARY.SCM`. Spent time is averaged over `N` times. The default
value of `N` is 10.

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

## `letloop literally LIBRARY.MD`

Generate a Scheme library from `LIBRARY.MD`.

## `letloop repl`

Basic Read-Eval-Print-Loop (REPL). The use of a readline tool similar
to [`rlwrap`](https://pkgs.org/search/?q=rlwrap&on=name) may be
necessary.
