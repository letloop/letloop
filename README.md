# letloop

<div align=center>
  <img src="https://raw.githubusercontent.com/letloop/letloop/main/letloop-wave-wide.png" />
</div>

A Chez Scheme distribution with many goodies packed in.

## Usage

```
Usage:

  letloop benchmark LIBRARY [THUNK [N]]
  letloop check [--fail-fast] LIBRARY ...
  letloop compile PROGRAM.SCM
  letloop exec PROGRAM.SCM [ -- ARGUMENT ...]
  letloop literally LIBRARY.MD
  letloop repl

All subcommands can use directories, and extension of libraries as
standalone arguments.

The following flags are available:

  --dev Generate allocation, and instruction counts, debug on
        exception, and dump profile information.

  --optimize-level=0-3 Configure optimization level, higher is less
                       safe, but faster
```

## Change Log

### v8

- Focus on Debian, and its derivative Ubuntu. Feel free to dare add
  support, and maintain the support for your favorite distribution.

#### Libraries

- Improve `(letloop www)`: add uri, host, and query readers
- Improve `(letlop html)`: no need for an accumulator

- Add support nix
- Add https://letloop.github.io/letloop/
- Add zero-knowledge password verification;
- Add blake3 bindings
- Add argon2 bindings
- Add termbox bindings
- Add lsm1 bindings
- Add sqlite3 bindings
- Add commonmark bindings

### v7

#### Primary

- Embed scheme libraries into letloop, and elf binaries produced by
  `letloop compile`;
- More robust, more efficient, and less noisy pre-release checks;

#### Secondary

- Release debian bookworm, mint vera, and more fedora binaries;
- Add some libraries from SRFI process;
- Add some libraries from R7RS aliasing libraries from SRFI;
- Create files in inside `/tmp/letloop/`, and cleanup whenever possible;
- Add a subcommand to execute benchmarks: `letloop benchmark
  LIBRARY.SCM THUNK [N]`;
