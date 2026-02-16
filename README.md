# pipes.fs

A diff-driven terminal renderer in F# with a pure functional core and minimal effects boundary.

## Architecture

- `src/pipes.fs.Domain`: pure types, defaults, rules, and low-level primitives.
- `src/pipes.fs.Engine`: pure simulation, render, diff, draw op optimization, ANSI encoding, CLI parsing.
- `src/pipes.fs.App`: terminal effects and runtime loop.
- `tests/pipes.fs.Tests`: unit and property tests (Expecto + FsCheck).

Only `src/pipes.fs.App/Program.fs` and `src/pipes.fs.App/Terminal.fs` perform runtime I/O.

## Quick Start

```bash
make run
```

Useful flags:

```bash
dotnet run --project src/pipes.fs.App -- --mode ink --flow swirl --fps 60 --walkers 48 --hud true
```

## Development

```bash
make restore
make build
make test
```

Open folder in VS Code and use tasks: `restore`, `build`, `run`, `test`.

Note: `make test` needs NuGet access to restore `Expecto` and `FsCheck`.
