# fortnb

`fortnb` is an experimental notebook/literate-programming tool for Fortran.

Current status: on hold. The repository contains notebook parsing, execution,
rendering, and figure-capture experiments, but it is not currently a dependable
notebook runtime.

## Intended Role

`fortnb` should provide Jupytext-style Fortran notebooks that can render to
Markdown/PDF and execute code through the Lazy Fortran toolchain.

Long-term flow:

1. parse notebook-style `.f`/`.lf` files
2. execute code cells through `fortrun` or `ffc`
3. capture stdout/stderr and figures
4. render reproducible Markdown/PDF output

## Current Limits

- execution depends on toolchain pieces that are still experimental
- figure capture currently includes placeholder behavior
- Lazy Fortran execution should wait for the FortFront/`ffc` compiler path to
  stabilize

## Build

```bash
fpm build
```

## Roadmap

Open issue:

- [#1](https://github.com/lazy-fortran/fortnb/issues/1): Jupytext-like
  features for `.f` files with Markdown output

Near-term work should focus on format parsing/rendering. Compiler/runtime
integration should wait for `ffc`.

## License

MIT. See `LICENSE`.
