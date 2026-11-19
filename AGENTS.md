# AGENTS.md

Repository rules for AI coding agents working on `metan`.

## Project

`metan` is a CRAN R package for multi-environment trial analysis. The
development branch is `master`; `DESCRIPTION` version `1.20.0` is a
development version. Package code lives in `R/`, generated documentation in
`man/`, and tests in `tests/testthat/`.

## Environment

Use the rig-managed R 4.6 from `~/.local/bin`, not the system R:

```bash
which R          # /home/<user>/.local/bin/R
R --version      # R 4.6.x
Rscript tools/check-env.R
```

The repository has a local, git-ignored `.Renviron` that sets
`R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.6`. If it is missing, create it
or run `Rscript tools/setup.R`.

## Commands

```bash
Rscript -e 'devtools::load_all()'   # load source
Rscript -e 'devtools::test()'       # run tests
Rscript -e 'devtools::document()'   # regenerate man/ and NAMESPACE
Rscript -e 'devtools::check()'      # full R CMD check
Rscript -e 'pkgdown::check_pkgdown()'
```

`make help` lists the same commands.

## Rules

- Never edit `man/*.Rd` or `NAMESPACE` by hand. Edit roxygen comments in `R/`
  and run `devtools::document()`.
- Keep `roxygen2` at 7.3.3, the version recorded in `DESCRIPTION`. Do not
  upgrade roxygen2 and regenerate the docs in the same PR as a code change.
- Add or update a test for every behavior change. Run the full test suite, not
  only the new test file.
- Update `NEWS.md` for user-facing changes.
- Do not change statistical methods, exported APIs, or defaults without a
  reproducible example and a regression test.
- Keep `docs/` tracked. GitHub Pages publishes `master`/`docs`; do not delete
  the directory and do not rebuild the site in a feature PR.
- `_pkgdown.yml` must stay at the repository root where pkgdown reads it.
- Follow the tidyverse style: snake_case, two-space indentation, one function
  per file where the existing layout allows it.
- Work on a `feat/...`, `fix/...`, or `chore/...` branch and open a PR to
  `master`. Do not push directly to `master`.
- Before claiming work is complete, run `devtools::test()` and
  `devtools::check()` and report their output.

## Known issue

`ggplot2` 4.0.0 deprecates `fortify(<lm>)`, used in `R/gamem_met.R`. The test
suite passes with a warning. Replace it with `broom::augment()` in a separate
PR with a regression test.
