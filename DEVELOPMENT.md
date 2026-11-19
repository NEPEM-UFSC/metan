# Development guide

This document is for people working on the `metan` source code. It covers the
local toolchain, the daily commands, the branch/PR flow, and the release
checklist. User-facing documentation lives in the vignettes and the pkgdown
site.

## 1. What this repository is

`metan` is an R package for multi-environment trial analysis. The package is
published on CRAN; `master` is the development branch. The current
`DESCRIPTION` version (`1.20.0`) is a development version until it is submitted
to CRAN and tagged.

Key directories:

| Path | Purpose |
|---|---|
| `R/` | Package source. Roxygen comments live here. |
| `man/` | Generated Rd files. Do not edit by hand. |
| `tests/testthat/` | Regression tests and snapshots. |
| `vignettes/` | Long-form user documentation. |
| `data/`, `inst/` | Bundled datasets and installed files. |
| `docs/` | Rendered pkgdown site. Tracked because GitHub Pages serves `master`/`docs`. |
| `_pkgdown.yml` | pkgdown configuration (must stay at the repository root). |
| `tools/` | Development scripts (not part of the package build). |

## 2. Prerequisites

- Ubuntu 24.04 (or WSL) with `gcc`, `g++`, `gfortran`, `make`, `git`, and
  `pandoc`.
- R 4.6 installed in user mode with
  [rig](https://r-lib.github.io/rig/). This keeps the system R (if any)
  untouched.
- `~/.local/bin` on `PATH` (the rig installer adds it).

## 3. First-time setup

Install `rig` and R 4.6 in user mode:

```bash
curl -LsSf https://r-lib.github.io/rig/install.sh | sh
rig system user-mode
rig add release
```

Verify that the shell now uses the rig-managed R:

```bash
which R          # expected: /home/<user>/.local/bin/R
R --version      # expected: R 4.6.x
```

The global `~/.Renviron` may point `R_LIBS_USER` at another R version's
library. This project therefore uses a local, git-ignored `.Renviron`:

```bash
printf 'R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.6\n' > .Renviron
```

Install the package dependencies and the development tools:

```bash
Rscript tools/setup.R
Rscript tools/check-env.R
```

`tools/setup.R` is idempotent. It installs the `DESCRIPTION` dependencies, the
documented development tools, and pins `roxygen2` to the version recorded in
`DESCRIPTION` (`7.3.3`). If a package needs to be compiled from source and a
system header is missing, install the corresponding `-dev` package with `apt`
and rerun the script. Do not modify the system R installation.

Because the project `.Renviron` sets `R_LIBS_USER` for R 4.6, running the
system `/usr/bin/R` (4.3.3) from inside this repository will also see the 4.6
library. Use the rig-managed R 4.6 inside this repository, or run the system R
from another directory.

## 4. Daily workflow

Load the package without installing it:

```bash
Rscript -e 'devtools::load_all()'
```

Run the test suite:

```bash
Rscript -e 'devtools::test()'
```

Regenerate `man/` and `NAMESPACE` after changing roxygen comments:

```bash
Rscript -e 'devtools::document()'
```

`devtools::document()` must leave the working tree clean. If it produces a
diff, inspect it. A clean tree is the expected state with the pinned
`roxygen2` 7.3.3; do not commit generated churn blindly.

Run `R CMD check` before opening a PR:

```bash
Rscript -e 'devtools::check()'
```

Shortcuts are available through `make`:

```bash
make help
make test
make check
```

## 5. Branch and pull request flow

1. Start from an up-to-date `master`:

   ```bash
   git switch master
   git pull --ff-only
   git switch -c feat/short-description
   ```

   Use `feat/...`, `fix/...`, or `chore/...` as the prefix.

2. Make a focused change. Add or update tests for behavior changes. Update
   `NEWS.md` for user-facing changes.

3. Run `devtools::test()` and `devtools::check()` before pushing.

4. Open a pull request against `master`. The `R-CMD-check` workflow runs on
   R 4.3, 4.5, and 4.6. The PR template lists the expected checks.

5. Merge only after the workflow is green. `master` is protected: force
   pushes and branch deletion are disabled, and the `R-CMD-check` jobs are
   required status checks.

Administrators can bypass protection in an emergency, but the bypass should be
documented in the PR description.

## 6. Documentation rules

- Edit roxygen comments in `R/`. Never edit `man/*.Rd` or `NAMESPACE` by hand.
- `README.Rmd` is the source of `README.md`. Knit the Rmd file; do not edit the
  generated Markdown alone.
- pkgdown configuration lives in `_pkgdown.yml` at the repository root.
  `pkgdown::check_pkgdown()` validates it.
- `docs/` is tracked because GitHub Pages publishes `master`/`docs`. Do not
  commit a full site rebuild in a feature PR; rebuild the site separately and
  review the diff.

## 7. Release checklist

1. Bump `Version` in `DESCRIPTION` and add a `NEWS.md` entry.
2. Run `R CMD check --as-cran` and resolve errors, warnings, and actionable
   notes.
3. Run reverse-dependency checks (`revdepcheck`) and update
   `cran-comments.md`.
4. Submit to CRAN.
5. After acceptance, create the release tag (`v<version>`) and publish the
   GitHub release. Only tag versions that are actually released.

## 8. Troubleshooting

| Symptom | Cause and fix |
|---|---|
| `R` is still 4.3.3 | The rig `~/.local/bin` directory is not first on `PATH`. Open a new shell or prepend it: `export PATH="$HOME/.local/bin:$PATH"`. |
| `there is no package called 'pak'` | The project `.Renviron` is missing or points to the wrong library. Recreate it and rerun `Rscript tools/setup.R`. |
| A source package asks for `curl/curl.h`, `libxml2`, etc. | Install the matching `-dev` package (`libcurl4-openssl-dev`, `libxml2-dev`, ...) with `apt`, then rerun the setup script. |
| `devtools::document()` produces a large diff | Check `packageVersion("roxygen2")`. It must be `7.3.3`; run `Rscript tools/setup.R` to restore the pin. |
| `R CMD check` reports a note about `.superpowers` | The local agent workspace is ignored by `.Rbuildignore`; if it still appears, remove the directory before building. |

## 9. Known issue

`ggplot2` 4.0.0 deprecates `fortify(<lm>)`, which is used in
`R/gamem_met.R`. The test suite still passes with a warning. Fixing it requires
replacing the call with `broom::augment()` and adding a regression test; that
belongs in a separate PR.
