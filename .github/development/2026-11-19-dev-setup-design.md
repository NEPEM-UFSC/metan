# Development Setup and Repository Hygiene — Design

**Date:** 2026-11-19
**Status:** Approved — 2026-11-19
**Owner:** Matheus L Machado
**Scope:** Development environment and repository process only. No changes to package code, statistical methods, or exported API.

---

## 1. Intent

Make `metan` predictable to develop locally and safe to change:

- a reproducible R toolchain that matches CI;
- a documented day-to-day workflow (load, test, document, check, PR);
- ignore/build rules that match the repository's real publishing model;
- a pkgdown configuration that is actually applied;
- CI that fails when the package is broken, instead of reporting informationally.

The package is mature (CRAN, ~286 exports, 114 files under `R/`) and already has a testthat suite and a GitHub Actions workflow. This work closes the remaining setup gaps rather than creating a project from scratch.

---

## 2. Current state (evidence)

| Item | Finding |
|---|---|
| Git | `master` clean and synced with `origin/master`; only `master` local. |
| Stale branches | `origin/feat` and `origin/fix/make-mat-across-deprecation` are fully merged into `master` (PR #10). |
| Tags | Latest tag is `v1.19.0`. Current `DESCRIPTION` version `1.20.0` is development, not a release. |
| Tests | `tests/testthat/` with 8 test files and 4 snapshots. |
| CI | `.github/workflows/R-CMD-check.yaml`, matrix R 4.5/4.6, `continue-on-error: true`; not required. |
| Branch protection | None on `master`. |
| Pages | GitHub Pages legacy source: branch `master`, path `/docs`. |
| Ignore inconsistency | `docs/` is tracked (1,167 files, 138 MB) but listed in `.gitignore`. |
| pkgdown | Config lives at `pkgdown/_pkgdown.yml`; pkgdown looks for `_pkgdown.yml` in the package root, so the config is not applied. It also contains `navbar.title: "sparklyr"` and a dead Universal Analytics ID. |
| Stale docs | `QUALITY_ASSESSMENT.md` says there are no tests and no CI (no longer true) and is not in `.Rbuildignore` (would enter the build/check). |
| Stale contributing | `.github/CONTRIBUTING.md` references Travis and AppVeyor. |
| Git identity | `user.name` and `user.email` are not configured. |
| Local R | R 4.3.3; gcc/g++/gfortran/make/pandoc present; 21 R packages missing, including all main `Imports` and dev tools. |
| GitHub access | `gh` authenticated as `matheus-lopesm`, `ADMIN` on `NEPEM-UFSC/metan`. |

---

## 3. Goals

1. **G1 — Environment:** install R 4.6 in user mode via `rig` (no sudo, does not touch system R 4.3.3), configure Posit Package Manager binaries, install `pak`, then install all package development dependencies.
2. **G2 — Documentation:** add `DEVELOPMENT.md` (setup + workflow + release checklist), `AGENTS.md` (repository rules for AI agents), and update `.github/CONTRIBUTING.md` to the real GitHub Actions workflow.
3. **G3 — Ignore/build hygiene:** make `.gitignore` and `.Rbuildignore` consistent with the real publishing model; keep `docs/` tracked and remove it from `.gitignore`; ignore local revdep artifacts.
4. **G4 — pkgdown:** move the config to the package root, fix the site title, remove dead analytics, and validate with `pkgdown::check_pkgdown()`.
5. **G5 — CI and branches:** remove `continue-on-error`, add R 4.3 (declared minimum) to the matrix, delete merged remote branches, and enable branch protection after the first green run.
6. **G6 — Verification:** `devtools::load_all()`, `devtools::test()`, `devtools::document()` (no diff), and `devtools::check()` all pass locally.

---

## 4. Non-goals

- No changes to package code, statistical behavior, exported functions, or tests beyond the existing suite.
- No rebuild/commit of the 138 MB `docs/` site in this change; that is a separate, reviewable step.
- No `renv` lockfile.
- No CRAN release or `v1.20.0` tag.
- No Windows/macOS CI matrix.
- No lint enforcement in CI yet.

## 5. Anti-goals

- Breaking or replacing the system R 4.3.3 installation.
- Rewriting git history or removing `docs/` from tracking while Pages still serves it.
- Making CI required before it has passed at least once.
- Committing generated noise or hand-editing `man/*.Rd`.

---

## 6. Approaches considered

| Approach | Contents | Trade-off |
|---|---|---|
| **A. Full setup (chosen)** | Environment, docs, tooling, ignore/build rules, pkgdown config, CI hardening, branch protection. | Touches many files and GitHub settings, but removes all known blockers in one reviewable PR. |
| B. Minimum to code | Install dependencies, configure git identity, fix ignore files, short guide. | Fast, but leaves broken CI/Pages/pkgdown config for later. |
| C. Plan only | A document with commands and recommendations. | No risk, but does not unblock development. |

A was chosen because the user selected the full scope and has admin access to finish the GitHub-side changes.

---

## 7. Decisions

- **D1 — R 4.6 via `rig` user mode.** Install `rig` into `~/.local`, run `rig system user-mode`, then `rig add release`. This avoids sudo and leaves `/usr/bin/R` 4.3.3 untouched. `rig` configures P3M and installs `pak`.
- **D2 — No `renv`.** Reproducibility comes from `DESCRIPTION`, P3M binaries, and `tools/setup.R` (idempotent). A lockfile can be added later if the team wants exact pinning.
- **D3 — `docs/` stays tracked.** GitHub Pages serves `master`/`docs`; removing it from tracking would break the site. Remove the `docs/` entry from `.gitignore` and document the policy.
- **D4 — pkgdown config at root.** Move `pkgdown/_pkgdown.yml` to `_pkgdown.yml`, change `navbar.title` from `"sparklyr"` to `"metan"`, remove the dead `ganalytics` ID, keep Bootstrap 5. Validate with `pkgdown::check_pkgdown()`; do not rebuild the site in this PR.
- **D5 — `QUALITY_ASSESSMENT.md` moves to `.github/`.** Preserve it as a dated historical assessment with a status note; `.github/` is already excluded from the build.
- **D6 — CI hardening.** Remove `continue-on-error`, rename the workflow from `R-CMD-check (informational)` to `R-CMD-check`, add R 4.3 to the matrix (4.3/4.5/4.6), keep Ubuntu, and update the header comment. Enable branch protection only after the PR check is green.
- **D7 — Branch and PR.** Work on `chore/dev-setup`, open a PR against `master`, no direct commits to `master`.
- **D8 — Git identity.** `Matheus L Machado <matheuslmachado03@gmail.com>` (from repository history); configurable at commit time.
- **D9 — Language.** Repository documentation in English (project convention); conversation in Portuguese.
- **D10 — Lint config, not lint CI.** Add `.lintr` with noisy linters disabled so local linting is useful; do not add a failing lint job yet.
- **D11 — Delete merged remote branches.** `feat` and `fix/make-mat-across-deprecation` are fully contained in `master`.
- **D12 — `Makefile` for common tasks.** `make setup`, `make document`, `make test`, `make check`, `make site`; add `Makefile` to `.Rbuildignore`.

---

## 8. Files to create or modify

### Create

- `.github/development/2026-11-19-dev-setup-design.md` (this document)
- `DEVELOPMENT.md`
- `AGENTS.md`
- `tools/setup.R`
- `tools/check-env.R`
- `.editorconfig`
- `.lintr`
- `.github/pull_request_template.md`
- `.github/dependabot.yml`
- `Makefile`
- `_pkgdown.yml` (moved from `pkgdown/_pkgdown.yml`)

### Modify

- `.gitignore` — remove `docs/`; add `revdep/library/`, `revdep/checks/`, `revdep/checks.noindex/`, `revdep/data.sqlite`, `revdep/email.yml`, `pkgdown-site/`.
- `.Rbuildignore` — add `QUALITY_ASSESSMENT.md`, `AGENTS.md`, `DEVELOPMENT.md`, `Makefile`, `tools`, `.editorconfig`, `.lintr`, `.github/development`.
- `.github/workflows/R-CMD-check.yaml` — remove `continue-on-error`; rename workflow to `R-CMD-check`; matrix `4.3`, `4.5`, `4.6`; keep `upload-snapshots: true`; update the header comment.
- `.github/CONTRIBUTING.md` — replace Travis/AppVeyor with GitHub Actions; add `devtools` commands and branch/NEWS expectations.
- `README.Rmd` and `README.md` — add the `R-CMD-check` badge once the workflow is required (same PR).
- `QUALITY_ASSESSMENT.md` → `.github/QUALITY_ASSESSMENT.md` — add a status note at the top.

### Delete

- `pkgdown/_pkgdown.yml` (after moving it).
- Remote branches `feat` and `fix/make-mat-across-deprecation`.

---

## 9. Verification plan

1. `R-4.6 -e 'pak::local_install_dev_deps()'` completes.
2. `R-4.6 -e 'devtools::load_all()'` loads without errors.
3. `R-4.6 -e 'devtools::test()'` passes.
4. `R-4.6 -e 'devtools::document()'` produces no `man/` or `NAMESPACE` diff.
5. `R-4.6 -e 'devtools::check()'` yields 0 errors and 0 warnings (notes acceptable).
6. `R-4.6 -e 'pkgdown::check_pkgdown()'` validates the moved config.
7. PR CI is green on all matrix jobs.
8. Branch protection on `master` requires the R-CMD-check jobs and blocks force pushes.

---

## 10. Risks and rollback

| Risk | Mitigation |
|---|---|
| R 4.6 user install fails on WSL/Ubuntu 24.04 | Fall back to R 4.4 from CRAN apt, or keep system R 4.3.3 and install compatible packages. `rig` user mode is isolated; removing it does not affect `/usr/bin/R`. |
| Latest CRAN packages need newer system libraries | Prefer P3M binaries (runtime libraries are already present); if a source build is required, provide the exact `apt` command for the user to run. |
| Moving pkgdown config changes the next site build | Validate with `check_pkgdown()`; rebuild the site separately and review the diff. |
| Branch protection blocks urgent fixes | Admin can bypass or temporarily disable protection; document this in `DEVELOPMENT.md`. |
| Committing a huge `docs/` diff | Do not rebuild the site in this PR. |

---

## 11. Open items for the owner

1. Confirm the git identity `Matheus L Machado <matheuslmachado03@gmail.com>`.
2. Confirm that `docs/` must stay tracked (we assume yes, because Pages serves `master`/`docs`).
3. Confirm that deferring the pkgdown site rebuild is acceptable.
