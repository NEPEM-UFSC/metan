# Development Setup and Repository Hygiene — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `metan` ready for safe local development: R 4.6 toolchain, installed dev dependencies, documented workflow, consistent ignore/build rules, working pkgdown config, and CI that blocks regressions.

**Architecture:** Use `rig` in user mode to install R 4.6 under `~/.local` without touching the system R 4.3.3. Install package dependencies with `pak` from Posit Package Manager binaries. Apply repository hygiene and documentation changes on a single `chore/dev-setup` branch, then open a PR and enable branch protection only after CI is green.

**Tech Stack:** R 4.6, rig, pak, testthat, roxygen2, devtools, pkgdown, GitHub Actions, `gh` CLI.

**Spec:** `.github/development/2026-11-19-dev-setup-design.md`

## Global Constraints

- No changes to package code, statistical behavior, exported functions, or the existing test suite.
- Do not rebuild or commit the 138 MB `docs/` site in this PR.
- Do not create a `renv` lockfile.
- Do not tag `v1.20.0`; it is a development version.
- Keep system R 4.3.3 untouched; install R 4.6 in user mode only.
- Repository docs in English; conversation in Portuguese.
- Work only on branch `chore/dev-setup`; open a PR to `master`.
- Commit identity: `Matheus L Machado <matheuslmachado03@gmail.com>`.
- Do not make CI required until it has passed at least once.

## Review Focus

- **R 4.6 user install does not shadow system R unexpectedly.** `which R` must resolve to the rig-managed binary after installation; `/usr/bin/R` must remain 4.3.3.
- **`docs/` stays tracked.** GitHub Pages serves `master`/`docs`; removing it from tracking would break the site.
- **pkgdown config is actually read.** `pkgdown::check_pkgdown()` must accept the root `_pkgdown.yml` and the site title must be `metan`, not `sparklyr`.
- **CI checks are meaningful.** Removing `continue-on-error` must not be done before the local test/check baseline is green.
- **No generated noise.** `devtools::document()` must produce no diff; a diff means the roxygen version or annotations need attention, not a blind commit.
- **Branch protection does not lock the maintainer out.** Admin bypass policy must be documented if `enforce_admins` is true.

---

## Task 0: Create the working branch and commit the design artifacts

**Files:**
- Create: `.github/development/2026-11-19-dev-setup-design.md` (already written)
- Create: `.github/development/2026-11-19-dev-setup-plan.md` (this file)

**Interfaces:**
- Consumes: approved design doc.
- Produces: branch `chore/dev-setup` with the design and plan committed.

- [ ] **Step 1: Configure git identity**

```bash
git config user.name "Matheus L Machado"
git config user.email "matheuslmachado03@gmail.com"
git config user.name
git config user.email
```

Expected: the two values print back.

- [ ] **Step 2: Create the branch**

```bash
git switch -c chore/dev-setup
git status --short --branch
```

Expected: `## chore/dev-setup` and the two untracked design files.

- [ ] **Step 3: Commit the design and plan**

```bash
git add .github/development/2026-11-19-dev-setup-design.md .github/development/2026-11-19-dev-setup-plan.md
git commit -m "docs: add development setup design and plan"
```

Expected: one commit on `chore/dev-setup`.

---

## Task 1: Install R 4.6 in user mode with rig

**Files:** none in the repository.

**Interfaces:**
- Consumes: system R 4.3.3, `~/.local/bin` already on `PATH`.
- Produces: `R-4.6` (and default `R` in the current shell) available under `~/.local`; `pak` installed.

- [ ] **Step 1: Record the current R state**

```bash
which R
R --version | head -1
```

Expected: `/usr/bin/R`, R 4.3.3.

- [ ] **Step 2: Install rig in user mode**

```bash
curl -LsSf https://r-lib.github.io/rig/install.sh | sh
hash -r
which rig
rig --version
```

Expected: `rig` resolves under `~/.local/bin` and prints a version.

- [ ] **Step 3: Enable user mode and install the current R release**

```bash
rig system user-mode
rig add release
rig list
```

Expected: R 4.6.x listed as installed and as the default.

- [ ] **Step 4: Verify R 4.6 and pak**

```bash
which R
R --version | head -1
R -e 'cat(as.character(packageVersion("pak")), "\n")'
```

Expected: R resolves to the rig-managed 4.6.x; `pak` prints a version. If the current shell still points to `/usr/bin/R`, use `R-4.6` for the remaining commands and note it in `DEVELOPMENT.md`.

- [ ] **Step 5: Confirm system R is untouched**

```bash
/usr/bin/R --version | head -1
```

Expected: R 4.3.3.

- [ ] **Step 6: Commit**

No repository files changed. Skip the commit.

---

## Task 2: Install development dependencies and add the setup script

**Files:**
- Create: `tools/setup.R`
- Create: `tools/check-env.R`

**Interfaces:**
- Consumes: R 4.6 with `pak`.
- Produces: `tools/setup.R` (idempotent dependency installer) and `tools/check-env.R` (environment report).

- [ ] **Step 1: Write `tools/setup.R`**

```r
#!/usr/bin/env Rscript

# Install all development dependencies declared in DESCRIPTION.
# Uses Posit Package Manager binaries when available.

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}

pak::local_install_dev_deps(path = ".", ask = FALSE)
pak::local_check_deps(path = ".")
```

- [ ] **Step 2: Write `tools/check-env.R`**

```r
#!/usr/bin/env Rscript

cat("R:", R.version.string, "\n")
cat("R home:", R.home(), "\n")
cat("libPaths:\n")
print(.libPaths())

needed <- c(
  "cli", "dplyr", "GGally", "ggforce", "ggplot2", "ggrepel", "lme4",
  "lmerTest", "patchwork", "purrr", "rlang", "tibble", "tidyr",
  "tidyselect", "testthat", "roxygen2", "knitr", "rmarkdown",
  "devtools", "pkgdown", "rcmdcheck", "styler", "lintr", "pak"
)

installed <- rownames(installed.packages())
missing <- setdiff(needed, installed)
if (length(missing) == 0) {
  cat("All development packages are installed.\n")
} else {
  cat("Missing packages:\n")
  print(missing)
  quit(status = 1)
}
```

- [ ] **Step 3: Run the setup script**

```bash
Rscript tools/setup.R
```

Expected: dependencies install without errors. If a source build asks for a missing system header, stop and report the exact package and `apt` command instead of changing the system.

- [ ] **Step 4: Verify the environment**

```bash
Rscript tools/check-env.R
```

Expected: `All development packages are installed.`

- [ ] **Step 5: Commit**

```bash
git add tools/setup.R tools/check-env.R
git commit -m "chore: add development dependency setup scripts"
```

---

## Task 3: Establish the baseline (load, test, document, check)

**Files:** none expected; this task records the baseline.

**Interfaces:**
- Consumes: installed dependencies.
- Produces: a green local baseline that justifies removing `continue-on-error`.

- [ ] **Step 1: Load the package**

```bash
Rscript -e 'devtools::load_all()'
```

Expected: no errors.

- [ ] **Step 2: Run the test suite**

```bash
Rscript -e 'devtools::test()'
```

Expected: all tests pass. If any fail, stop and report the failures; do not make CI required.

- [ ] **Step 3: Check documentation is in sync**

```bash
Rscript -e 'devtools::document()'
git status --short
```

Expected: no changes under `R/`, `man/`, or `NAMESPACE`. If roxygen regenerates files, inspect the diff and report before committing; do not commit generated churn blindly.

- [ ] **Step 4: Run R CMD check**

```bash
Rscript -e 'devtools::check(args = c("--no-manual", "--no-build-vignettes"))'
```

Allow up to 30 minutes. Expected: 0 errors, 0 warnings. Notes are acceptable but must be reported.

- [ ] **Step 5: Commit**

No repository files changed. Skip the commit unless Step 3 produced an intentional fix, in which case commit it separately.

---

## Task 4: Repository hygiene (ignore, build, quality doc, pkgdown, contributing)

**Files:**
- Modify: `.gitignore`
- Modify: `.Rbuildignore`
- Move: `QUALITY_ASSESSMENT.md` → `.github/QUALITY_ASSESSMENT.md`
- Move: `pkgdown/_pkgdown.yml` → `_pkgdown.yml`
- Modify: `.github/CONTRIBUTING.md`

**Interfaces:**
- Consumes: established baseline.
- Produces: ignore/build rules consistent with GitHub Pages and a pkgdown config that pkgdown reads.

- [ ] **Step 1: Fix `.gitignore`**

Remove the `docs/` line. Add:

```gitignore
revdep/library/
revdep/checks/
revdep/checks.noindex/
revdep/data.sqlite
revdep/email.yml
pkgdown-site/
```

- [ ] **Step 2: Fix `.Rbuildignore`**

Add:

```text
^QUALITY_ASSESSMENT\.md$
^AGENTS\.md$
^DEVELOPMENT\.md$
^Makefile$
^tools$
^\.editorconfig$
^\.lintr$
^\.github/development$
```

- [ ] **Step 3: Move and annotate the quality assessment**

```bash
git mv QUALITY_ASSESSMENT.md .github/QUALITY_ASSESSMENT.md
```

Add this note at the top of `.github/QUALITY_ASSESSMENT.md`:

```markdown
> **Historical document (2026-07-31).** Some findings are outdated: the
> repository now has a `testthat` suite and a GitHub Actions workflow.
> Keep this as a point-in-time assessment; see `DEVELOPMENT.md` for the
> current process.
```

- [ ] **Step 4: Move and clean the pkgdown config**

```bash
git mv pkgdown/_pkgdown.yml _pkgdown.yml
```

In `_pkgdown.yml`:
- change `navbar.title: "sparklyr"` to `navbar.title: "metan"`;
- remove the `ganalytics: UA-140429633-1` line and the now-empty `params` block if it only contained analytics.

- [ ] **Step 5: Update `.github/CONTRIBUTING.md`**

Replace the Travis/AppVeyor sentence with GitHub Actions. Add a short "Development commands" block:

```bash
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'
Rscript -e 'devtools::document()'
Rscript -e 'devtools::check()'
```

Mention that `man/` is generated by roxygen2 and must not be edited by hand.

- [ ] **Step 6: Verify the pkgdown config**

```bash
Rscript -e 'pkgdown::check_pkgdown()'
```

Expected: no errors; warnings are reported and evaluated.

- [ ] **Step 7: Verify ignore rules**

```bash
git check-ignore --no-index -v docs/index.html || echo "docs is not ignored (expected)"
git check-ignore --no-index -v .github/QUALITY_ASSESSMENT.md || echo "quality assessment is not ignored (expected)"
```

Expected: `docs` is not ignored; `.github/QUALITY_ASSESSMENT.md` is not ignored.

- [ ] **Step 8: Commit**

```bash
git add .gitignore .Rbuildignore .github/CONTRIBUTING.md .github/QUALITY_ASSESSMENT.md _pkgdown.yml
git add -u pkgdown
git commit -m "chore: align ignore rules, pkgdown config, and contributor docs"
```

---

## Task 5: Add the development boilerplate

**Files:**
- Create: `DEVELOPMENT.md`
- Create: `AGENTS.md`
- Create: `.editorconfig`
- Create: `.lintr`
- Create: `.github/pull_request_template.md`
- Create: `.github/dependabot.yml`
- Create: `Makefile`

**Interfaces:**
- Consumes: `tools/setup.R`, `tools/check-env.R`, and the branch/PR process.
- Produces: one place for a new contributor or agent to learn the workflow.

- [ ] **Step 1: Write `DEVELOPMENT.md`**

Cover:
- prerequisites (R 4.6 via rig, `~/.local/bin` on `PATH`, pandoc);
- first-time setup (`rig system user-mode`, `rig add release`, `Rscript tools/setup.R`, `Rscript tools/check-env.R`);
- daily commands (`devtools::load_all()`, `test()`, `document()`, `check()`);
- branch naming (`feat/...`, `fix/...`, `chore/...`);
- PR flow and the required CI check;
- NEWS.md expectation for user-facing changes;
- pkgdown policy (`docs/` is tracked because Pages serves `master`/`docs`; rebuild the site separately and review the diff);
- release checklist (version bump, NEWS, `R CMD check --as-cran`, revdep, CRAN submission, tag only at release);
- troubleshooting (system R is still 4.3.3; `R-4.6` fallback; missing system headers require the user to run `apt`).

- [ ] **Step 2: Write `AGENTS.md`**

Cover:
- repository purpose and layout;
- exact commands for load/test/document/check;
- rule: never edit `man/*.Rd` by hand;
- rule: no changes to statistical methods without tests;
- rule: update `NEWS.md` for user-facing changes;
- rule: keep `docs/` tracked and do not rebuild it in feature PRs;
- rule: run `devtools::test()` and `devtools::check()` before claiming completion;
- branch/PR conventions.

- [ ] **Step 3: Write `.editorconfig`**

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true
indent_style = space
indent_size = 2

[*.md]
trim_trailing_whitespace = false
```

- [ ] **Step 4: Write `.lintr`**

Use `linters_with_defaults()` with noisy linters disabled for this legacy codebase:

```r
linters_with_defaults(
  line_length_linter = NULL,
  object_usage_linter = NULL,
  cyclocomp_linter = NULL,
  commented_code_linter = NULL
)
```

- [ ] **Step 5: Write the PR template**

Include summary, related issue, test plan (`devtools::test()`, `devtools::check()`), and a checklist for NEWS/docs/tests.

- [ ] **Step 6: Write `.github/dependabot.yml`**

```yaml
version: 2
updates:
  - package-ecosystem: github-actions
    directory: /
    schedule:
      interval: weekly
    open-pull-requests-limit: 5
```

- [ ] **Step 7: Write the `Makefile`**

Targets:
- `setup`: `Rscript tools/setup.R`
- `check-env`: `Rscript tools/check-env.R`
- `document`: `Rscript -e 'devtools::document()'`
- `test`: `Rscript -e 'devtools::test()'`
- `check`: `Rscript -e 'devtools::check()'`
- `site`: `Rscript -e 'pkgdown::build_site()'`
- `help`: list targets

- [ ] **Step 8: Verify the Makefile and lint config**

```bash
make help
Rscript -e 'lintr::lint_package()' || true
```

Expected: `make help` lists targets; lintr runs and reports findings without breaking the build.

- [ ] **Step 9: Commit**

```bash
git add DEVELOPMENT.md AGENTS.md .editorconfig .lintr .github/pull_request_template.md .github/dependabot.yml Makefile
git commit -m "docs: add development guide, agent rules, and project tooling"
```

---

## Task 6: Harden CI and add the workflow badge

**Files:**
- Modify: `.github/workflows/R-CMD-check.yaml`
- Modify: `README.Rmd`
- Modify: `README.md`

**Interfaces:**
- Consumes: green local baseline from Task 3.
- Produces: blocking CI on R 4.3/4.5/4.6 and a visible status badge.

- [ ] **Step 1: Update the workflow**

- Rename the workflow to `R-CMD-check`.
- Remove `continue-on-error: true`.
- Change the matrix to `['4.3', '4.5', '4.6']`.
- Update the header comment to describe a blocking check.
- Keep `upload-snapshots: true`.

- [ ] **Step 2: Add the badge to `README.Rmd` and `README.md`**

Add near the other badges:

```markdown
[![R-CMD-check](https://github.com/NEPEM-UFSC/metan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NEPEM-UFSC/metan/actions/workflows/R-CMD-check.yaml)
```

- [ ] **Step 3: Validate the YAML**

```bash
Rscript -e 'yaml::read_yaml(".github/workflows/R-CMD-check.yaml")'
```

Expected: no error.

- [ ] **Step 4: Commit**

```bash
git add .github/workflows/R-CMD-check.yaml README.Rmd README.md
git commit -m "ci: make R-CMD-check blocking and test the declared minimum R"
```

---

## Task 7: Open the PR, clean stale branches, and enable branch protection

**Files:** none.

**Interfaces:**
- Consumes: `chore/dev-setup` branch and green local baseline.
- Produces: PR, deleted stale branches, protected `master`.

- [ ] **Step 1: Push the branch**

```bash
git push -u origin chore/dev-setup
```

- [ ] **Step 2: Open the PR**

```bash
gh pr create \
  --repo NEPEM-UFSC/metan \
  --base master \
  --head chore/dev-setup \
  --title "chore: development setup and repository hygiene" \
  --body "## Summary
...
## Test plan
...
"
```

Include the local test/check results in the body.

- [ ] **Step 3: Watch the CI**

```bash
gh pr checks --repo NEPEM-UFSC/metan --watch
```

Expected: all `R-CMD-check` matrix jobs pass. If a job fails, stop, report the failure, and do not enable branch protection.

- [ ] **Step 4: Delete the merged remote branches**

```bash
git push origin --delete feat fix/make-mat-across-deprecation
```

Expected: both refs deleted.

- [ ] **Step 5: Enable branch protection**

Use the exact check names reported by `gh pr checks` in the JSON payload:

```bash
gh api -X PUT repos/NEPEM-UFSC/metan/branches/master/protection --input - <<'JSON'
{
  "required_status_checks": {
    "strict": true,
    "contexts": ["R-CMD-check (4.3)", "R-CMD-check (4.5)", "R-CMD-check (4.6)"]
  },
  "enforce_admins": true,
  "required_pull_request_reviews": null,
  "restrictions": null,
  "allow_force_pushes": false,
  "allow_deletions": false,
  "required_conversation_resolution": true
}
JSON
```

Verify:

```bash
gh api repos/NEPEM-UFSC/metan/branches/master/protection
```

Expected: required checks and force-push/deletion restrictions are present.

- [ ] **Step 6: Merge after owner approval**

If the owner approves the PR:

```bash
gh pr merge --repo NEPEM-UFSC/metan --squash --delete-branch
```

If not, leave the PR open and report its URL.

---

## Task 8: Final verification and handoff

**Files:** none.

**Interfaces:**
- Consumes: all previous tasks.
- Produces: a verified, documented setup.

- [ ] **Step 1: Verify the working tree**

```bash
git status --short --branch
```

Expected: clean on `master` (after merge) or clean on `chore/dev-setup` (if the PR is open).

- [ ] **Step 2: Re-run the tests on the merged state**

```bash
Rscript -e 'devtools::test()'
```

Expected: all pass.

- [ ] **Step 3: Verify the remote state**

```bash
git ls-remote --heads origin
gh api repos/NEPEM-UFSC/metan/branches/master/protection
```

Expected: no `feat`/`fix/make-mat-across-deprecation` heads; protection present.

- [ ] **Step 4: Report**

Summarize:
- R version and library path;
- dependency setup command;
- branch/PR URL;
- CI result;
- branch protection status;
- any deferred item (pkgdown site rebuild, Windows/macOS CI, lint CI).
