# Software Package Quality Assessment — `metan`

**Package:** metan (Multi Environment Trials Analysis) · v1.20.0
**Assessed:** 2026-07-31 · Branch: `fix/make-mat-across-deprecation`
**Method:** Static inspection of source tree, NAMESPACE, DESCRIPTION, git history, and structural metrics. No `R CMD check`, no test execution (no test suite exists — see §Testing). Findings below are evidence-based; anything not directly observed is flagged as a limitation rather than assumed.

---

## 1. Executive Summary

`metan` is a mature, widely-used CRAN package (286 exports, 69 S3 methods, 2,528 commits, single long-term maintainer) providing statistical methods for multi-environment trial analysis in agronomy. It is functionally rich and well-documented at the user level (full Rd/roxygen coverage, vignettes, pkgdown site, CRAN comments). However, from a software-engineering standpoint it shows the profile typical of a domain-scientist-authored statistical package that grew organically over years: **no automated test suite, no CI/CD, several very large modules (one file is 2,705 lines), and a single-maintainer bus-factor of 1.** The current branch under review is a one-line, well-targeted fix for a `dplyr` `across()` deprecation — appropriately scoped and low-risk.

## 2. Scores

| Dimension | Score |
|---|---|
| Overall Health | 52 / 100 |
| Software Quality | 55 / 100 |
| Architecture | 45 / 100 |
| Maintainability | 48 / 100 |
| Readability | 62 / 100 |
| Technical Debt | 40 / 100 (higher = less debt) |
| Test Quality | 5 / 100 |
| Documentation | 75 / 100 |

Scores reflect engineering-process maturity, not statistical correctness or scientific value, which is outside this assessment's scope.

## 3. Top Strengths

1. **Comprehensive user-facing documentation** — every exported function has an `.Rd` file, roxygen `@examples`, and the package ships 9 vignettes plus a pkgdown site and cheat sheet.
2. **Consistent public API conventions** — snake_case naming, consistent `print.*`/`plot.*`/`predict.*` S3 pattern across model classes (`waas`, `waasb`, `gamem`, `mgidi`, `gge`, …), giving users a predictable mental model once they learn one method.
3. **Active maintenance discipline on dependency drift** — the branch under review shows the maintainer proactively fixing a `dplyr` deprecation (`across(..., fun, na.rm=TRUE)` → `across(..., \(x) fun(x, na.rm=TRUE))`) ahead of it becoming a hard error, and `NEWS.md`/`cran-comments.md` are kept current.
4. **No global-state smells** — zero uses of `<<-` across 34k lines; side effects are locally scoped.
5. **Clean licensing/metadata** — `DESCRIPTION` is complete (title, description with DOIs for every method, URL, BugReports, versioned `Imports`).

## 4. Top Weaknesses

1. **No automated test suite.** `tests/testthat` does not exist, `testthat` is not even in `Suggests`. For 114 R files and 286 exported functions implementing nontrivial statistics (AMMI, BLUP/mixed models, MTSI, GGE), there is zero regression protection.
2. **No CI/CD.** No `.github/workflows`, no `.travis.yml`. Every change — including dependency-deprecation fixes like the current branch — merges without any automated check that the package still builds, loads, or produces correct output.
3. **God module: `R/utilities.R` at 2,705 lines**, plus several other 900–1,300 line files (`WAASB.R`, `gge.R`, `get_model_data.R`, `gamem.R`, `path_coeff.R`). These concentrate unrelated helpers and inflate cognitive load per file.
4. **Bus factor of 1.** Single author/maintainer/copyright-holder for the entire package; no CONTRIBUTING-enforced review gate visible in the repo (a `CONTRIBUTING.md` exists but there's no branch protection or CI to enforce it).
5. **187 explicit `for` loops** across the codebase in an ecosystem (tidyverse-based, per `Imports`) that generally favors vectorized/functional idioms — a signal of inconsistent style between older and newer modules, and a likely source of the very large file sizes.

## 5. Code Smells Detected

| Smell | Evidence | Severity | Cost | Suggested fix |
|---|---|---|---|---|
| God module | `R/utilities.R` (2,705 lines) mixes unrelated helper concerns | High | High (any change risks unrelated breakage, no tests to catch it) | Split by concern (e.g. `utils_string.R`, `utils_matrix.R`, already partially done via `utils_*.R` siblings — `utilities.R` should be fully absorbed into that pattern) |
| Long files / low cohesion | 6 files >900 lines (`gamem.R`, `path_coeff.R`, `get_model_data.R`, `WAASB.R`, `gge.R`, `mgidi.R`) | Medium | Medium | Extract per-method helpers into private files, keep public entry point thin |
| Missing regression safety net | No `tests/` directory at all | Critical | Very high — every refactor or dependency bump (like this branch's fix) is unverified | Introduce `testthat` with golden-output snapshot tests against the bundled datasets (`data_ge`, `data_ge2`, `data_g`, `data_alpha`) |
| Deprecated-API churn | `.Deprecated()`/deprecated markers present in 11 files; the branch itself is reacting to an upstream (`dplyr`) deprecation | Medium | Recurring (any tidyverse major bump repeats this class of fix) | Pin/test against tidyverse CI matrix so deprecations are caught automatically, not manually noticed |
| Loop-heavy style inconsistency | 187 `for` loops in a tidyverse-idiom codebase | Low-Medium | Medium (readability drag, harder to reason about vectorized correctness) | Not urgent to rewrite wholesale; apply during touched-file refactors only |

No circular dependencies, no `<<-` global mutation, and no dead `TODO/FIXME` markers were found — these are genuine positives, not just absence of evidence to the contrary within what was inspected.

## 6. Architecture Review

The package follows R's standard flat architecture: no internal package/namespace layering beyond `R/*.R` + `NAMESPACE`, which is normal and appropriate for a package of this kind — this is not itself a smell. Within that constraint:

- **Separation of concerns is uneven.** Method-specific files (`ammi.R`, `waas.R`, `gge.R`, `mgidi.R`) are reasonably self-contained, but generic helpers are split awkwardly between purpose-named `utils_*.R` files (na, sets, bind, as, data_org, sample, progress, nasapower, tidy-eval) *and* a catch-all `utilities.R` — an architectural inconsistency (two competing conventions for the same kind of content).
- **Coupling** across model objects is via shared S3 dispatch (`print`, `plot`, `predict`) and a common `get_model_data()` accessor (1,084 lines) — a reasonable pattern, though `get_model_data.R`'s size suggests it has become a de facto dispatch god-function across many model types (the recent history shows it churned 102 times, the 4th-highest of any file, consistent with it absorbing new model types over time).
- **Extensibility** for new methods looks straightforward (the AMMI→ammi rename in recent history, and `get_model_data()`'s "projection" addition, both suggest a working extension pattern), but each new model type adds another branch to `get_model_data()` rather than a more polymorphic dispatch — an early sign of the "shotgun surgery" smell if the number of model types keeps growing.

## 7. Quality Traits (1–10)

| Trait | Score | Rationale |
|---|---|---|
| Maintainability | 5 | Good naming/docs, but large files + zero tests make change confidently risky |
| Readability | 6 | Consistent naming conventions; large files hurt local readability |
| Reliability | 4 | No tests means reliability is asserted, not verified |
| Robustness | 4 | Same reasoning; edge-case behavior is unverified by automation |
| Simplicity | 5 | Individual functions are typically direct; some files overloaded |
| Modularity | 5 | File-per-method is a reasonable module boundary; `utilities.R` breaks it |
| Extensibility | 6 | Established S3 + `get_model_data()` pattern for new methods |
| Scalability | 6 | N/A in the runtime-scaling sense for a CRAN stats package; code-base scaling (adding more methods) is the relevant axis and is moderate |
| Testability | 3 | Nothing prevents testing (pure functions, tidy data in/out) but nothing exercises it |
| Reusability | 6 | Exported functions are independently usable; internal helpers less so given `utilities.R` |
| Consistency | 6 | Naming/API consistent; loop-vs-vectorized style is not |
| Observability | 3 | Minimal logging; a `cli`-based progress/messaging layer exists (`utils_progress.R`) but no structured diagnostics |
| Performance | N/A | Not assessed — requires profiling, out of static-analysis scope |
| Security | 8 | Low attack surface (no network/eval/system calls found beyond `utils_nasapower.R`'s intentional API client); nothing alarming observed |
| Portability | 7 | Pure R + CRAN deps, `R (>= 4.1.0)`, no OS-specific code seen |
| Developer Experience | 5 | Good docs for *users*; no CI feedback loop or test harness for *contributors* |

## 8. Technical Debt

- **Structural debt:** `utilities.R` and the 900+ line files represent accumulated debt from organic growth without periodic consolidation.
- **Process debt (dominant item):** absence of tests + CI is the single largest debt item. It doesn't block current functionality but makes every future change — including trivial-looking ones like this branch's — unverifiable except by manual/visual inspection or downstream user bug reports.
- **Long-term impact:** Low near-term risk (the package works, is CRAN-published, revdep checks exist in `revdep/`), but maintenance cost per change will keep rising as the file sizes and method count grow, and any contributor besides the sole maintainer faces a steep, unguarded ramp-up.

## 9. Risk Matrix

| Area | Risk | Why |
|---|---|---|
| `R/utilities.R` | High | Size + breadth of use + no tests = highest blast radius per edit |
| `get_model_data.R` | High | Central dispatch point touched by nearly every model type; 102 historical changes, no tests |
| `WAASB.R` / mixed-model code (`lme4`/`lmerTest`) | Medium-High | Statistically complex, hardest code to eyeball-verify without tests |
| Tidyverse-deprecation-reactive files (e.g. `make_mat.R`) | Low | Small, mechanical fixes like the current branch; low blast radius, easy to verify by inspection |
| Package metadata / DESCRIPTION | Low | Clean and complete |

## 10. Testing Assessment

**Status: absent.** There is no `tests/` directory, no `testthat` dependency, and `R CMD check`'s test step would currently be a no-op. This is the most consequential finding in this report:

- No unit tests for any of the 286 exported functions.
- No regression protection for statistical correctness (AMMI, WAASB, MTSI, GGE, path coefficients, etc.) — these are numerically subtle methods where silent regressions are especially costly to catch by inspection alone.
- No test at all exists for the specific change on this branch (`across()` lambda-syntax fix) — its correctness currently rests on the maintainer's manual verification.
- The bundled datasets (`data_ge`, `data_ge2`, `data_g`, `data_alpha`, `int.effects`, `meansGxE`) are ready-made fixtures for snapshot/golden-file tests and are not currently used for that purpose.

## 11. Documentation Assessment

Strong for an R package: full `.Rd` coverage (roxygen-generated, `RoxygenNote: 7.3.3`, `Roxygen: list(markdown = TRUE)`), 9 vignettes covering AMMI, BLUP, GGE, indexes, stability, cross-validation, biometry, descriptive stats, and utilities, a maintained `README.Rmd`/`README.md`, `NEWS.md`, and a pkgdown site with rendered `docs/`. No gaps identified at the user-facing documentation layer. The gap is exclusively on the *developer*-facing side (no CONTRIBUTING-enforced test/CI expectations, despite `CONTRIBUTING.md` existing).

## 12. Definition of Done Matrix

| Criterion | Status | Evidence | Recommendation |
|---|---|---|---|
| Code compiles/loads | Pass (inferred) | Package is CRAN-published at v1.20.0 | — |
| Linting | Not observed | No `.lintr` or lint CI step found | Add `lintr` + CI step |
| Formatting consistency | Partial | Generally consistent style, mixed loop/vectorized idiom | Adopt `styler`/`Air` formatting in CI |
| Documentation complete | Pass | Full Rd + vignettes | — |
| Tests exist | **Fail** | No `tests/` dir | Add `testthat` suite, start with high-risk files (§9) |
| Tests meaningful/pass | **Fail** | N/A — none exist | — |
| Examples work | Likely pass (unverified) | `@examples` present throughout; not executed in this assessment | Run `R CMD check --as-cran` to confirm |
| APIs documented | Pass | — | — |
| Error handling consistent | Partial | Only 1 direct `stop(` hit at repo root scan depth used here; deeper check needed | Audit error-handling consistency across all 114 files |
| Package metadata complete | Pass | `DESCRIPTION` fully populated | — |
| Versioning coherent | Pass | `NEWS.md` tracks versions, `cran-comments.md` current | — |
| CI/CD validates quality | **Fail** | No `.github/workflows` | Add GitHub Actions: R CMD check, testthat, lint, on push/PR |
| Review readiness | Partial | Small, well-scoped current diff is review-ready; process lacks enforced review gate | Add branch protection requiring CI pass |

## 13. Engineering Maturity

**Growing / Production-Ready (functionally), Early-Development (process).** The package is functionally mature and trusted enough to be on CRAN with a real user base and citation-worthy methods, but its engineering process (no tests, no CI) sits at an early-development maturity level. This is a common and not-unusual profile for academic-authored statistical R packages, but it caps how safely the codebase can be extended by anyone other than its original author.

## 14. Improvement Roadmap

**Quick wins**
- Add `testthat` to `Suggests`, scaffold `tests/testthat/`, write a handful of snapshot tests against the bundled datasets for the highest-churn files (`get_model_data.R`, `WAASB.R`, `utilities.R`).
- Add a GitHub Actions workflow running `R CMD check` + `testthat` on push/PR (a template exists via `usethis::use_github_action_check_standard()`).
- For this branch specifically: add one regression test asserting `make_mat()` output is unchanged before/after the `across()` lambda fix, since that's exactly the kind of change tests exist to protect.

**Medium-term**
- Split `R/utilities.R` into topic-scoped files following the existing `utils_*.R` naming convention already used elsewhere.
- Introduce `lintr`/`styler` in CI to converge the loop-vs-vectorized style inconsistency going forward (not a mass rewrite).
- Build out test coverage for the statistically core methods (AMMI, WAASB, MTSI) using the bundled datasets as golden fixtures.

**Long-term**
- Reduce `get_model_data.R`'s central-dispatch god-function pattern in favor of a more polymorphic per-model-class accessor, so adding a new stability method doesn't require editing a shared 1,000+ line file.
- Consider a second maintainer/reviewer to address the bus-factor-of-1 risk, gated by the CI above so review isn't purely manual trust.

## 15. Final Assessment

`metan` is a scientifically well-documented, functionally mature R package let down by an engineering process that hasn't kept pace with its own growth: no tests, no CI, and a few oversized modules are the concrete, fixable gaps. The change under review on this branch is a good example of the maintainer doing the right thing (proactively fixing a `dplyr` deprecation) without the safety net (tests/CI) that would let anyone — including future-the-maintainer — verify it beyond inspection.

## 16. Recommended Next Steps

1. Merge the current one-line fix (it's correct on inspection: `across(cols, fun, na.rm = TRUE)` and `across(cols, \(x) fun(x, na.rm = TRUE))` are equivalent under dplyr's current `across()` semantics, and the change is scoped to exactly the deprecated call).
2. Before or immediately after merging, add a minimal `testthat` scaffold and one snapshot test covering `make_mat()`'s output — the cheapest possible step that converts this fix from "trust me" to "verified."
3. Open a follow-up (non-blocking) item to introduce CI via `usethis::use_github_action_check_standard()`.
4. Track `R/utilities.R` decomposition as a separate, lower-priority refactor — not blocking, but worth scheduling given its size and churn.

---
*Limitations: this assessment is static/structural. It did not execute `R CMD check`, run examples, profile performance, or verify statistical correctness of any method — those require executing R and are recommended as a follow-up, not claimed here.*
