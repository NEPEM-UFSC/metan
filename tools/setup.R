#!/usr/bin/env Rscript

# Install all development dependencies declared in DESCRIPTION.
# Uses Posit Package Manager binaries when available.
#
# This project expects R 4.6 installed in user mode via rig. If the global
# R_LIBS_USER points at another R version's library, prepend the rig library
# for the running R version so pak is found.

if (getRversion() < "4.6.0") {
  stop(
    "metan development requires R >= 4.6.0; found ", R.version.string,
    call. = FALSE
  )
}

r_minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
rig_lib <- file.path(
  Sys.getenv("HOME"),
  "R",
  paste0(R.version$platform, "-library"),
  paste0(R.version$major, ".", r_minor)
)
if (dir.exists(rig_lib)) {
  .libPaths(c(rig_lib, .libPaths()))
}

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}

pak::local_install_dev_deps(root = ".", ask = FALSE)

# Tools used by the documented workflow but not listed in DESCRIPTION.
pak::pkg_install(
  c("devtools", "pkgdown", "rcmdcheck", "styler", "lintr"),
  ask = FALSE
)

# Keep roxygen2 at the version recorded in DESCRIPTION (RoxygenNote).
# Upgrade both together in a dedicated documentation PR.
pak::pkg_install("roxygen2@7.3.3", ask = FALSE)

message("Dependencies installed. Run `Rscript tools/check-env.R` to verify.")
