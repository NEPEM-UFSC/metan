#!/usr/bin/env Rscript

# Report the R environment and verify that development packages are installed.

rig_lib <- file.path(
  Sys.getenv("HOME"),
  "R",
  R.version$platform,
  paste0(R.version$major, ".", R.version$minor)
)
if (dir.exists(rig_lib)) {
  .libPaths(c(rig_lib, .libPaths()))
}

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
