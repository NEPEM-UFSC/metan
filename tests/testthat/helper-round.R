round_num <- function(x, digits = 5) {
  dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), \(v) round(v, digits)))
}

# Principal component scores are defined up to sign. BLAS/LAPACK builds can
# return opposite signs for the same component, so normalize each PC before
# snapshotting to keep the regression test deterministic across platforms.
normalize_pc_sign <- function(x) {
  pc_cols <- grep("^PC[0-9]+$", names(x), value = TRUE)
  for (col in pc_cols) {
    values <- x[[col]]
    if (length(values) > 0 && values[which.max(abs(values))] < 0) {
      x[[col]] <- -values
    }
  }
  x
}
