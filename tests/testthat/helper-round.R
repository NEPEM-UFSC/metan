round_num <- function(x, digits = 5) {
  dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), \(v) round(v, digits)))
}
