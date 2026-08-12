test_that("round_cols() works without the dplyr across() deprecation warning", {
  data <- data.frame(ENV = c("FA1", "FA1", "FA2"),
                     GEN = c("BGB083", "BGB083", "BGB083"),
                     GY = c(1.2345, 2.5678, 3.1415))

  expect_no_warning(res <- round_cols(data, digits = 2))
  expect_equal(res$GY, c(1.23, 2.57, 3.14))

  expect_no_warning(res2 <- round_cols(data, GY, digits = 1))
  expect_equal(res2$GY, c(1.2, 2.6, 3.1))
})

test_that("remove_space() works without the dplyr across() deprecation warning", {
  data <- data.frame(ENV = c("FA 1", "FA1", "FA2"),
                     GEN = c("BG B083", "BGB083", "BGB083"),
                     num = c(1, 2, NA))

  expect_no_warning(res <- remove_space(data))
  expect_equal(res$ENV, c("FA1", "FA1", "FA2"))
  expect_equal(res$GEN, c("BGB083", "BGB083", "BGB083"))

  expect_no_warning(res2 <- remove_space(data, ENV))
  expect_equal(res2$ENV, c("FA1", "FA1", "FA2"))
})

test_that("remove_strings() works without the dplyr across() deprecation warning", {
  data <- data.frame(GEN = c("G1", "G2", "G3"),
                     txt = c("12.5", "8.0", "3.0"))

  expect_no_warning(res <- remove_strings(data, txt))
  expect_equal(res$txt, c(12.5, 8, 3))
  expect_type(res$txt, "double")
})
