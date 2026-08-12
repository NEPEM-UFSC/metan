test_that("make_mat() produces a stable two-way table (mean)", {
  m <- data_ge |> make_mat(row = GEN, col = ENV, value = GY)

  expect_s3_class(m, "data.frame")
  expect_equal(dim(m), c(10, 14))
  expect_equal(rownames(m)[1:3], c("G1", "G10", "G2"))
  expect_equal(
    unname(unlist(m[1:3, 1:3])),
    c(2.365787, 1.974073, 2.901747, 2.307633, 1.536253, 2.297053, 1.355667, 0.899100, 1.490720),
    tolerance = 1e-5
  )
})

test_that("make_mat() honors a custom summary function (sem)", {
  m <- data_ge |> make_mat(GEN, ENV, GY, sem)

  expect_s3_class(m, "data.frame")
  expect_equal(dim(m), c(10, 14))
  expect_true(all(unlist(m) >= 0))
})

test_that("make_mat() na.rm behavior is unchanged by the across() lambda fix", {
  data_na <- data_ge
  data_na$GY[1] <- NA

  m <- data_na |> make_mat(row = GEN, col = ENV, value = GY)

  expect_false(anyNA(m))
  expect_equal(dim(m), c(10, 14))
})
