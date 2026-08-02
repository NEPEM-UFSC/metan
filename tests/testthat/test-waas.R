# Golden-output regression test. Baseline recorded in tests/testthat/_snaps/
# and verified against R release (4.6.1); any future diff must be reviewed
# with testthat::snapshot_review() before accepting.

test_that("waas() output is stable for balanced data", {
  mod <- data_ge |> waas(ENV, GEN, REP, GY, verbose = FALSE)

  expect_s3_class(mod, "waas")
  expect_snapshot_value(round_num(mod[[1]]$model), style = "deparse")
})
