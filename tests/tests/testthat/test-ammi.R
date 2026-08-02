# Golden-output regression test. Baseline recorded in tests/testthat/_snaps/
# and verified against R release (4.6.1); any future diff must be reviewed
# with testthat::snapshot_review() before accepting.

test_that("ammi() output is stable for balanced data", {
  mod <- data_ge |> ammi(ENV, GEN, REP, GY, verbose = FALSE)

  expect_s3_class(mod, "performs_ammi")
  expect_snapshot_value(round_num(mod[[1]]$MeansGxE), style = "deparse")
  expect_snapshot_value(round_num(mod[[1]]$ANOVA), style = "deparse")
})
