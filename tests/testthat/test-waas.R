# Golden-output regression test. Baseline recorded in tests/testthat/_snaps/
# and verified against R release (4.6.x); any future diff must be reviewed
# with testthat::snapshot_review() before accepting. PC scores are normalized
# for sign because eigenvectors are only defined up to sign.

test_that("waas() output is stable for balanced data", {
  mod <- data_ge |> waas(ENV, GEN, REP, GY, verbose = FALSE)

  expect_s3_class(mod, "waas")
  expect_snapshot_value(
    round_num(normalize_pc_sign(mod[[1]]$model)),
    style = "deparse"
  )
})
