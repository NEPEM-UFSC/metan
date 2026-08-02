test_that("ge_factanal() accepts trait-specific mineval as a named vector", {
  mod_all <- ge_factanal(data_ge2,
                         env = ENV, gen = GEN, rep = REP,
                         resp = c(PH, ED),
                         mineval = c(PH = 1.1, ED = 1.0),
                         verbose = FALSE)
  expect_s3_class(mod_all, "ge_factanal")
  expect_equal(names(mod_all), c("PH", "ED"))

  mod_ph <- ge_factanal(data_ge2,
                        env = ENV, gen = GEN, rep = REP,
                        resp = PH,
                        mineval = 1.1,
                        verbose = FALSE)
  mod_ed <- ge_factanal(data_ge2,
                        env = ENV, gen = GEN, rep = REP,
                        resp = ED,
                        mineval = 1.0,
                        verbose = FALSE)

  expect_equal(round_num(mod_all$PH$PCA), round_num(mod_ph$PH$PCA))
  expect_equal(round_num(mod_all$ED$PCA), round_num(mod_ed$ED$PCA))
})

test_that("ge_factanal() accepts trait-specific mineval as a named list", {
  mod_vec <- ge_factanal(data_ge2,
                         env = ENV, gen = GEN, rep = REP,
                         resp = c(PH, ED),
                         mineval = c(PH = 1.1, ED = 1.0),
                         verbose = FALSE)
  mod_list <- ge_factanal(data_ge2,
                          env = ENV, gen = GEN, rep = REP,
                          resp = c(PH, ED),
                          mineval = list(PH = 1.1, ED = 1.0),
                          verbose = FALSE)

  expect_equal(round_num(mod_list$PH$PCA), round_num(mod_vec$PH$PCA))
  expect_equal(round_num(mod_list$ED$PCA), round_num(mod_vec$ED$PCA))
})

test_that("a length-one mineval still applies to all response variables", {
  mod_all <- ge_factanal(data_ge2,
                         env = ENV, gen = GEN, rep = REP,
                         resp = c(PH, ED),
                         mineval = 1.1,
                         verbose = FALSE)
  mod_ph <- ge_factanal(data_ge2,
                        env = ENV, gen = GEN, rep = REP,
                        resp = PH,
                        mineval = 1.1,
                        verbose = FALSE)
  expect_equal(round_num(mod_all$PH$PCA), round_num(mod_ph$PH$PCA))
})

test_that("ge_factanal() throws informative errors for invalid mineval", {
  expect_error(
    ge_factanal(data_ge2, env = ENV, gen = GEN, rep = REP,
                resp = c(PH, ED), mineval = c(1.5, 1.0), verbose = FALSE),
    "named vector or list"
  )
  expect_error(
    ge_factanal(data_ge2, env = ENV, gen = GEN, rep = REP,
                resp = c(PH, ED), mineval = c(YY = 1.5, PH = 1.0), verbose = FALSE),
    "do not match any response variable"
  )
  expect_error(
    ge_factanal(data_ge2, env = ENV, gen = GEN, rep = REP,
                resp = c(PH, ED), mineval = c(PH = "high", ED = "low"), verbose = FALSE),
    "must be numeric"
  )
  expect_error(
    ge_factanal(data_ge2, env = ENV, gen = GEN, rep = REP,
                resp = TKW, mineval = 2, verbose = FALSE),
    "No eigenvalue"
  )
})
