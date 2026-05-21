#' Get data from a model easily
#'
#' `r badge('experimental')`
#'
#' * `get_model_data()` Easily get data from some objects generated in the
#' **metan** package such as the WAASB and WAASBY indexes  (Olivoto et al.,
#' 2019a, 2019b) BLUPs, variance components, details of AMMI models and
#' AMMI-based stability statistics.
#' * `gmd()` Is a shortcut to `get_model_data`.
#' * `sel_gen()` Extracts the selected genotypes by a given index.
#' @param x An object created with the functions [ammi_indexes()],
#'   [anova_ind()], [anova_joint()], [can_corr()] [ecovalence()],  [finlay_wilkinson()], [Fox()],
#'   [gai()], [gamem()],[gafem()], [ge_acv()], [ge_means()], [eberhart_russell()],
#'   [gytb()], [mgidi()], [performs_ammi()], [blup_indexes()], [Shukla()],
#'   [lin_binns()], [waas()] or [waasb()].
#' @param what What should be captured from the model. See more in section
#'   **Details**.
#' @param type Chose if the statistics must be show by genotype (`type =
#'   "GEN"`, default) or environment (`TYPE = "ENV"`), when possible.
#' @param verbose Logical argument. If `verbose = FALSE` the code will run
#'   silently.
#' @return A tibble showing the values of the variable chosen in argument
#'   `what`.
#' @name get_model_data
#' @details
#' Bellow are listed the options allowed in the argument `what` depending
#' on the class of the object
#'
#'  **Objects of class `ammi_indexes`:**
#' * `"ASV"` AMMI stability value.
#' * `"EV"` Averages of the squared eigenvector values.
#' * `"SIPC"` Sums of the absolute value of the IPCA scores.
#' * `"WAAS"` Weighted average of absolute scores (default).
#' * `"ZA"` Absolute value of the relative contribution of IPCAs to the
#' interaction.
#'
#'  **Objects of class `anova_ind`:**
#' * `"MEAN"`The mean value of the variable
#' * `"DFG", "DFB", "DFCR", "DFIB_R", "DFE"`. The degree of freedom for
#' genotypes, blocks (randomized complete block design), complete replicates,
#' incomplete blocks within replicates (alpha-lattice design), and error,
#' respectively.
#' * `"MSG", "FCG", "PFG"` The mean square, F-calculated and P-values for
#' genotype effect, respectively.
#' * `"MSB", "FCB", "PFB"` The mean square, F-calculated and P-values for
#' block effect in randomized complete block design.
#' * `"MSCR", "FCR", "PFCR"` The mean square, F-calculated and P-values for
#' complete replicates in alpha lattice design.
#' * `"MSIB_R", "FCIB_R", "PFIB_R"` The mean square, F-calculated and
#' P-values for incomplete blocks within complete replicates, respectively (for
#' alpha lattice design only).
#' * `"MSE"` The mean square of error.
#' * `"CV"` The coefficient of variation.
#' * `"h2"` The broad-sence heritability
#' * `"AS"` The accucary of selection (square root of h2).
#' * `"FMAX"` The Hartley's test (the ratio of the largest MSE to the smallest
#' MSE).
#'
#'
#'  **Objects of class `anova_joint` or `gafem`:**
#' * `"Y"` The observed values.
#' * `"h2"` The broad-sense heritability.
#' * `"Sum Sq"` Sum of squares.
#' * `"Mean Sq"` Mean Squares.
#' * `"F value"` F-values.
#' * `"Pr(>F)"` P-values.
#' * `".fitted"` Fitted values (default).
#' * `".resid"` Residuals.
#' * `".stdresid"` Standardized residuals.
#' * `".se.fit"` Standard errors of the fitted values.
#' * `"details"` Details.
#'
#'  **Objects of class `Annicchiarico` and `Schmildt`:**
#' * `"Sem_rp"` The standard error of the relative mean performance (Schmildt).
#' * `"Mean_rp"` The relative performance of the mean.
#' * `"rank"` The rank for genotypic confidence index.
#' * `"Wi"` The genotypic confidence index.
#'
#'  **Objects of class `can_corr`:**
#' * `"coefs"` The canonical coefficients (default).
#' * `"loads"` The canonical loadings.
#' * `"crossloads"` The canonical cross-loadings.
#' * `"canonical"` The canonical correlations and hypothesis testing.
#'
#'  **Objects of class `colindiag`:**
#' * `"cormat"` The correlation matrix betwen predictors.
#' * `"corlist"` The correlations in a 'long' format
#' * `"evalevet"` The eigenvalue with associated eigenvectors
#' * `"VIF"` The Variance Inflation Factor
#' * `"indicators"` The colinearity indicators
#'
#'  **Objects of class `ecovalence`:**
#' * `"Ecoval"` Ecovalence value (default).
#' * `"Ecov_perc"` Ecovalence in percentage value.
#' * `"rank"` Rank for ecovalence.
#'
#'  **Objects of class `fai_blup`:** See the **Value** section of
#'  [fai_blup()] to see valid options for `what` argument.
#'
#'  **Objects of class `ge_acv`:**
#' * `"ACV"` The adjusted coefficient of variation (default).
#' * `"ACV_R"` The rank for adjusted coefficient of variation.
#'
#'  **Objects of class `ge_polar`:**
#' * `"POLAR"` The Power Law Residuals (default).
#' * `"POLAR_R"` The rank for Power Law Residuals.
#'
#'  **Objects of class `FW`:**
#' * `"estimates"` The genotype intercepts and slopes of the regression (default).
#' * `"predictions"` The predicted values.
#' * `"var_e"` The residual variances for each genotype.
#' * `"var_e_weighted"` The pooled weighted error variance.
#'
#'  **Objects of class `eberhart_russell`:**
#' * `GEN`: the genotypes.
#' * `b0` and `b1` (default): the intercept and slope of the regression,
#' respectively.
#' * `t(b1=1)`: the calculated t-value
#' * `pval_t`: the p-value for the t test.
#' *  `s2di` the deviations from the regression (stability parameter).
#' * `F(s2di=0)`: the F-test for the deviations.
#' *  `pval_f`: the p-value for the F test;
#' *  `RMSE` the root-mean-square error.
#' *  `R2` the determination coefficient of the regression.
#'
#'
#'  **Objects of class `ge_effects`:**
#' * For objects of class `ge_effects` no argument `what` is required.
#'
#'  **Objects of class `ge_means`:**
#' * `"ge_means"` Genotype-environment interaction means (default).
#' * `"env_means"` Environment means.
#' * `"gen_means"` Genotype means.
#'
#'  **Objects of class `gge`:**
#' * `"scores"` The scores for genotypes and environments for all the
#' analyzed traits (default).
#' * `"exp_var"` The eigenvalues and explained variance.
#' * `"projection"` The projection of each genotype in the AEC coordinates in
#' the stability GGE plot
#'
#'  **Objects of class `gytb`:**
#' * `"gyt"` Genotype by yield*trait table (Default).
#' * `"stand_gyt"` The standardized (zero mean and unit variance) Genotype by yield*trait table.
#' * `"si"` The superiority index (sum standardized value across all yield*trait combinations).
#'
#'  **Objects of class `mgidi`:** See the **Value** section of
#'  [mgidi()] to see valid options for `what` argument.
#'
#'  **Objects of class `mtsi`:** See the **Value** section of
#'  [mtsi()] to see valid options for `what` argument.
#'
#'  **Objects of class `path_coeff`
#' * `"coef"` Path coefficients
#' * `"eigenval"` Eigenvalues and eigenvectors.
#' * `"vif "` Variance Inflation Factor
#'
#'  **Objects of class `path_coeff_seq`
#' * `"resp_fc"` Coefficients of primary predictors and response
#' * `"resp_sc"` Coefficients of secondary predictors and response
#' * `"resp_sc2"` contribution to the total effects through primary traits
#' * `"fc_sc_coef"` Coefficients of secondary predictors and primary predictors.
#'
#'  **Objects of class `Shukla`:**
#' * `"rMean"` Rank for the mean.
#' * `"ShuklaVar"` Shukla's stablity variance (default).
#' * `"rShukaVar"` Rank for Shukla's stablity variance.
#' * `"ssiShukaVar"` Simultaneous selection index.
#'
#'  **Objects of class `sh`:** See the **Value** section of
#'  [Smith_Hazel()] to see valid options for `what` argument.
#'
#'  **Objects of class `Fox`:**
#' * `"TOP"` The proportion of locations at which the genotype occurred in
#' the top third (default).
#'
#'  **Objects of class `gai`:**
#' * `"GAI"` The geometric adaptability index (default).
#' * `"GAI_R"` The rank for the GAI values.
#'
#'  **Objects of class `lin_binns`:**
#' * `"Pi_a"` The superiority measure for all environments (default).
#' * `"R_a"` The rank for Pi_a.
#' * `"Pi_f"` The superiority measure for favorable environments.
#' * `"R_f"` The rank for Pi_f.
#' * `"Pi_u"` The superiority measure for unfavorable environments.
#' * `"R_u"` The rank for Pi_u.
#'
#'  **Objects of class `Huehn`:**
#' * `"S1"` Mean of the absolute rank differences of a genotype over the n
#' environments (default).
#' * `"S2"` variance among the ranks over the k environments.
#' * `"S3"` Sum of the absolute deviations.
#' * `"S6"` Relative sum of squares of rank for each genotype.
#' * `"S1_R"`, `"S2_R"`, `"S3_R"`, and  `"S6_R"`, the ranks
#' for S1, S2, S3, and S6, respectively.
#'
#'  **Objects of class `Thennarasu`:**
#' * `"N1"` First statistic (default).
#' * `"N2"` Second statistic.
#' * `"N3"` Third statistic.
#' * `"N4"` Fourth statistic.
#' * `"N1_R"`, `"N2_R"`, `"N3_R"`, and `"N4_R"`, The ranks
#' for the statistics.
#'
#'
#'  **Objects of class `performs_ammi`:**
#' * `"PC1", "PC2", ..., "PCn"` The values for the nth interaction
#' principal component axis.
#' * `"ipca_ss"` Sum of square for each IPCA.
#' * `"ipca_ms"` Mean square for each IPCA.
#' * `"ipca_fval"` F value for each IPCA.
#' * `"ipca_pval"` P-value for for each IPCA.
#' * `"ipca_expl"`  Explained sum of square for each IPCA (default).
#' * `"ipca_accum"` Accumulated explained sum of square.
#'
#'
#' **Objects of class `waas`, `waas_means`, and `waasb`:**
#' * `"PC1", "PC2", ..., "PCn"` The values for the nth interaction
#' principal component axis.
#' * `"WAASB"`  The weighted average of the absolute scores (default for
#' objects of class `waas`).
#' * `"PctResp"` The rescaled values of the response variable.
#' * `"PctWAASB"` The rescaled values of the WAASB.
#' * `"wResp"` The weight for the response variable.
#' * `"wWAASB"` The weight for the stability.
#' * `"OrResp"` The ranking regarding the response variable.
#' * `"OrWAASB"` The ranking regarding the WAASB.
#' * `"OrPC1"` The ranking regarding the first principal component axix.
#' * `"WAASBY"` The superiority index WAASBY.
#' * `"OrWAASBY"` The ranking regarding the superiority index.
#'
#'  **Objects of class `gamem` and `waasb`:**
#' * `"blupge"` Best Linear Unbiased Prediction for genotype-environment
#' interaction (mixed-effect model, class `waasb`).
#' * `"blupg"` Best Linear Unbiased Prediction for genotype effect.
#' * `"bluege"` Best Linear Unbiased Estimation for genotype-environment
#' interaction (fixed-effect model, class `waasb`).
#' * `"blueg"` Best Linear Unbiased Estimation for genotype effect (fixed
#' model).
#' * `"data"` The data used.
#' * `"details"` The details of the trial.
#' * `"genpar"` Genetic parameters (default).
#' * `"gcov"` The genotypic variance-covariance matrix.
#' * `"pcov"` The phenotypic variance-covariance matrix.
#' * `"gcor"` The genotypic correlation matrix.
#' * `"pcor"` The phenotypic correlation matrix.
#' * `"h2"` The broad-sense heritability.
#' * `"lrt"` The likelihood-ratio test for random effects.
#' * `"vcomp"` The variance components for random effects.
#' * `"ranef"` Random effects.
#'
#'  **Objects of class `blup_ind`**
#' * `"HMGV","HMGV_R"` For harmonic mean of genotypic values or its ranks.
#' * `"RPGV", RPGV_Y"` For relative performance of genotypic values or its
#' ranks.
#' * `"HMRPGV", "HMRPGV_R"` For harmonic mean of relative performance of
#' genotypic values or its ranks.
#' * `"WAASB", "WAASB_R"` For the weighted average of absolute scores from the
#' singular or its ranks. value decomposition of the BLUPs for GxE interaction
#' or its ranks.
#'
#' @md
#' @importFrom dplyr starts_with matches case_when full_join arrange_if
#' @importFrom purrr reduce
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @export
#' @references
#'
#' Annicchiarico, P. 1992. Cultivar adaptation and recommendation from alfalfa
#' trials in Northern Italy. J. Genet. Breed. 46:269-278.
#'
#' Dias, P.C., A. Xavier, M.D.V. de Resende, M.H.P. Barbosa, F.A. Biernaski,
#' R.A. Estopa. 2018. Genetic evaluation of Pinus taeda clones from somatic
#' embryogenesis and their genotype x environment interaction. Crop Breed. Appl.
#' Biotechnol. 18:55-64.
#' \doi{10.1590/1984-70332018v18n1a8}
#'
#' Azevedo Peixoto, L. de, P.E. Teodoro, L.A. Silva, E.V. Rodrigues, B.G.
#' Laviola, and L.L. Bhering. 2018. Jatropha half-sib family selection with high
#' adaptability and genotypic stability. PLoS One 13:e0199880.
#' \doi{10.1371/journal.pone.0199880}
#'
#' Eberhart, S.A., and W.A. Russell. 1966. Stability parameters for comparing
#' Varieties. Crop Sci. 6:36-40.
#' \doi{10.2135/cropsci1966.0011183X000600010011x}
#'
#' Fox, P.N., B. Skovmand, B.K. Thompson, H.J. Braun, and R. Cormier. 1990.
#' Yield and adaptation of hexaploid spring triticale. Euphytica 47:57-64.
#' \doi{10.1007/BF00040364}
#'
#' Huehn, V.M. 1979. Beitrage zur erfassung der phanotypischen stabilitat. EDV
#' Med. Biol. 10:112.
#'
#' Olivoto, T., A.D.C. Lúcio, J.A.G. da silva, V.S. Marchioro, V.Q. de
#' Souza, and E. Jost. 2019a. Mean performance and stability in
#' multi-environment trials I: Combining features of AMMI and BLUP techniques.
#' Agron. J. 111:2949-2960.
#' \doi{10.2134/agronj2019.03.0220}
#'
#' Olivoto, T., A.D.C. Lúcio, J.A.G. da silva, B.G. Sari, and M.I. Diel.
#' 2019b. Mean performance and stability in multi-environment trials II:
#' Selection based on multiple traits. Agron. J. 111:2961-2969.
#' \doi{10.2134/agronj2019.03.0221}
#'
#' Purchase, J.L., H. Hatting, and C.S. van Deventer. 2000.
#' Genotype vs environment interaction of winter wheat (Triticum aestivum L.)
#' in South Africa: II. Stability analysis of yield performance. South African
#' J. Plant Soil 17:101-107.
#' \doi{10.1080/02571862.2000.10634878}
#'
#' Resende MDV (2007) Matematica e estatistica na analise de experimentos e no
#' melhoramento genetico. Embrapa Florestas, Colombo
#'
#' Sneller, C.H., L. Kilgore-Norquest, and D. Dombek. 1997. Repeatability of
#' Yield Stability Statistics in Soybean. Crop Sci. 37:383-390.
#' \doi{10.2135/cropsci1997.0011183X003700020013x}
#'
#' Mohammadi, R., & Amri, A. (2008). Comparison of parametric and non-parametric
#' methods for selecting stable and adapted durum wheat genotypes in variable
#' environments. Euphytica, 159(3), 419-432.
#' \doi{10.1007/s10681-007-9600-6}
#'
#' Wricke, G. 1965. Zur berechnung der okovalenz bei sommerweizen und hafer. Z.
#' Pflanzenzuchtg 52:127-138.
#'
#' Zali, H., E. Farshadfar, S.H. Sabaghpour, and R. Karimizadeh. 2012.
#' Evaluation of genotype vs environment interaction in chickpea using measures
#' of stability from AMMI model. Ann. Biol. Res. 3:3126-3136.
#'
#' @seealso [ammi_indexes()], [anova_ind()], [anova_joint()], [ecovalence()],
#'   [Fox()], [gai()], [gamem()], [gafem()], [ge_acv()], [ge_polar()]
#'   [ge_means()], [eberhart_russell()], [finlay_wilkinson()], [mgidi()], [mtsi()], [mps()], [mtmps()],
#'   [performs_ammi()], [blup_indexes()], [Shukla()], [lin_binns()], [waas()],
#'   [waasb()]
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @examples
#' \donttest{
#' library(metan)
#'
#'
#' #################### WAASB index #####################
#' # Fitting the WAAS index
#' AMMI <- waasb(data_ge2,
#'               env = ENV,
#'               gen = GEN,
#'               rep = REP,
#'               resp = c(PH, ED, TKW, NKR))
#'
#' # Getting the weighted average of absolute scores
#' gmd(AMMI, what = "WAASB")
#'
#'
#' #################### BLUP model #####################
#' # Fitting a mixed-effect model
#' # Genotype and interaction as random
#' blup <- gamem_met(data_ge2,
#'                   env = ENV,
#'                   gen = GEN,
#'                   rep = REP,
#'                   resp = c(PH, ED))
#'
#' # Getting p-values for likelihood-ratio test
#' gmd(blup, what = "lrt")
#'
#' # Getting the variance components
#' gmd(blup, what = "vcomp")

#'
#'}
#'
get_model_data <- function(x,
                           what = NULL,
                           type = "GEN",
                           verbose = TRUE) {
  call_f <- match.call()

  valid_classes <- c("waasb", "waasb_group", "waas","waas_means", "gamem", "performs_ammi", "ammi",
                     "blup_ind", "ammi_indexes", "ecovalence", "plaisted_peterson", "eberhart_russell", "ge_reg", "Fox", "Shukla",
                     "lin_binns", "superiority", "ge_effects", "gai", "Huehn", "Thennarasu",
                     "ge_stats", "Annicchiarico", "Schmildt", "ge_means", "anova_joint",
                     "gafem", "gafem_group", "gamem_group", "anova_ind", "gge", "can_cor",
                     "can_cor_group", "gytb", "ge_acv", "ge_polar", "mgidi", "mtsi",
                     "env_stratification", "fai_blup", "sh", "mps", "mtmps", "path_coeff",
                     "path_coeff_seq", "group_path_seq", "group_path", "colindiag", "colingroup", "FW")

  if (!has_class(x, valid_classes)) {
    cli::cli_abort("Invalid class in object {call_f[['x']]}. See ?get_model_data for more information.")
  }

  if (!is.null(what) && what != "PCA" && substr(what, 1, 2) == "PC") {
    npc <- ncol(x[[1]][["model"]] |>
                  select(starts_with("PC")) |>
                  select(matches("PC\\d+")))
    npcwhat <- as.numeric(substr(what, 3, nchar(what)))
    if (npcwhat > npc) {
      cli::cli_abort("The number of principal components informed is greater than those in model ({npc}).")
    }
  }

  check <- c("blupg", "blupge","blueg","bluege", "Y", "WAASB", "PctResp", "PctWAASB", "wRes", "wWAASB", "OrResp", "OrWAASB", "OrPC1", "WAASBY", "OrWAASBY", "vcomp", "lrt", "details", "genpar", "ranef", "data", "gcov", "gcor", "pcov", "pcor", "fixed", "h2")
  check1 <- c("Y", "WAAS", "PctResp", "PctWAAS", "wRes", "wWAAS", "OrResp", "OrWAAS", "OrPC1", "WAASY", "OrWAASY")
  check2 <- paste("PC", 1:200, sep = "")
  check3 <- c("blupg", "blupge", "blueg","bluege", "vcomp", "lrt", "genpar", "details", "ranef", "data", "gcov", "gcor", "pcov", "pcor", "fixed")
  check3.1 <- c("h2", "blupg", "blueg", "vcomp", "lrt", "genpar", "details", "ranef", "data", "gcov", "gcor", "pcov", "pcor", "fixed")
  check4 <- c("Y", "WAASB", "PctResp", "PctWAASB", "wRes", "wWAASB", "OrResp", "OrWAASB", "OrPC1", "WAASBY", "OrWAASBY")
  check5 <- c("ipca_ss", "ipca_ms", "ipca_fval", "ipca_pval", "ipca_expl", "ipca_accum")
  check6 <- c("HMGV", "HMGV_R", "RPGV", "RPGV_Y", "RPGV_R", "HMRPGV", "HMRPGV_Y", "HMRPGV_R", "WAASB", "WAASB_R")
  check7 <- c("ASTAB", "ASTAB_R", "ssiASTAB", "ASI", "ASI_R", "ASI_SSI", "ASV", "ASV_R", "ASV_SSI","AVAMGE", "AVAMGE_R","AVAMGE_SSI","DA","DA_R","DA_SSI","DZ","DZ_R","DZ_SSI","EV","EV_R","EV_SSI","FA", "FA_R","FA_SSI","MASI","MASI_R","MASI_SSI","MASV","MASV_R","MASV_SSI","SIPC","SIPC_R","SIPC_SSI", "ZA","ZA_R","ZA_SSI","WAAS","WAAS_R","WAAS_SSI")
  check8 <- c("Ecoval", "Ecov_perc", "rank")
  check8.1 <- c("theta", "theta_prop")
  check9 <- c("GEN", "b0", "b1", "t(b1=1)", "pval_t", "s2di", "F(s2di=0)", "pval_f", "RMSE", "R2", "coefs", "anova")
  check10 <- c("TOP")
  check11 <- c("ShuklaVar", "rMean", "rShukaVar", "ssiShukaVar")
  check12 <- c("Pi_a", "R_a", "Pi_f", "R_f", "Pi_u", "R_u")
  check13 <- c("GAI", "GAI_R")
  check14 <- c("S1","S1_R", "S2", "S2_R", "S3", "S3_R", "S6", "S6_R")
  check15 <- c("N1", "N1_R", "N2", "N2_R", "N3", "N3_R", "N4", "N4_R")
  check16 <- c("stats", "ranks")
  check17 <- c("Mean_rp", "Sd_rp", "Wi", "rank")
  check18 <- c("Mean_rp", "Sem_rp", "Wi", "rank")
  check19 <- c("ge_means", "env_means", "gen_means")
  check20 <- c("Y", "h2", "Sum Sq", "Mean Sq", "F value", "Pr(>F)", "fitted", "resid", "stdres", "se.fit", "details")
  check21 <- c("ALL", "MEAN", "DFG", "MSG", "FCG", "PFG", "DFB", "MSB", "FCB", "PFB", "DFCR", "MSCR", "FCR", "PFCR", "DFIB_R", "MSIB_R", "FCIB_R", "PFIB_R", "DFE", "MSE", "CV", "h2", "AS", "FMAX")
  check22 <- c("scores", "exp_var", "projection")
  check23 <- c("coefs", "loads", "crossloads", "canonical")
  check24 <- c("gyt", "stand_gyt", "si")
  check25 <- c("ACV", "ACV_R")
  check26 <- c("POLAR", "POLAR_R")
  check27 <- c("data", "cormat", "PCA", "FA", "KMO", "MSA", "communalities", "communalities_mean", "initial_loadings", "finish_loadings", "canonical_loadings", "scores_gen", "scores_ide", "gen_ide", "MGIDI", "contri_fac", "contri_fac_rank", "contri_fac_rank_sel", "sel_dif", "stat_gain", "sel_gen")
  check28 <- c("data", "cormat", "PCA", "FA", "KMO", "MSA", "communalities", "communalities_mean", "initial_loadings", "finish_loadings", "canonical_loadings", "scores_gen", "scores_ide", "gen_ide", "MTSI", "contri_fac", "contri_fac_rank", "contri_fac_rank_sel", "sel_dif_trait", "stat_dif_trait", "sel_dif_stab", "stat_dif_stab", "sel_dif_mps", "stat_dif_mps", "sel_gen")
  check29 <- c("FA", "env_strat", "mega_env_stat")
  check30 <- c("data", "eigen", "FA", "canonical_loadings", "FAI", "sel_dif_trait", "sel_gen", "construction_ideotypes")
  check31 <- c("b", "index", "sel_dif_trait", "total_gain", "sel_gen", "gcov", "pcov")
  check32 <- c("observed", "performance", "performance_res", "stability", "stability_res", "mps_ind", "h2", "perf_method", "wmper", "sense_mper", "stab_method", "wstab", "sense_stab")
  check36 <- c("estimates", "predictions", "var_e", "var_e_weighted")
  check33 <- c("coef", "eigenval", "vif")
  check34 <- c("resp_fc", "resp_sc", "resp_sc2", "fc_sc_coef")
  check35 <- c("cormat", "corlist", "evalevet", "VIF", "indicators")

  bind <- NULL

  if(has_class(x, c("colindiag", "colingroup"))){
    if (is.null(what)) what <- "indicators"
    what <- rlang::arg_match(what, values = check35)

    if(has_class(x, "colingroup")){
      bind <- x |>
        mutate(data = map(data, \(d) d[[what]])) |>
        unnest(data)
    } else {
      bind <- x[[what]]
    }
  }

  if(has_class(x, c("path_coeff", "group_path"))){
    if (is.null(what)) what <- "coef"
    what <- rlang::arg_match(what, values = check33)

    what <- switch(what, "coef" = "Coefficients", "eigenval" = "Eigen", "vif" = "vif")
    if(has_class(x, "group_path")){
      bind <- x |>
        mutate(data = map(data, \(d) d[[what]])) |>
        unnest(data)
    } else {
      bind <- x[[what]]
    }
  }

  if (has_class(x, c("path_coeff_seq", "group_path_seq"))){
    if (is.null(what)) what <- "resp_sc2"
    what <- rlang::arg_match(what, values = check34)

    if(has_class(x, "group_path_seq")){
      if(what %in% c("resp_fc", "resp_sc")){
        bind <- x |>
          mutate(data = map(data, \(d) d[[what]][["Coefficients"]])) |>
          unnest(data)
      } else {
        bind <- x |>
          mutate(data = map(data, \(d) d[[what]])) |>
          unnest(data)
      }
    } else {
      if(what %in% c("resp_fc", "resp_sc")){
        bind <- x[[what]][["Coefficients"]]
      } else {
        bind <- x[[what]]
      }
    }
  }

  if (!is.null(what) && what %in% check3 && !has_class(x, c("waasb", "waas", "waasb_group", "gamem", "gamem_group", "gafem", "anova_joint"))) {
    cli::cli_abort("Invalid argument 'what'. It can only be used with an object of class 'waasb' or 'gamem', 'gafem', or 'anova_joint'. Please, check and fix.")
  }
  if (!type %in% c("GEN", "ENV")) {
    cli::cli_abort("Argument 'type' invalid. It must be either 'GEN' or 'ENV'.")
  }

  if(has_class(x, "mps")){
    if (is.null(what)) what <- "mps_ind"
    what <- rlang::arg_match(what, values = check32)

    if(has_class(x, "mps_group")){
      bind <- x |>
        mutate(data = map(data, \(d) d[[what]])) |>
        unnest(data)
    } else {
      bind <- x[[what]]
    }
  }

  if(has_class(x, "sh")){
    if (is.null(what)) what <- "sel_dif_trait"
    what <- rlang::arg_match(what, values = check31)
    bind <- x[[what]]
  }

  if(has_class(x, "fai_blup")){
    if (is.null(what)) what <- "sel_dif_trait"
    what <- rlang::arg_match(what, values = check30)
    bind <- if(what == "sel_dif_trait") x[[what]][[1]] else x[[what]]
  }

  if(has_class(x, "env_stratification")){
    if (is.null(what)) what <- "env_strat"
    what <- rlang::arg_match(what, values = check29)
    bind <- x |> map_dfr(\(m) bind_rows(!!! m[[what]]), .id = 'TRAIT')
  }

  if(has_class(x, c("mtsi", "mtmps"))){
    if (is.null(what)) what <- "sel_dif_trait"
    what <- rlang::arg_match(what, values = check28)
    bind <- x[[what]]
  }

  if(has_class(x, "mgidi")){
    if (is.null(what)) what <- "sel_dif"
    what <- rlang::arg_match(what, values = check27)

    if(has_class(x, "mgidi_group")){
      bind <- x |>
        mutate(data = map(data, \(d) d[[what]])) |>
        unnest(data)
    } else {
      bind <- x[[what]]
    }
  }

  if (has_class(x, "ge_polar")) {
    if (is.null(what)) what <- "POLAR"
    what <- rlang::arg_match(what, values = check26)
    bind <- map(x, \(m) m[[what]]) |>
      as_tibble() |>
      mutate(GEN = x[[1]][["GEN"]]) |>
      column_to_first(GEN)
  }

  if (has_class(x, "ge_acv")) {
    if (is.null(what)) what <- "ACV"
    what <- rlang::arg_match(what, values = check25)
    bind <- map(x, \(m) m[[what]]) |>
      as_tibble() |>
      mutate(GEN = x[[1]][["GEN"]]) |>
      column_to_first(GEN)
  }

  if(has_class(x, "gge") & length(class(x)) == 1){
    if (is.null(what)) what <- "scores"
    what <- rlang::arg_match(what, values = check22)

    if(what == "scores"){
      npc <- length(x[[1]]$varexpl)
      bind <- lapply(x, function(m) {
        rbind(m$coordgen |> as.data.frame() |> set_names(paste0("PC", 1:npc)) |> add_cols(TYPE = "GEN", CODE = m$labelgen, .before = 1),
              m$coordenv |> as.data.frame() |> set_names(paste0("PC", 1:npc)) |> add_cols(TYPE = "ENV", CODE = m$labelenv, .before = 1))
      }) |> rbind_fill_id(.id = "TRAIT")
    }
    if(what == "exp_var"){
      bind <- lapply(x, function(m) {
        tibble(PC = m$labelaxes, Eigenvalue = m$eigenvalues, Variance = m$varexpl, Accumulated = cumsum(Variance))
      }) |> rbind_fill_id(.id = "TRAIT")
    }
    if(what == "projection"){
      bind <- lapply(x, function(m) {
        coord_gen <- m$coordgen[, c(1, 2)]
        coord_env <- m$coordenv[, c(1, 2)]
        med1 <- mean(coord_env[, 1])
        med2 <- mean(coord_env[, 2])
        labgen <- m$labelgen
        x1 <- NULL
        for (i in 1:nrow(m$ge_mat)) {
          xi <- solve(matrix(c(-med2, med1, med1, med2), nrow = 2),
                      matrix(c(0, med2 * coord_gen[i, 2] + med1 * coord_gen[i, 1]), ncol = 1))
          x1 <- rbind(x1, t(xi))
        }
        data.frame(coord_gen, type = "genotype", GEN = labgen) |>
          mutate(x1_x = x1[, 1], x1_y = x1[, 2], PROJECTION = sqrt((x1_x - X1)^2 + (x1_y - X2)^2))
      }) |>
        rbind_fill_id(.id = "TRAIT") |>
        select(TRAIT, GEN, PROJECTION) |>
        arrange(PROJECTION)
    }
  }

  if(has_class(x, "gytb")){
    if (is.null(what)) what <- "gyt"
    what <- rlang::arg_match(what, values = check24)

    if(what == "gyt") bind <- x[["mod"]][["data"]]
    if(what == "stand_gyt") bind <- x[["mod"]][["ge_mat"]]
    if(what == "si"){
      bind <- x[["mod"]][["ge_mat"]] |>
        as.data.frame() |>
        (\(df) add_cols(df, SI = rowSums(df)))() |>
        rownames_to_column("GEN") |>
        select(GEN, SI) |>
        arrange(-SI)
    }
  }

  if(has_class(x, c("can_cor", "can_cor_group"))){
    if (is.null(what)) what <- "coefs"
    what <- rlang::arg_match(what, values = check23)

    fg_what <- case_when(what == "coefs" ~ "Coef_FG", what == "loads" ~ "Loads_FG", what == "crossloads" ~ "Crossload_FG")
    sg_what <- case_when(what == "coefs" ~ "Coef_SG", what == "loads" ~ "Loads_SG", what == "crossloads" ~ "Crossload_SG")

    if(has_class(x, "can_cor_group")){
      npairs <- ncol(x[["data"]][[1]][["Coef_FG"]])
      if(what == "canonical"){
        bind <- x |> mutate(test = map(data, \(d) d[["Sigtest"]])) |> remove_cols(data) |> unnest(test)
      } else {
        bind <- rbind(
          x |> mutate(FG = map(data, \(d) d[[fg_what]] |> as_tibble(rownames = NA) |> set_names(paste0("CP", 1:npairs)) |> rownames_to_column("VAR"))) |> remove_cols(data) |> unnest(FG) |> add_cols(GROUP = "FG", .before = VAR),
          x |> mutate(SG = map(data, \(d) d[[sg_what]] |> as_tibble(rownames = NA) |> set_names(paste0("CP", 1:npairs)) |> rownames_to_column("VAR"))) |> remove_cols(data) |> unnest(SG) |> add_cols(GROUP = "SG", .before = VAR)
        )
      }
    } else {
      npairs <- ncol(x[["Coef_FG"]])
      if(what == "canonical"){
        bind <- x[["Sigtest"]] |> as_tibble(rownames = NA) |> rownames_to_column("GROUP")
      } else {
        bind <- rbind(
          x[[fg_what]] |> as_tibble(rownames = NA) |> set_names(paste0("CP", 1:npairs)) |> rownames_to_column("VAR") |> add_cols(GROUP = "FG", .before = VAR),
          x[[sg_what]] |> as_tibble(rownames = NA) |> set_names(paste0("CP", 1:npairs)) |> rownames_to_column("VAR") |> add_cols(GROUP = "SG", .before = VAR)
        )
      }
    }
  }

  if (has_class(x, c("waasb", "waasb_group", "gamem", "gamem_group"))) {
    if (is.null(what)) what <- "genpar"

    if(has_class(x, c("gamem_group", "waasb_group"))){
      bind <- x |>
        mutate(bind = map(data, \(d) gmd(d, what = what, verbose = verbose))) |>
        unnest(bind) |>
        remove_cols(data)
    } else {
      if(is.null(x[[1]][["ESTIMATES"]]) && what %in% c("genpar", "gcov", "gcor", "h2")){
        cli::cli_warn("Using what = '{what}' is only possible for models fitted with random = 'gen' or random = 'all'\nSetting what to 'vcomp'.")
        what <- "vcomp"
      }
      if(has_class(x, "gamem")) what <- rlang::arg_match(what, values = check3.1)
      if(has_class(x, "waasb")) what <- rlang::arg_match(what, values = check)

      if (has_class(x, "waasb") && what %in% check4) {
        bind <- map(x, \(m) m$model[[what]]) |>
          as_tibble() |>
          mutate(GEN = x[[1]][["model"]][["Code"]], TYPE = x[[1]][["model"]][["type"]]) |>
          dplyr::filter(TYPE == {{type}}) |>
          remove_cols(TYPE) |>
          column_to_first(GEN)
      }
      if(what == "h2"){
        bind <- gmd(x, verbose = FALSE) |>
          subset(Parameters == "h2mg") |>
          remove_cols(1) |>
          t() |> as.data.frame() |>
          rownames_to_column("VAR") |>
          set_names("VAR", "h2")
      }
      if (what == "data") {
        bind <- map(x, \(m) m[["residuals"]] |> dplyr::select(1:Y)) |>
          rbind_fill_id(.id = "VAR") |>
          pivot_wider(names_from = VAR, values_from = Y, values_fn = mean)
      }
      if (what == "gcov") {
        data <- gmd(x, "data", verbose = FALSE)
        if(ncol(select_numeric_cols(data)) < 2) cli::cli_abort("Only one numeric variable. No matrix generated.")

        fctrs <- names(select_non_numeric_cols(data))
        formula <- x[[1]][["formula"]] |> replace_string(pattern = "Y", replacement = "value") |> as.formula()

        gvar <- data |>
          pivot_longer(-all_of(fctrs)) |>
          group_by(name) |>
          doo(\(d) VarCorr(lmer(formula, data = d))) |>
          mutate(data = as.numeric(map(data, \(d) d[["GEN"]])))

        factors <- select_non_numeric_cols(data)
        combined_vars <- comb_vars(data, verbose = FALSE)

        gcov_df <- cbind(factors, combined_vars) |>
          pivot_longer(-all_of(fctrs)) |>
          group_by(name) |>
          doo(\(d) VarCorr(lmer(formula, data = d))) |>
          mutate(data = as.numeric(map(data, \(d) d[["GEN"]]))) |>
          separate(name, into = c("v1", "v2"), sep = "x") |>
          left_join(gvar, by = c("v1" = "name")) |>
          left_join(gvar, by = c("v2" = "name")) |>
          mutate(gcov = (data.x - data.y - data) / 2)

        gcov_mat <- diag(gvar$data, nrow = length(gvar$data), ncol = length(gvar$data))
        colnames(gcov_mat) <- rownames(gcov_mat) <- gvar$name

        for (i in 1:nrow(gcov_df)){
          gcov_mat[which(rownames(gcov_mat) == as.character(gcov_df[i, 1])),
                   which(colnames(gcov_mat) == as.character(gcov_df[i, 2]))] <- pull(gcov_df[i, 6])
        }
        for(i in 1:nrow(gcov_mat)){
          for(j in 1:ncol(gcov_mat)){
            gcov_mat[i, j] <- if(gcov_mat[i, j] == 0) gcov_mat[j, i] else gcov_mat[i, j]
          }
        }
        bind <- make_sym(gcov_mat, diag = diag(gcov_mat), make = "lower")
        bind <- bind[names(x), names(x)]
      }
      if (what == "gcor") {
        gcov <- gmd(x, "gcov", verbose = FALSE)
        bind <- matrix(NA, nrow = nrow(gcov), ncol = ncol(gcov))
        for(i in 1:nrow(gcov)){
          for(j in 1:ncol(gcov)){
            if(i != j) bind[i, j] <- gcov[i, j] / sqrt(gcov[i, i] * gcov[j, j])
          }
        }
        diag(bind) <- 1
        rownames(bind) <- colnames(bind) <- rownames(gcov)
      }
      if (what == "pcov") {
        data <- gmd(x, "data", verbose = FALSE)
        if(ncol(select_numeric_cols(data)) < 2) cli::cli_abort("Only one numeric variable. No matrix generated.")
        bind <- data |> mean_by(GEN) |> remove_cols(GEN) |> cov()
      }
      if (what == "pcor") {
        pcov <- gmd(x, "pcov", verbose = FALSE)
        bind <- matrix(NA, nrow = nrow(pcov), ncol = ncol(pcov))
        for(i in 1:nrow(pcov)){
          for(j in 1:ncol(pcov)){
            if(i != j) bind[i, j] <- pcov[i, j] / sqrt(pcov[i, i] * pcov[j, j])
          }
        }
        diag(bind) <- 1
        rownames(bind) <- colnames(bind) <- rownames(pcov)
      }
      if (what == "fixed"){
        temps <- lapply(seq_along(x), function(i) {
          x[[i]][["fixed"]] |> add_cols(VAR = names(x)[i]) |> column_to_first(VAR)
        })
        names(temps) <- names(x)
        bind <- temps |> reduce(full_join, by = names(temps[[1]]))
      }
      if (what == "vcomp") {
        bind <- map(x, \(m) m[["random"]][["Variance"]]) |>
          as_tibble() |>
          mutate(Group = x[[1]][["random"]][["Group"]]) |>
          column_to_first(Group)
      }
      if (what == "genpar") {
        bind <- map(x, \(m) m[["ESTIMATES"]][["Values"]]) |>
          as_tibble() |>
          mutate(Parameters = x[[1]][["ESTIMATES"]][["Parameters"]]) |>
          column_to_first(Parameters)
      }
      if (what == "details") {
        bind <- map(x, \(m) as.character(m[["Details"]][["Values"]])) |>
          as_tibble() |>
          mutate(Parameters = x[[1]][["Details"]][["Parameters"]]) |>
          column_to_first(Parameters)
      }
      if (what == "lrt") {
        temps <- lapply(seq_along(x), function(i) {
          x[[i]][["LRT"]] |> remove_rows_na(verbose = FALSE) |> add_cols(VAR = names(x)[i]) |> column_to_first(VAR)
        })
        names(temps) <- names(x)
        bind <- temps |> reduce(full_join, by = names(temps[[1]]))
      }
      if (what %in% c("blupg", "blupge", "blueg", "bluege")) {
        if (what == "blupg") {
          list_m <- lapply(x, \(m) m[["BLUPgen"]] |> select(GEN, Predicted))
          bind <- suppressWarnings(
            lapply(seq_along(list_m), \(i) set_names(list_m[[i]], "GEN", names(list_m)[i])) |>
              reduce(full_join, by = "GEN") |> arrange(GEN)
          )
        }
        if (what == "blupge") {
          list_m <- lapply(x, \(m) m[["residuals"]] |> mean_by(ENV, GEN) |> dplyr::select(ENV, GEN, .fitted))
          bind <- suppressWarnings(
            lapply(seq_along(list_m), \(i) set_names(list_m[[i]], "ENV", "GEN", names(list_m)[i])) |>
              reduce(full_join, by = c("ENV", "GEN")) |> arrange(ENV, GEN)
          )
        }
        if (what == "blueg") {
          list_m <- lapply(x, \(m) m[["residuals_lm"]] |> select(GEN, .fitted) |> mean_by(GEN))
          bind <- suppressWarnings(
            lapply(seq_along(list_m), \(i) set_names(list_m[[i]], "GEN", names(list_m)[i])) |>
              reduce(full_join, by = "GEN") |> arrange(GEN)
          )
        }
        if (what == "bluege") {
          list_m <- lapply(x, \(m) m[["residuals_lm"]] |> select(ENV, GEN, .fitted) |> mean_by(ENV, GEN))
          bind <- suppressWarnings(
            lapply(seq_along(list_m), \(i) set_names(list_m[[i]], "ENV", "GEN", names(list_m)[i])) |>
              reduce(full_join, by = c("ENV", "GEN")) |> arrange(ENV, GEN)
          )
        }
      }
      if (what == "ranef") {
        dfs <- lapply(x, function(m){
          int <- if(has_class(m, "waasb")) m[["BLUPint"]] else m[["ranef"]]
          factors <- int |> select_non_numeric_cols()
          numeric <- int |> dplyr::select(contains("BLUP"))
          df_list2 <- list()
          for (i in seq_len(ncol(numeric))){
            temp <- cbind(factors, numeric[i])
            var_names <- strsplit(case_when(names(temp)[ncol(factors)+1] == "BLUPg" ~ "GEN",
                                            names(temp)[ncol(factors)+1] == "BLUPe" ~ "ENV",
                                            names(temp)[ncol(factors)+1] == "BLUPge" ~ "ENV GEN",
                                            names(temp)[ncol(factors)+1] == "BLUPre" ~ "ENV REP",
                                            names(temp)[ncol(factors)+1] == "BLUPg+ge" ~ "ENV GEN",
                                            names(temp)[ncol(factors)+1] == "BLUPbre" ~ "REP BLOCK",
                                            names(temp)[ncol(factors)+1] == "BLUPg+bre" ~ "GEN REP BLOCK",
                                            names(temp)[ncol(factors)+1] == "BLUPg+ge+bre" ~ "ENV REP BLOCK GEN",
                                            names(temp)[ncol(factors)+1] == "BLUPe+ge+re+bre" ~ "ENV REP BLOCK GEN",
                                            names(temp)[ncol(factors)+1] == "BLUPg+e+ge+re+bre" ~ "ENV REP BLOCK GEN",
                                            names(temp)[ncol(factors)+1] == "BLUPg+e+ge+re" ~ "ENV GEN REP",
                                            names(temp)[ncol(factors)+1] == "BLUPge+e+re" ~ "ENV GEN REP"),
                                  " ")[[1]]
            temp <- temp |> select(all_of(var_names), last_col()) |> distinct_all(.keep_all = TRUE)
            fact_nam <- paste(sapply(colnames(temp |> select_non_numeric_cols()), paste), collapse = '_')
            df_list2[[paste(fact_nam)]] <- temp
          }
          return(df_list2)
        })
        nvcomp <- length(dfs[[1]])
        bind <- list()
        for(i in 1:nvcomp){
          var_names <- names(dfs[[1]][[i]] |> select_non_numeric_cols())
          num <- lapply(seq_along(dfs), \(j) set_names(dfs[[j]][[i]], var_names, names(dfs)[j])) |>
            reduce(full_join, by = var_names) |>
            arrange(across(where(~!is.numeric(.x))))
          bind[[names(dfs[[1]])[i]]] <- num
        }
      }
    }
  }

  if (has_class(x, "anova_ind")) {
    if (is.null(what)) what <- "ALL"
    what <- rlang::arg_match(what, values = check21)

    if(what == "ALL"){
      bind <- lapply(x, \(m) m[[1]]) |> rbind_fill_id(.id = "trait")
    } else {
      if(what == "FMAX"){
        bind <- map(x, \(m) m[["MSRratio"]]) |> as.data.frame() |> rownames_to_column("TRAIT") |> setNames(c("TRAIT", "F_RATIO"))
      } else {
        bind <- map(x, \(m) m[["individual"]][[what]]) |> as_tibble() |> mutate(ENV = x[[1]][["individual"]][["ENV"]]) |> column_to_first(ENV)
      }
    }
  }

  if (has_class(x, c("anova_joint", "gafem", "gafem_group"))) {
    if(has_class(x, "gafem_group")){
      bind <- x |> mutate(bind = map(data, \(d) gmd(d, what = what, verbose = verbose))) |> unnest(bind) |> remove_cols(data)
    } else {
      if (is.null(what)) what <- "fitted"
      what <- rlang::arg_match(what, values = check20)

      if(what %in% c("Sum Sq", "Mean Sq", "F value", "Pr(>F)")){
        bind <- map(x, \(m) m[["anova"]][[what]]) |> as_tibble()
        bind <- cbind(x[[1]][["anova"]] |> select_non_numeric_cols(), bind) |> remove_rows_na(verbose = FALSE)
      }
      if(what == "h2"){
        bind <- map(x, function(m){
          MSG <- as.numeric(m[["anova"]][which(m[["anova"]][["Source"]] == "GEN"), 4])
          MSE <- as.numeric(m[["anova"]][which(m[["anova"]][["Source"]] == "Residuals"), 4])
          (MSG - MSE) / MSG
        }) |> as.data.frame() |> rownames_to_column("VAR") |> set_names("VAR", "h2")
      }
      if(what %in% c("Y", "fitted", "resid", "stdres", "se.fit")){
        bind <- map(x, \(m) m[["augment"]][[what]]) |> as_tibble()
        bind <- cbind(x[[1]][["augment"]] |> select_non_numeric_cols(), bind) |> as_tibble()
      }
      if(what == "details"){
        bind <- map(x, \(m) m[["details"]][[2]]) |> as_tibble() |> mutate(Parameters = x[[1]][["details"]][["Parameters"]]) |> column_to_first(Parameters)
      }
    }
  }

  if(has_class(x, "ge_means")){
    if (is.null(what)) what <- "ge_means"
    what <- rlang::arg_match(what, values = check19)

    if(what == "ge_means"){
      bind <- map(x, \(m) m[["ge_means_long"]][["Mean"]]) |> as_tibble() |> add_cols(ENV = x[[1]][["ge_means_long"]][["ENV"]], GEN = x[[1]][["ge_means_long"]][["GEN"]]) |> column_to_first(ENV, GEN)
    }
    if(what == "env_means"){
      bind <- map(x, \(m) m[["env_means"]][["Mean"]]) |> as_tibble() |> add_cols(ENV = x[[1]][["env_means"]][["ENV"]]) |> column_to_first(ENV)
    }
    if(what == "gen_means"){
      bind <- map(x, \(m) m[["gen_means"]][["Mean"]]) |> as_tibble() |> add_cols(GEN = x[[1]][["gen_means"]][["GEN"]]) |> column_to_first(GEN)
    }
  }

  if (has_class(x, "Annicchiarico")) {
    if (is.null(what)) what <- "Wi"
    what <- rlang::arg_match(what, values = check17)
    bind <- map(x, \(m) m[["general"]][[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["general"]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "Schmildt")) {
    if (is.null(what)) what <- "Wi"
    what <- rlang::arg_match(what, values = check18)
    bind <- map(x, \(m) m[["general"]][[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["general"]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "ge_stats")) {
    if (is.null(what)) what <- "stats"
    what <- rlang::arg_match(what, values = check16)

    bind <- do.call(cbind, lapply(x, function(m) {
      if(what == "stats") m |> select(-contains("_R"), -contains("GEN")) else m |> select(contains("_R"))
    })) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> pivot_longer(cols = contains(".")) |> separate(name, into = c("var", "stat"), sep = "\\.") |> pivot_wider(values_from = value, names_from = stat) |> column_to_first(var) |> arrange(var)
  }

  if (has_class(x, "Thennarasu")) {
    if (is.null(what)) what <- "N1"
    what <- rlang::arg_match(what, values = check15)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "Huehn")) {
    if (is.null(what)) what <- "S1"
    what <- rlang::arg_match(what, values = check14)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "gai")) {
    if (is.null(what)) what <- "GAI"
    what <- rlang::arg_match(what, values = check13)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "ge_effects")) {
    bind <- map(x, \(m) make_long(m)[[3]]) |> as_tibble()
    factors <- x[[1]] |> make_long() |> select(1:2)
    bind <- cbind(factors, bind)
  }

  if (has_class(x, "superiority") || has_class(x, "lin_binns")) {
    if (is.null(what)) what <- "Pi_a"
    what <- rlang::arg_match(what, values = check12)
    bind <- map(x, \(m) m[["index"]][[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["index"]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "Shukla")) {
    if (is.null(what)) what <- "ShuklaVar"
    what <- rlang::arg_match(what, values = check11)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "Fox")) {
    if (is.null(what)) what <- "TOP"
    what <- rlang::arg_match(what, values = check10)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "ge_reg") || has_class(x, "eberhart_russell")) {
    if (is.null(what)) what <- "b1"
    what <- rlang::arg_match(what, values = check9)

    if(what %in% c("coefs", "anova")){
      if(what == "coefs"){
        bind <- lapply(x, \(m) m$regression) |> rbind_fill_id(.id = "TRAIT")
      } else {
        bind <- lapply(x, \(m) m$anova) |> rbind_fill_id(.id = "TRAIT")
      }
    } else {
      bind <- map(x, \(m) m[["regression"]][[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["regression"]][["GEN"]]) |> column_to_first(GEN)
    }
  }

  if (has_class(x, "ecovalence")) {
    if (is.null(what)) what <- "Ecoval"
    what <- rlang::arg_match(what, values = check8)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "plaisted_peterson")) {
    if (is.null(what)) what <- "theta"
    what <- rlang::arg_match(what, values = check8.1)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "ammi_indexes")) {
    if (is.null(what)) what <- "WAAS"
    what <- rlang::arg_match(what, values = check7)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, "blup_ind")) {
    if (is.null(what)) what <- "HMRPGV"
    what <- rlang::arg_match(what, values = check6)
    bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["GEN"]]) |> column_to_first(GEN)
  }

  if (has_class(x, c("performs_ammi", "ammi"))) {
    if (is.null(what)) what <- "ipca_expl"
    what <- rlang::arg_match(what, values = c("Y", check2, check5))

    if (what == "Y" || what %in% check2) {
      bind <- map(x, \(m) m$model[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["model"]][["Code"]], TYPE = x[[1]][["model"]][["type"]]) |> dplyr::filter(TYPE == {{type}}) |> remove_cols(TYPE) |> column_to_first(GEN)
    }
    if (what %in% check5) {
      what_mapped <- case_when(what == "ipca_ss" ~ "Sum Sq", what == "ipca_ms" ~ "Mean Sq", what == "ipca_fval" ~ "F value", what == "ipca_pval" ~ "Pr(>F)", what == "ipca_expl" ~ "Proportion", what == "ipca_accum" ~ "Accumulated")
      bind <- map(x, \(m) m[["PCA"]][[what_mapped]]) |> as_tibble() |> mutate(PC = x[[1]][["PCA"]][["PC"]], DF = x[[1]][["PCA"]][["Df"]]) |> column_to_first(PC, DF)
    }
  }

  if (has_class(x, c("waas", "waas_means"))){
    if (is.null(what)) what <- "WAAS"
    what <- rlang::arg_match(what, values = c("details", check1, check2))

    if (what == "details") {
      bind <- map(x, \(m) as.character(m[["Details"]][["Values"]])) |> as_tibble() |> mutate(Parameters = x[[1]][["Details"]][["Parameters"]]) |> column_to_first(Parameters)
    }
    if (what %in% check1 || what %in% check2) {
      bind <- map(x, \(m) m$model[[what]]) |> as_tibble() |> mutate(GEN = x[[1]][["model"]][["Code"]], TYPE = x[[1]][["model"]][["type"]]) |> dplyr::filter(TYPE == {{type}}) |> remove_cols(TYPE) |> column_to_first(GEN)
    }
  }

  if (has_class(x, "FW")) {
    if (is.null(what)) what <- "estimates"
    what <- rlang::arg_match(what, values = check36)

    if (what %in% c("estimates", "predictions")) {
        bind <- lapply(x, \(m) m[[what]]) |> rbind_fill_id(.id = "TRAIT")
    } else if (what == "var_e") {
        bind <- map(x, \(m) m[[what]]) |> as_tibble() |> mutate(GEN = names(x[[1]][[what]])) |> column_to_first(GEN)
    } else if (what == "var_e_weighted") {
        bind <- map(x, \(m) m[[what]]) |> as_tibble()
    }
  }

  if(verbose){
    cli::cli_inform("Class of the model: {.val {class(x)}}")
    cli::cli_inform("Variable extracted: {what}")
  }

  return(bind)
}

#' @name get_model_data
#' @export
gmd <- function(x,
                what = NULL,
                type = "GEN",
                verbose = TRUE){
  get_model_data(x, what, type, verbose)
}

#' @name get_model_data
#' @export
sel_gen <- function(x){
  gmd(x, "sel_gen", verbose = FALSE)
}
