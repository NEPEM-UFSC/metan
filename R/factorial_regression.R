#' Factorial Regression for Genotype-by-Environment Interaction
#'
#' @description
#' `r badge('stable')`
#'
#' Fits a parametric factorial regression model to partition Genotype-by-Environment (GxE)
#' interaction variance using explicit environmental covariates. If the number of covariates
#' exceeds the degrees of freedom threshold determined by the number of environments, an
#' automated Principal Component Analysis (PCA) is executed to reduce dimensionality
#' and project the environmental space into independent orthogonal component scores.
#'
#' @details
#' Factorial regression opens the black box of structural GxE interactions by integrating
#' physical, climatic, or edaphic external covariates directly into the linear modeling framework.
#' The total GxE Sum of Squares (\eqn{SS_{G\times E}}) is partitioned via a Type-I sequential
#' analysis of variance (ANOVA) to quantify the precise proportion of interaction variation
#' explained by each individual covariate.
#'
#' \subsection{The Mathematical Model}{
#' The phenotypic performance of genotype \eqn{i} in environment \eqn{j} within replication/block \eqn{k}
#' is structured as follows:
#'
#' \deqn{Y_{ijk} = \mu + e_j + b_{k(j)} + g_i + \sum_{m=1}^{M} (\gamma_{im} Z_{jm}) + d_{ij} + \varepsilon_{ijk}}{Y_{ijk} = \mu + e_j + b_{k(j)} + g_i + \sum (\gamma_{im} Z_{jm}) + d_{ij} + \varepsilon_{ijk}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{Y_{ijk}} is the observed phenotypic value of genotype \eqn{i} in environment \eqn{j}, block \eqn{k}.
#'   \item \eqn{\mu} is the grand mean of the multi-environment trial network.
#'   \item \eqn{e_j} is the main environmental effect of site \eqn{j}.
#'   \item \eqn{b_{k(j)}} is the localized blocking effect nested within environment \eqn{j} (omitted if \code{rep = NULL}).
#'   \item \eqn{g_i} is the main additive genetic effect of genotype \eqn{i}.
#'   \item \eqn{Z_{jm}} is the value of the \eqn{m}-th environmental covariate (or the \eqn{m}-th Principal Component score) captured in environment \eqn{j}.
#'   \item \eqn{\gamma_{im}} is the genotypic slope (sensitivity coefficient) of genotype \eqn{i} responding to covariate \eqn{m}.
#'   \item \eqn{d_{ij}} is the residual, unexplained GxE interaction deviation for genotype \eqn{i} in environment \eqn{j}.
#'   \item \eqn{\varepsilon_{ijk}} is the random experimental plot error.
#' }
#' }
#'
#' \subsection{Automated Dimensionality Reduction (PCA Mitigator)}{
#' To safeguard the model against column rank deficiencies, overparameterization, and structural
#' singularity, the maximum number of simultaneously fitted covariates is constrained to \eqn{M = E - 2},
#' where \eqn{E} is the total number of unique environments.
#' If the length of selected covariates exceeds this limit, a singular value decomposition (PCA)
#' is automatically run on the centered and scaled covariate submatrix:
#' \deqn{\mathbf{Z} = \mathbf{X}\mathbf{V}}{\mathbf{Z} = \mathbf{X}\mathbf{V}}
#' The top \eqn{E - 2} Principal Components (\eqn{\mathbf{Z}}) are then extracted as orthogonal synthetic
#' environmental indexes to drive both global ANOVA partitioning and individual slope evaluations.
#' }
#'
#' @param .data A \code{data.frame} or \code{tibble} containing phenotypic trial observations.
#' @param env The unquoted column name containing the levels of the environments. Handles character or factor variables.
#' @param gen The unquoted column name containing the levels of the genotypes. Handles character or factor variables.
#' @param resp Tidyselect specification for phenotypic trait columns to analyze (e.g., \code{c(grain_yield, plant_height)}, \code{everything()}, or \code{contains("yield")}).
#' @param cov_data A separate \code{data.frame} or \code{tibble} containing site-specific environmental characteristics. Must contain a column matching the name supplied to \code{env}.
#' @param cov_vars Tidyselect specification for environmental covariate columns to extract from \code{cov_data}. Defaults to \code{everything()}.
#' @param rep The unquoted column name containing the levels of replications or blocks. Defaults to \code{NULL} for un-replicated trials or pre-aggregated data cells.
#' @param select_covar Logical. If \code{TRUE} and the number of covariates
#'   exceeds \code{E-2}, tests all covariates iteratively in a forward stepwise
#'   procedure to find the subset that maximizes the explained
#'   Genotype-by-Environment (GxE) interaction Sum of Squares, bypassing PCA.
#'   Defaults to \code{FALSE}.
#' @param collinear_threshold Numeric between 0 and 1. The maximum absolute Pearson
#'   correlation allowed between a candidate covariate and already selected
#'   covariates in the forward stepwise procedure. Defaults to 0.9.
#' @param scale_covars Logical. If \code{TRUE}, all selected covariates are
#'   standardized (mean = 0, standard deviation = 1) before model fitting.
#'   Defaults to \code{FALSE}.
#' @param x An object of class \code{factorial_reg}.
#' @param which Type of plot: \code{"contribution"} (1) or \code{"coefficients"} (2).
#' @param trait Specific trait name to plot (relevant for \code{"coefficients"} or multi-trait visualizations).
#' @param covariate Tidyselect specification for the covariate(s) to show in the coefficients plot. Defaults to \code{NULL} (all covariates).
#' @param color_palette Custom vector of colors for plot customization.
#' @param error_bars Logical. If \code{TRUE}, plots 95 percent confidence intervals on individual genotypic coefficients.
#' @param ncol The number of columns in the plot layout when displaying multiple panels.
#' @param ... Additional arguments passed to generic plot methods.
#' @return A list object of class \code{factorial_reg} containing consolidated dataframes:
#'   * \code{gxe_contribution}: Percent variance explained by each active covariate/PC across traits.
#'   * \code{gxe_coefficients}: Independent per-genotype regression metrics (estimate, std.error, t-value, p-value).
#'   * \code{anova_tables}: The underlying sequential Type-I global ANOVA matrices per trait.
#'   * \code{pca_results}: Structured list containing PC scores, variable loadings, eigenvalues, and variance explanation percentages if the auto-PCA pathway was triggered.
#'   * \code{stepwise_results}: Summary of tested covariate combinations and their total explained SS if \code{select_covar = TRUE} was used.
#' @md
#'
#' @author Tiago Olivoto \email{tiago.olivoto@@ufsc.br}
#'
#' @aliases factorial_reg plot.factorial_gxe
#' @rdname factorial_reg
#' @export
#'
#' @examples
#' \dontrun{
#' library(metan)
#' library(dplyr)
#' library(ggplot2)
#'
#' # 1. Simulate a multi-environment trial (5 environments, 10 genotypes, 3 blocks)
#' set.seed(42)
#' df_trial <- expand.grid(
#'   env   = paste0("Env", 1:5),
#'   gen   = paste0("Gen", 1:10),
#'   block = paste0("B", 1:3)
#' ) |>
#'   mutate(
#'     grain_yield  = rnorm(n(), mean = 65, sd = 12),
#'     plant_height = rnorm(n(), mean = 110, sd = 15)
#'   )
#'
#' # Simulate 5 weather covariates (triggers auto-PCA since covariates > env - 2)
#' df_weather <- data.frame(
#'   env             = paste0("Env", 1:5),
#'   max_temperature = c(34.2, 28.1, 31.5, 35.0, 26.4),
#'   min_temperature = c(18.5, 14.2, 16.0, 20.1, 12.8),
#'   total_rainfall  = c(120,  450,  210,  95,   580),
#'   solar_radiation = c(24.5, 18.2, 21.0, 26.1, 14.8),
#'   relative_humid  = c(62,   88,   71,   55,   92)
#' )
#'
#' # 2. Run Multi-Trait Analysis
#' multi_results <- factorial_reg(
#'   df_ge  = df_trial,
#'   df_cov = df_weather,
#'   resp   = c(grain_yield, plant_height)
#' )
#'
#' # S3 Plot: Multi-trait 100% stacked bar chart
#' plot(multi_results, which = "contribution")
#'
#' # 3. Run Single-Trait Analysis Focus
#' single_results <- factorial_reg(
#'   df_ge  = df_trial,
#'   df_cov = df_weather,
#'   resp   = grain_yield
#' )
#'
#' # S3 Plot: Single trait horizontal contribution chart
#' plot(single_results, which = "contribution", trait = "grain_yield")
#'
#' # S3 Plot: Absolute Genotypic Slopes with 95% Confidence Intervals
#' plot(single_results, which = "coefficients", trait = "grain_yield", error_bars = TRUE)
#' }
factorial_reg <- function(.data, env, gen, resp, cov_data, cov_vars = everything(), rep = NULL, select_covar = FALSE, collinear_threshold = 0.9, scale_covars = FALSE) {

  # 1. Standardize and isolate core variables using tidy evaluation
  env_var  <- rlang::ensym(env)
  gen_var  <- rlang::ensym(gen)
  rep_var  <- if (!missing(rep) && !is.null(substitute(rep))) rlang::ensym(rep) else NULL

  # Validate column presence in main dataset
  col_names <- names(.data)
  if (!as.character(env_var) %in% col_names) cli::cli_abort("Column {.var {env_var}} not found in your dataset.")
  if (!as.character(gen_var) %in% col_names) cli::cli_abort("Column {.var {gen_var}} not found in your dataset.")
  if (!is.null(rep_var) && !as.character(rep_var) %in% col_names) {
    cli::cli_abort("Replication column {.var {rep_var}} not found in your dataset.")
  }

  # Validate column presence in covariate dataset
  if (!as.character(env_var) %in% names(cov_data)) {
    cli::cli_abort("The covariate dataset ({.arg cov_data}) must contain the matching environmental column: {.var {env_var}}.")
  }

  # 2. Extract selected trait and covariate names using tidyselect
  trait_names <- .data    |> dplyr::select({{ resp }})     |> names()
  cov_names   <- cov_data |> dplyr::select({{ cov_vars }}) |> names()

  # Always ensure the environment tracker label is stripped from covariates vector
  cov_names <- setdiff(cov_names, as.character(env_var))

  if (length(trait_names) == 0) cli::cli_abort("No traits selected. Check your {.arg resp} specification.")
  if (length(cov_names) == 0)   cli::cli_abort("No environmental covariates selected. Check your {.arg cov_vars} specification.")

  if (scale_covars) {
    cov_data <- cov_data |>
      dplyr::mutate(dplyr::across(dplyr::all_of(cov_names), ~as.numeric(scale(.x))))
  }

  # 3. Covariate and Dimensionality Check (AUTO-PCA LOGIC)
  n_env <- length(unique(cov_data[[as.character(env_var)]]))
  if (n_env < 3) {
    cli::cli_abort("You need at least 3 environments to partition GxE variance using this method.")
  }

  max_covs   <- n_env - 2
  pca_output <- NULL
  stepwise_results <- NULL

  # We need the merged data early if select_covar is TRUE to run combinatorial models
  df_model <- .data |>
    dplyr::inner_join(cov_data, by = as.character(env_var)) |>
    dplyr::rename(ENV = !!env_var, GEN = !!gen_var) |>
    dplyr::mutate(ENV = as.factor(ENV), GEN = as.factor(GEN))

  has_rep <- !is.null(rep_var)
  if (has_rep) {
    df_model <- df_model |>
      dplyr::rename(REP = !!rep_var) |>
      dplyr::mutate(REP = as.factor(REP))
  }

  if (length(cov_names) > max_covs) {
    if (!select_covar) {
      cli::cli_alert_info("Notice: {.val {length(cov_names)}} covariates provided, but only {.val {max_covs}} can be fitted with {.val {n_env}} environments to prevent singularity.")
      cli::cli_alert_info("{.bd Automatically running PCA and extracting maximum allowable Principal Components...}")

      cov_data_matrix <- cov_data |> dplyr::select(dplyr::all_of(cov_names))
      pca_res  <- stats::prcomp(cov_data_matrix, scale. = TRUE)

      pc_names <- paste0("PC", 1:max_covs)
      df_pca   <- as.data.frame(pca_res$x[, 1:max_covs, drop = FALSE])
      names(df_pca) <- pc_names

      pca_output <- list(
        scores      = dplyr::bind_cols(cov_data |> dplyr::select(!!env_var), df_pca),
        loadings    = as.data.frame(pca_res$rotation[, 1:max_covs, drop = FALSE]) |> tibble::rownames_to_column("variable"),
        eigenvalues = pca_res$sdev[1:max_covs]^2,
        variance_explained = (pca_res$sdev^2 / sum(pca_res$sdev^2))[1:max_covs] * 100
      )

      # Update cov_names and merge the new PC columns back into df_model
      cov_names <- pc_names
      df_model <- df_model |>
        dplyr::select(-dplyr::all_of(names(cov_data_matrix))) |>
        dplyr::inner_join(pca_output$scores, by = c("ENV" = as.character(env_var)))

      cli::cli_alert_success("PCA completed successfully.")
    } else {
      # Forward Stepwise Selection Logic
      cli::cli_alert_info("Stepwise procedure: Forward selection of up to {.val {max_covs}} covariates to maximize GxE explanation...")

      base_effects <- if (has_rep) c("ENV", "ENV:REP", "GEN") else c("ENV", "GEN")
      residual_gxe <- "ENV:GEN"

      remaining_covs <- cov_names
      selected_covs <- character(0)
      stepwise_history <- list()

      pb <- cli::cli_progress_bar(
        format = "{cli::pb_spin} Selecting covariates | {cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}] | ETA: {cli::pb_eta}",
        total = max_covs
      )

      cov_data_matrix <- df_model |> dplyr::select(dplyr::all_of(cov_names)) |> dplyr::select_if(is.numeric)

      for (step in 1:max_covs) {
        if (length(remaining_covs) == 0) break

        # Check collinearity for steps > 1
        if (length(selected_covs) > 0) {
          valid_candidates <- character(0)
          for (cand in remaining_covs) {
            # Compute correlation with already selected covariates
            cor_vals <- abs(stats::cor(cov_data_matrix[[cand]], cov_data_matrix[, selected_covs, drop = FALSE], use = "pairwise.complete.obs"))
            if (max(cor_vals, na.rm = TRUE) < collinear_threshold) {
              valid_candidates <- c(valid_candidates, cand)
            }
          }
          remaining_covs <- valid_candidates
        }

        if (length(remaining_covs) == 0) {
          cli::cli_alert_warning("No more non-collinear covariates available. Stopping at {.val {length(selected_covs)}} covariates.")
          break
        }

        best_cand <- NULL
        best_ss <- -Inf

        for (cand in remaining_covs) {
          test_combo <- c(selected_covs, cand)
          interaction_effects <- paste0("GEN:", test_combo)
          rhs <- paste(c(base_effects, interaction_effects, residual_gxe), collapse = " + ")

          total_ss <- 0
          for (trait in trait_names) {
            formula_str <- paste(trait, "~", rhs)
            mod <- stats::lm(stats::as.formula(formula_str), data = df_model)
            aov_res <- stats::anova(mod)
            ss_covs <- sum(aov_res[rownames(aov_res) %in% interaction_effects, "Sum Sq"])
            total_ss <- total_ss + ss_covs
          }

          if (total_ss > best_ss) {
            best_ss <- total_ss
            best_cand <- cand
          }
        }

        selected_covs <- c(selected_covs, best_cand)
        remaining_covs <- setdiff(remaining_covs, best_cand)
        stepwise_history[[step]] <- data.frame(
          step = step,
          added_covariate = best_cand,
          total_explained_ss = best_ss
        )
        cli::cli_progress_update(id = pb)
      }
      cli::cli_progress_done(id = pb)

      stepwise_results <- dplyr::bind_rows(stepwise_history)
      cov_names <- selected_covs
      cli::cli_alert_success("Stepwise selection complete. Selected {.val {length(cov_names)}} optimal covariates: {.val {cov_names}}")
    }
  }

  # 5. Build dynamic linear model formula terms
  base_effects        <- if (has_rep) c("ENV", "ENV:REP", "GEN") else c("ENV", "GEN")
  interaction_effects <- paste0("GEN:", cov_names)
  residual_gxe        <- "ENV:GEN"

  # 6. Process each selected trait
  individual_results <- purrr::map(trait_names, function(trait) {

    # 6a. Global ANOVA Partitioning (Type I SS)
    rhs         <- paste(c(base_effects, interaction_effects, residual_gxe), collapse = " + ")
    formula_str <- paste(trait, "~", rhs)
    global_model <- stats::lm(stats::as.formula(formula_str), data = df_model)

    anova_tbl <- stats::anova(global_model) |>
      as.data.frame() |>
      tibble::rownames_to_column("Term") |>
      dplyr::mutate(Term = ifelse(Term == "ENV:GEN", "Residual", Term)) |>
      rlang::set_names(c("term", "df", "sum_sq", "mean_sq", "f_value", "p_value"))

    gxe_terms <-
      anova_tbl |>
      dplyr::filter(term %in% c(interaction_effects)) |>
      dplyr::select(term, df, sum_sq) |>
      dplyr::arrange(trait, desc(sum_sq))

    gxe_resid <-
      anova_tbl |>
      dplyr::filter(term %in% "Residual") |>
      dplyr::select(term, df, sum_sq)

    gxe_terms <-
      dplyr::bind_rows(gxe_terms, gxe_resid)

    total_gxe_ss <- sum(gxe_terms$sum_sq)

    gxe_contribution <- gxe_terms |>
      dplyr::mutate(
        trait = trait,
        percent_gxe = (sum_sq / total_gxe_ss) * 100,
        type = ifelse(term == "Residual", "Unexplained (Residual GxE)", "Explained by Covariate")
      ) |>
      dplyr::select(trait, term, type, df, sum_sq, percent_gxe) |>
      dplyr::mutate(percent_gxe_accum = cumsum(percent_gxe))

    # 6b. Independent Regression Extraction Per Genotype
    coef_tbl <- df_model |>
      dplyr::group_by(GEN) |>
      dplyr::group_modify(~ {
        ind_rhs   <- if (has_rep) paste(c("REP", cov_names), collapse = " + ") else paste(cov_names, collapse = " + ")
        ind_form  <- stats::as.formula(paste(trait, "~", ind_rhs))
        ind_model <- stats::lm(ind_form, data = .x)

        as.data.frame(summary(ind_model)$coefficients) |>
          tibble::rownames_to_column("term")
      }) |>
      dplyr::ungroup() |>
      dplyr::filter(term %in% cov_names) |>
      dplyr::mutate(
        trait = trait,
        term = paste0(GEN, ":", term),
        LL = Estimate - 1.96 * `Std. Error`,
        UP = Estimate + 1.96 * `Std. Error`,
        significance = ifelse(`Pr(>|t|)` < 0.05, "Significant", "Non-significant")
      ) |>
      dplyr::select(trait, term, estimate = Estimate, std.error = `Std. Error`,
                    statistic = `t value`, p.value = `Pr(>|t|)`, LL, UP, significance)

    list(anova = anova_tbl |> dplyr::filter(term == "Residual" | grepl("^GEN:", term)),
         contribution = gxe_contribution,
         coefficients = coef_tbl)
  })
  names(individual_results) <- trait_names

  # 7. Package Outputs Cleanly
  final_output <- list(
    gxe_contribution = purrr::map_dfr(individual_results, ~ .x$contribution),
    gxe_coefficients = purrr::map_dfr(individual_results, ~ .x$coefficients) |>
      dplyr::arrange(trait, term),
    gxe_partition    = purrr::map(individual_results, ~ .x$anova),
    pca_results      = pca_output,
    stepwise_results = stepwise_results
  )
  class(final_output) <- "factorial_reg"
  return(final_output)
}

#' @rdname factorial_reg
#' @export
plot.factorial_reg <- function(x,
                               which = c("contribution", "coefficients"),
                               trait = NULL,
                               covariate = NULL,
                               color_palette = NULL,
                               error_bars = TRUE,
                               ncol = NULL,
                               ...) {

  if (is.numeric(which)) {
    if (which == 1) which <- "contribution"
    else if (which == 2) which <- "coefficients"
    else cli::cli_abort("If providing a numeric index, {.arg which} must be 1 or 2.")
  }
  which <- rlang::arg_match(which)

  available_traits <- unique(x$gxe_contribution$trait)
  n_traits <- length(available_traits)

  # --- CUSTOM MINIMAL THEME ---
  theme_gxe <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title        = element_text(face = "bold", size = 14, color = "#222222", margin = margin(b = 8)),
        plot.subtitle     = element_text(size = 11, color = "#555555", margin = margin(b = 15)),
        axis.title        = element_text(face = "bold", size = 11, color = "#333333"),
        axis.text         = element_text(color = "#444444", size = 10),
        panel.grid.major  = element_line(color = "#ebebeb", linewidth = 0.4),
        panel.grid.minor  = element_blank(),
        strip.background  = element_rect(fill = "#f7f7f7", color = NA),
        strip.text        = element_text(face = "bold", color = "#333333", size = 11),
        legend.position   = "top",
        legend.title      = element_text(face = "bold", size = 10)
      )
  }

  # ==========================================
  # OPTION 1: GxE VARIANCE CONTRIBUTION PLOT
  # ==========================================
  if (which == "contribution") {

    dfcont <- x$gxe_contribution |> mutate(term = sub("GEN:", "", term))

    # --- SCENARIO A: MULTIPLE TRAITS (100% Stacked / position = fill) ---
    if (n_traits > 1) {
      p <- ggplot(dfcont, aes(x = trait, y = sum_sq, fill = term)) +
        geom_col(position = "fill", width = 0.5, alpha = 0.9, color = NA) +
        scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"), expand = c(0, 0)) +
        scale_fill_viridis_d(option = "plasma", end = 0.85) +
        labs(
          title = "GxE Variance Profile Comparison across Traits",
          subtitle = "Relative Sum of Squares (SS) partitioning explained by environmental factors",
          x = "Response Variable (Trait)",
          y = "Relative Sum of Squares (100%)",
          fill = "Model Terms:"
         ) +
        theme_gxe()

      return(p)

      # --- SCENARIO B: SINGLE TRAIT (Horizontal Sorted Bar Chart) ---
    } else {
      if (is.null(trait)) trait <- available_traits[1]
      df_plot <- dfcont |> dplyr::filter(trait == !!trait)

      if (is.null(color_palette)) {
        color_palette <- c("Explained by Covariate" = "#1b9e77", "Unexplained (Residual GxE)" = "#e66101")
      }

      p <- ggplot(df_plot, aes(x = reorder(term, percent_gxe), y = percent_gxe, fill = type)) +
        geom_col(width = 0.65, alpha = 0.9, color = NA) +
        coord_flip() +
        # Native percent suffix via paste0
        scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
        scale_fill_manual(values = color_palette) +
        labs(
          title = paste("GxE Variance Partitioning:", trait),
          subtitle = "Percentage of total interaction Sum of Squares explained by each term",
          x = "Model Term",
          y = "Contribution to GxE Variance (%)",
          fill = "Classification:"
        ) +
        theme_gxe()

      return(p)
    }
  }

  # ==========================================
  # OPTION 2: GENOTYPE COEFFICIENTS (SLOPES) PLOT
  # ==========================================
  if (which == "coefficients") {
    if (is.null(trait)) trait <- available_traits[1]

    df_plot <- x$gxe_coefficients |>
      dplyr::filter(trait == !!trait) |>
      mutate(
        covariate_name = sub(".*:", "", term),
        genotype = sub(":.*", "", term)
      )

    if (nrow(df_plot) == 0) {
      cli::cli_abort("No coefficients found for the selected trait {.val {trait}}. Available options: {.val {available_traits}}")
    }

    cov_quo <- rlang::enquo(covariate)
    if (!rlang::quo_is_null(cov_quo)) {
      available_covs <- unique(df_plot$covariate_name)
      dummy_data <- as.data.frame(matrix(0, nrow = 1, ncol = length(available_covs))) |>
        rlang::set_names(available_covs)

      selected_covs <- tidyselect::eval_select(cov_quo, dummy_data)
      cov_selected  <- names(selected_covs)

      if (length(cov_selected) == 0) {
        cli::cli_abort("No valid covariates selected via the {.field covariate} argument. Available options: {.val {available_covs}}")
      }
      df_plot <- df_plot |> dplyr::filter(covariate_name %in% cov_selected)
    }

    if (!"LL" %in% names(df_plot)) {
      df_plot <- df_plot |>
        dplyr::mutate(
          LL = estimate - 1.96 * std.error,
          UP = estimate + 1.96 * std.error
        )
    }
    if (!"significance" %in% names(df_plot)) {
      df_plot <- df_plot |>
        dplyr::mutate(
          significance = ifelse(p.value < 0.05, "Significant", "Non-significant")
        )
    }

    p <- ggplot(df_plot, aes(y = genotype, x = estimate, color = significance)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#999999", linewidth = 0.5) +
      geom_point(size = 3.5, alpha = 0.9) +
      facet_wrap(~covariate_name, scales = "free_x", ncol = ncol) +
      labs(
        title = paste("Genotypic Environmental Sensitivity:", trait),
        subtitle = "Absolute reaction norms (Slopes). Values deviating from zero indicate high responsiveness.",
        y = "Genotype",
        x = "Response Coefficient (Absolute Slope)",
        color = "Significance (p < 0.05)"
      ) +
      theme_gxe()

    if (error_bars) {
      p <- p + geom_errorbarh(aes(xmin = LL,
                                  xmax = UP),
                              width = 0.2, linewidth = 0.7, alpha = 0.9
      )
    }

    if (!is.null(color_palette)) {
      p <- p + scale_color_manual(values = rep(color_palette, length.out = 2))
    } else {
      p <- p + scale_color_manual(values = c("Significant" = "#2ca25f", "Non-significant" = "#999999"))
    }

    return(p)
  }
}

