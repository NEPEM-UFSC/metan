#' Finlay-Wilkinson Regression Analysis
#'
#' @description
#' `finlay_wilkinson()` evaluates Genotype-by-Environment (GxE) interactions across
#' multiple traits simultaneously using a tidyselection interface. Raw experimental
#' replicates within each genotype and environment combination are automatically
#' averaged into cell means prior to analysis.
#'
#' The underlying phenotypic response is modeled per trait as:
#' \deqn{Y_{ij} = b_0 + b_1 h_j + \varepsilon_{ij}}
#'
#' Where:
#' * \eqn{Y_{ij}} is the mean phenotypic performance of genotype \eqn{i} in environment \eqn{j}.
#' * \eqn{b_0} is the genotype intercept, representing its average performance across all trials.
#' * \eqn{b_1} is the environmental sensitivity slope of genotype \eqn{i}.
#' * \eqn{h_j} is the environmental index for site \eqn{j}, calculated via sum-to-zero contrasts (\eqn{\sum h_j = 0}).
#' * \eqn{\varepsilon_{ij}} is the residual error variance associated with genotype \eqn{i} in environment \eqn{j}.
#'
#' @param data A data frame containing genotypes, environments, and trait matrix columns.
#' @param gen Unquoted column name designating variety/genotype labels.
#' @param env Unquoted column name designating trial environment labels.
#' @param resp Tidyselection expression identifying one or more columns to analyze
#'   (e.g. `c(Yield, TKW)`, `starts_with("Trait")`, or `everything()`).
#'
#' @return A named list where each element represents an analyzed trait containing:
#' * `estimates`: A tibble with variety names (`gen`), intercepts (`b0`), and slopes (`b1`).
#' * `predictions`: A tibble showing aggregated inputs matched against expected regression fits (`yhat`).
#' * `var_e`: A named numeric vector tracking residual variances for each genotype.
#' * `var_e_weighted`: The pooled weighted error variance across the trait landscape.
#' @seealso [eberhart_russell()], [metan::ge_factanal()],  [superiority()], [ecovalence()], [ge_stats()]
#' @export
#' @md
#' @examples
#' \dontrun{
#' reg <- finlay_wilkinson(
#'   data_ge2,
#'   env  = ENV,
#'   gen  = GEN,
#'   resp = PH
#' )
#' plot(reg)
#'
#' # Example 1: Use an expression helper inside the highlight argument
#' # Highlights all genotypes that are highly responsive to environmental
#' changes (slope > 1)
#' plot(reg, trait = "PH", highlight = "b1 > 1")
#'
#' # Example 2: Combine expression filters
#' # Highlights genotypes that are highly stable (b1 < 1) AND produce yields
#' above the average population mean
#' plot(reg, trait = "PH", highlight = "b1 < 1 & b0 > mean(b0)")
#' }
finlay_wilkinson <- function(data, gen, env, resp) {

  # 1. Capture and resolve variables with tidy evaluation
  var_sym <- rlang::enquo(gen)
  env_sym <- rlang::enquo(env)

  # Resolve tidyselection for traits (resp)
  selected_traits <- tidyselect::eval_select(rlang::enquo(resp), data)
  trait_names     <- names(selected_traits)

  if (length(trait_names) == 0) {
    cli::cli_abort("No valid columns selected via the {.field resp} argument.")
  }

  # 2. Build local contrast safeguards
  old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old_contrasts), add = TRUE)

  # 3. Restructure core data frame into a clean, normalized long structure
  df_long <- data |>
    dplyr::mutate(
      .var_fct = factor(!!var_sym),
      .env_fct = factor(!!env_sym)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(trait_names),
      names_to = "trait",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(value = as.numeric(value)) |>
    # Calculate cell means for each GxE combination per trait
    dplyr::group_by(trait, .env_fct, .var_fct) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  VARlevels <- levels(df_long$.var_fct)
  ENVlevels <- levels(df_long$.env_fct)
  n.env     <- length(ENVlevels)

  # 4. Process each trait independently using split-apply-combine logic
  trait_output_list <- df_long |>
    dplyr::group_by(trait) |>
    dplyr::group_split() |>
    purrr::map(function(trait_df) {

      current_trait <- unique(trait_df$trait)

      # --- Step A: Compute Environmental Index (h) ---
      lm0 <- lm(value ~ .env_fct + .var_fct, data = trait_df)
      h_coefs <- stats::coef(lm0)[2:n.env]
      h_values <- c(h_coefs, -sum(h_coefs, na.rm = TRUE))

      env_lookup <- tibble::tibble(
        .env_fct = factor(ENVlevels, levels = ENVlevels),
        h = h_values
      )

      trait_with_h <- trait_df |>
        dplyr::left_join(env_lookup, by = ".env_fct")

      # --- Step B: Fit Variety-Specific Regressions ---
      variety_models <- trait_with_h |>
        dplyr::group_by(.var_fct) |>
        tidyr::nest() |>
        dplyr::mutate(
          model   = purrr::map(data, ~ lm(value ~ h, data = .x)),
          summary = purrr::map(model, summary),
          b0      = purrr::map_dbl(model, ~ stats::coef(.x)[1]),
          b1      = purrr::map_dbl(model, ~ stats::coef(.x)[2]),
          df      = purrr::map_int(summary, ~ .x$df[2]),
          var_e   = purrr::map_dbl(summary, ~ (.x$sigma)^2)
        ) |>
        dplyr::ungroup()

      # --- Step C: Calculate Pooled Error Variances ---
      var_e_weighted <- variety_models |>
        dplyr::summarise(weighted = sum(var_e * df) / sum(df)) |>
        dplyr::pull(weighted)

      # --- Step D: Construct Variety Estimates Tibble ---
      estimates_df <- variety_models |>
        dplyr::select(gen = .var_fct, b0, b1) |>
        dplyr::mutate(gen = as.character(gen))

      # --- Step E: Construct Predicted Values Matrix ---
      predictions_df <- trait_with_h |>
        dplyr::left_join(variety_models |> dplyr::select(.var_fct, b0, b1), by = ".var_fct") |>
        dplyr::mutate(yhat = b0 + b1 * h) |>
        dplyr::select(
          gen = .var_fct,
          env = .env_fct,
          observed = value,
          env_index = h,
          yhat
        ) |>
        dplyr::mutate(
          gen = as.character(gen),
          env = as.character(env)
        )

      # --- Step F: Assemble Named List Package ---
      var_e_vector <- stats::setNames(variety_models$var_e, as.character(variety_models$.var_fct))

      trait_results <- list(
        estimates      = estimates_df,
        predictions    = predictions_df,
        var_e          = var_e_vector,
        var_e_weighted = var_e_weighted
      )

      list(trait = current_trait, data = trait_results)
    })

  # 5. format outcomes into a clean named list structure
  final_names <- purrr::map_chr(trait_output_list, ~ .x$trait)
  final_list  <-
    purrr::map(trait_output_list, ~ .x$data) |>
    purrr::set_names(final_names)

  class(final_list) <- c("FW", "list")
  return(final_list)
}

#' Plot Method for Finlay-Wilkinson Multi-Trait Objects
#'
#' Visualizes genotype regression lines across the calculated environmental index
#' for a specified trait, allowing custom variety filtering, flexible expression-based
#' highlighting, and population reference lines.
#'
#' @param x An object of class `FW` generated by `finlay_wilkinson`.
#' @param trait Character string specifying which trait in the list to plot.
#'   Defaults to the first trait.
#' @param type Integer specifying the plot architecture type.
#'   * `1` (Default): Plots variety regression trajectories against the calculated
#'     environmental index gradient (h).
#'   * `2`: Generates the Finlay-Wilkinson coordinate mapping model, tracking
#'     environmental sensitivity slopes (\eqn{b_1}) against overall variety arithmetic means (\eqn{b_0}).
#' @param genotypes Optional selection tool. Can be a character vector of specific
#'   genotype names, or a logical string expression evaluating parameters in the
#'   estimates table (e.g., `"b1 > 1"`, `"b1 < 1"`, or `"b0 > mean(b0)"`).
#' @param highlight Dynamic highlighting tool. Pass an **integer** to highlight the top
#'   \eqn{n} genotypes with the largest slope (`b1`), a **character vector** of
#'   explicit genotype names, or a **logical string helper expression** (e.g., `"b1 > 1"`)
#'   to highlight matching elements. Non-highlighted genotypes turn gray with alpha = 0.5.
#'   Default is `10`.
#' @param show_reference Logical. If `TRUE`, adds a dashed reference line. For `type = 1`,
#'   plots the average population performance baseline (\eqn{\beta_1 = 1}). For `type = 2`,
#'   adds crosshair markers highlighting population static averages. Default is `TRUE`.
#' @param plot_theme The graphical theme of the plot. Default is
#'   `theme_metan_minimal()`. For more details, see [ggplot2::theme()].
#' @param ... Unused trailing arguments for S3 consistency.
#'
#' @return A `ggplot` object tracking genotype performance lines across
#'   the environmental index gradient.
#' @references
#' Finlay, K.W., and G.N. Wilkinson. 1963. The analysis of adaptation in a
#' plant-breeding programme. Australian Journal of Agricultural Research 14(6):
#' 742–754. \doi{10.1071/AR9630742}
#' @method plot FW
#' @export
#' @md
plot.FW <- function(x,
                    trait = NULL,
                    type = 1,
                    genotypes = NULL,
                    highlight = 10,
                    show_reference = TRUE,
                    plot_theme = theme_metan_minimal(),
                    ...) {


  if (!type %in% c(1, 2)) {
    cli::cli_abort("{.field type} argument must be either {.val 1} (Trajectories) or {.val 2} (Adaptation Coordinates).")
  }

  # 2. Isolate target trait
  available_traits <- names(x)
  if (is.null(trait)) {
    trait <- available_traits[1]
    cli::cli_alert_info("No trait specified. Defaulting to first available trait: {.val {trait}}")
  } else if (!trait %in% available_traits) {
    cli::cli_abort("Trait {.val {trait}} not found. Available traits are: {.val {available_traits}}")
  }

  trait_data <- x[[trait]]
  estimates  <- trait_data$estimates
  predicts   <- trait_data$predictions

  # 3. Determine Environmental Index limits
  h_min <- min(predicts$env_index, na.rm = TRUE)
  h_max <- max(predicts$env_index, na.rm = TRUE)

  # 4. Helper Function to parse logical string expressions safely
  parse_logical_expression <- function(expr_string, target_df) {
    expr_string <- gsub("&&", "&", expr_string, fixed = TRUE)
    expr_string <- gsub("||", "|", expr_string, fixed = TRUE)
    eval_env <- as.list(target_df)
    eval_env$mean   <- mean
    eval_env$median <- median

    passed_logical <- tryCatch({
      eval(parse(text = expr_string), envir = eval_env)
    }, error = function(e) {
      cli::cli_abort("Could not evaluate expression: {.code {expr_string}}")
    })

    if (!is.logical(passed_logical)) {
      cli::cli_abort("Expression must return a logical vector: {.code {expr_string}}")
    }
    return(target_df$gen[passed_logical & !is.na(passed_logical)])
  }

  # 5. Filter Genotypes dynamically based on user input (Base dataset selection)
  selected_gens <- estimates$gen

  if (!is.null(genotypes)) {
    if (is.character(genotypes) && length(genotypes) == 1 && any(grepl("b0|b1", genotypes))) {
      cli::cli_process_start("Filtering subset with expression: {.code {genotypes}}")
      selected_gens <- parse_logical_expression(genotypes, estimates)
      cli::cli_process_done(msg = "Dataset expression filtered successfully. Retained {.val {length(selected_gens)}} genotypes.")
    } else if (is.character(genotypes)) {
      invalid_names <- setdiff(genotypes, estimates$gen)
      if (length(invalid_names) > 0) {
        cli::cli_warn("Ignoring missing genotype(s) from selection vector: {.val {invalid_names}}")
      }
      selected_gens <- intersect(genotypes, estimates$gen)
    }
  }

  if (length(selected_gens) == 0) {
    cli::cli_abort("No genotypes matched your base dataset selection criteria.")
  }

  # Apply baseline subset selection filters
  estimates_filtered <- estimates |> dplyr::filter(gen %in% selected_gens)
  predicts_filtered  <- predicts  |> dplyr::filter(gen %in% selected_gens)

  # 6. Handle Dynamic Highlighting Feature (Explicit names, Top N, OR Helper Expressions)
  is_highlight_active <- FALSE
  if (!is.null(highlight)) {

    # Try to evaluate string vector expressions (like "c('G20', 'G11')")
    if (is.character(highlight) && length(highlight) == 1) {
      evaluated_val <- tryCatch({
        val <- eval(parse(text = highlight), envir = baseenv())
        if (is.character(val)) val else NULL
      }, error = function(e) {
        NULL
      })
      if (!is.null(evaluated_val)) {
        highlight <- evaluated_val
      }
    }

    # Mode A: User provided an expression helper string (e.g. "b1 > 1")
    if (is.character(highlight) && length(highlight) == 1 && any(grepl("b0|b1", highlight))) {
      cli::cli_process_start("Evaluating highlight expression rule: {.code {highlight}}")
      matched_highlights <- parse_logical_expression(highlight, estimates_filtered)
      highlighted_gens   <- intersect(matched_highlights, estimates_filtered$gen)

      if (length(highlighted_gens) > 0) {
        is_highlight_active <- TRUE
        cli::cli_process_done(
          msg = "Highlight expression successfully isolated {.val {length(highlighted_gens)}} genotype(s): {.val {highlighted_gens}}"
        )
      } else {
        cli::cli_process_failed()
        cli::cli_alert_danger("Zero active genotypes match your highlight helper expression: {.code {highlight}}")
      }

      # Mode B: User provided an integer number (Top N largest b1 values)
    } else if (is.numeric(highlight) && length(highlight) == 1) {
      if (highlight <= 0) cli::cli_abort("{.field highlight} rank value must be a positive integer.")

      highlight_n <- as.integer(highlight)
      n_available <- nrow(estimates_filtered)

      if (highlight_n >= n_available) {
        cli::cli_alert_warning(
          "Requested {.field highlight} count ({highlight_n}) is >= total active subset lines ({n_available}). Canvas highlight ignored."
        )
      } else {
        is_highlight_active <- TRUE
        highlighted_gens <- estimates_filtered |>
          dplyr::arrange(dplyr::desc(b1)) |>
          dplyr::slice_head(n = highlight_n) |>
          dplyr::pull(gen)

        cli::cli_alert_success(
          "Isolated top {.val {highlight_n}} most responsive genotype(s) by slope ({.field b1}): {.val {highlighted_gens}}"
        )
      }

      # Mode C: User provided an explicit character vector of genotype names
    } else if (is.character(highlight)) {
      invalid_highlights <- setdiff(highlight, estimates_filtered$gen)
      if (length(invalid_highlights) > 0) {
        cli::cli_alert_warning(
          "Skipping explicit highlight target(s) (either missing completely or dropped by base filters): {.val {invalid_highlights}}"
        )
      }

      highlighted_gens <- intersect(highlight, estimates_filtered$gen)
      if (length(highlighted_gens) > 0) {
        is_highlight_active <- TRUE
        cli::cli_alert_success("Highlighting explicit genotype configurations: {.val {highlighted_gens}}")
      } else {
        cli::cli_alert_danger("None of your explicit highlight parameters exist inside the current active dataset slice.")
      }
    } else {
      cli::cli_abort("{.field highlight} must be a positive rank integer, explicit name character vector, or string logical statement.")
    }

    # Apply aesthetic modifications if highlighting mode successfully triggered
    if (is_highlight_active) {
      n_highlighted <- length(highlighted_gens)

      estimates_filtered <- estimates_filtered |>
        dplyr::mutate(
          is_top = ifelse(gen %in% highlighted_gens, gen, "Other"),
          is_top = factor(is_top, levels = c(highlighted_gens, "Other")),
          alpha_val = ifelse(gen %in% highlighted_gens, 1.0, 0.4),
          size_val  = ifelse(gen %in% highlighted_gens, 1.0, 0.5)
        )

      predicts_filtered <- predicts_filtered |>
        dplyr::mutate(
          is_top = ifelse(gen %in% highlighted_gens, gen, "Other"),
          is_top = factor(is_top, levels = c(highlighted_gens, "Other")),
          alpha_val = ifelse(gen %in% highlighted_gens, 0.6, 0.1)
        )

      color_palette <- ggplot2::scale_color_manual(
        values = stats::setNames(
          c(grDevices::hcl.colors(n_highlighted, palette = "Dark 2"), "gray70"),
          c(highlighted_gens, "Other")
        )
      )
    }
  }

  cli::cli_alert_success("Rendering Finlay-Wilkinson graphics framework for {.val {length(selected_gens)}} variety vectors.")

  # ==========================================
  # ARCHITECTURE TYPE 1: REGRESSION TRAJECTORIES
  # ==========================================
  if (type == 1) {
    p <- ggplot2::ggplot()

    if (is_highlight_active) {
      p <- p + ggplot2::geom_abline(
        data = estimates_filtered,
        ggplot2::aes(intercept = b0, slope = b1, color = is_top, alpha = alpha_val, linewidth = size_val)
      ) +
        ggplot2::geom_point(
          data = predicts_filtered,
          ggplot2::aes(x = env_index, y = observed, color = is_top, alpha = alpha_val),
          size = 1.5
        ) +
        ggplot2::scale_alpha_identity() +
        ggplot2::scale_linewidth_identity() +
        color_palette
    } else {
      p <- p + ggplot2::geom_abline(
        data = estimates_filtered,
        ggplot2::aes(intercept = b0, slope = b1, color = gen),
        linewidth = 0.8
      ) +
        ggplot2::geom_point(
          data = predicts_filtered,
          ggplot2::aes(x = env_index, y = observed, color = gen),
          alpha = 0.4, size = 1.5
        )
    }

    if (isTRUE(show_reference)) {
      avg_intercept <- mean(estimates$b0, na.rm = TRUE)
      p <- p + ggplot2::geom_abline(
        intercept = avg_intercept, slope = 1, linetype = "dashed", color = "gray30", linewidth = 1
      )
    }

    p <- p +
      ggplot2::labs(
        title = paste("Finlay-Wilkinson Regression Analysis:", trait),
        subtitle = paste("Showing", length(selected_gens), "of", nrow(estimates), "genotypes"),
        x = "Environmental Index (h)",
        y = "Phenotypic Response Value"
      ) +
      ggplot2::xlim(h_min - 0.5, h_max + 0.5)

    # ==========================================
    # ARCHITECTURE TYPE 2: ADAPTATION COORDINATES
    # ==========================================
  } else {
    p <- ggplot2::ggplot()

    if (is_highlight_active) {
      p <- p + ggplot2::geom_point(
        data = estimates_filtered,
        ggplot2::aes(x = b0, y = b1, color = is_top, alpha = alpha_val, size = size_val * 3)
      ) +
        ggrepel::geom_text_repel(
          data = estimates_filtered |> dplyr::filter(gen %in% highlighted_gens),
          ggplot2::aes(x = b0, y = b1, label = gen, color = is_top),
          fontface = "bold", box.padding = 0.3, max.overlaps = Inf
        ) +
        ggplot2::scale_alpha_identity() +
        ggplot2::scale_size_identity() +
        color_palette
    } else {
      p <- p + ggplot2::geom_point(
        data = estimates_filtered,
        ggplot2::aes(x = b0, y = b1, color = gen), size = 3
      ) +
        ggrepel::geom_text_repel(
          data = estimates_filtered,
          ggplot2::aes(x = b0, y = b1, label = gen, color = gen),
          box.padding = 0.3, max.overlaps = 15
        )
    }

    if (isTRUE(show_reference)) {
      pop_mean_b0 <- mean(estimates$b0, na.rm = TRUE)
      p <- p +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = pop_mean_b0, linetype = "dashed", color = "gray40", alpha = 0.7)
    }

    p <- p +
      ggplot2::labs(
        title = paste("Finlay-Wilkinson Population Adaptation Pattern:", trait),
        subtitle = "Generalized coordinate model plotting stability sensitivity vs mean performance",
        x = paste("Genotype Mean Performance (b0)"),
        y = "Environmental Sensitivity Coefficient (b1)"
      )
  }

  # 11. Common Layout Aesthetics Styling
  p <- p +
    plot_theme %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(color = if (is_highlight_active) "Highlighted Genotypes" else "Genotype")

  return(p)
}
