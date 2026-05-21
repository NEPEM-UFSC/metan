#' Graphical analysis of genotype-vs-environment interaction
#' @description
#' `r badge('stable')`
#'
#' This function produces a line plot for a graphical interpretation of the
#' genotype-vs-environment interaction. By default, environments are in the x
#' axis whereas the genotypes are depicted by different lines. The y axis
#' contains the value of the selected variable. A heatmap can also be created.
#'
#' @param .data The dataset containing the columns related to Environments,
#'   Genotypes, replication/block and response variable(s).
#' @param env The name of the column that contains the levels of the
#'   environments
#' @param gen The name of the column that contains the levels of the genotypes.
#' @param resp The response variable.
#' @param type The type of plot `type = 1` for a heatmap or `type = 2`
#'   for a line plot.
#' @param values Show the values in the plot? Defaults to `TRUE`.
#' @param text_row_pos,text_col_pos The position of the text in the
#'   rows and columns. The defaults show the text at left and top.
#' @param average Show the average values for environments and genotypes?
#'   Defaults to `TRUE`.
#' @param order_g,order_e A charactere vector indicating the order of the levels
#'   for genotypes and environments, respectively. This can be used to change
#'   the default ordering of rows and columns.
#' @param xlab,ylab The labels for x and y axis, respectively.
#' @param width_bar,heigth_bar The width and heigth of the legend bar,
#'   respectively.
#' @param plot_theme The graphical theme of the plot. Default is
#'   `plot_theme = theme_metan_minimal()`. For more details,see
#'   [ggplot2::theme()].
#' @param colour Logical argument. If `FALSE` then the plot will not be
#'   colored.
#' @param row_col,row_col_type Shows row/column and defines what to show.
#'   Defaults to 'average'.
#' @param highlight Dynamic highlighting tool. Pass an **integer** to highlight the top
#'   `n` genotypes by overall mean, a **character vector** of explicit
#'   genotype names, or a **logical string helper expression** (e.g. `"all(Y > env_mean)"`)
#'   to highlight matching elements. Non-highlighted genotypes turn gray with alpha = 0.4.
#' @param plot_env_mean Logical argument. If `TRUE` (default), a dashed line and a solid square mark
#'   are added to the plot (for `type = 2`) to represent the environmental mean.
#' @return An object of class `gg, ggplot`.
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' library(metan)
#' ge_plot(data_ge2, ENV, GEN, PH)
#' ge_plot(data_ge, ENV, GEN, GY, type = 2)
#'}
ge_plot <- function(.data,
                    env,
                    gen,
                    resp,
                    type = 1,
                    values = TRUE,
                    text_col_pos = c("top", "bottom"),
                    text_row_pos = c("left", "right"),
                    average = TRUE,
                    row_col = TRUE,
                    row_col_type = c("average", "sum"),
                    order_g = NULL,
                    order_e = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    width_bar = 1.5,
                    heigth_bar = 15,
                    plot_theme = theme_metan_minimal(),
                    colour = TRUE,
                    highlight = NULL,
                    plot_env_mean = TRUE) { # <-- NEW ARGUMENT

  text_col_pos <- rlang::arg_match(text_col_pos)
  text_row_pos <- rlang::arg_match(text_row_pos)

  is_highlight_active <- FALSE
  if (!is.null(highlight)) {
    df_eval <- dplyr::select(.data, ENV = {{env}}, GEN = {{gen}}, Y = {{resp}}) |>
      mean_by(ENV, GEN, na.rm = TRUE) |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(env_mean = mean(Y, na.rm = TRUE)) |>
      dplyr::ungroup()

    df_eval$GEN <- as.character(df_eval$GEN)

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

    if (is.character(highlight) && length(highlight) == 1 && !highlight %in% df_eval$GEN) {
      cli::cli_process_start("Evaluating highlight expression rule: {.code {highlight}}")

      highlight <- gsub("&&", "&", highlight, fixed = TRUE)
      highlight <- gsub("||", "|", highlight, fixed = TRUE)

      highlighted_gens <- tryCatch({
        df_clean <- df_eval |> dplyr::filter(!is.na(Y) & !is.na(env_mean))
        expr <- rlang::parse_expr(highlight)

        tryCatch({
          df_clean |>
            dplyr::group_by(GEN) |>
            dplyr::summarise(matched = !!expr, .groups = "drop") |>
            dplyr::filter(!is.na(matched) & matched == TRUE) |>
            dplyr::pull(GEN)
        }, error = function(e_inner) {
          df_clean |>
            dplyr::group_by(GEN) |>
            dplyr::mutate(matched_rule = !!expr) |>
            dplyr::summarise(matched = any(matched_rule, na.rm = TRUE), .groups = "drop") |>
            dplyr::filter(!is.na(matched) & matched == TRUE) |>
            dplyr::pull(GEN)
        })
      }, error = function(e_outer) {
        cli::cli_abort("Could not evaluate expression: {.code {highlight}}")
      })

      if (length(highlighted_gens) > 0) {
        is_highlight_active <- TRUE
        cli::cli_process_done(msg = "Highlight expression successfully isolated {.val {length(highlighted_gens)}} genotype(s).")
      } else {
        cli::cli_process_failed()
        cli::cli_alert_danger("Zero active genotypes match your highlight helper expression: {.code {highlight}}")
      }

    } else if (is.numeric(highlight) && length(highlight) == 1) {
      highlight_n <- as.integer(highlight)
      highlighted_gens <- df_eval |>
        dplyr::group_by(GEN) |>
        dplyr::summarise(Y = mean(Y, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(Y)) |>
        dplyr::slice_head(n = highlight_n) |>
        dplyr::pull(GEN)
      if (length(highlighted_gens) > 0) is_highlight_active <- TRUE

    } else if (is.character(highlight)) {
      highlighted_gens <- intersect(highlight, df_eval$GEN)
      if (length(highlighted_gens) > 0) {
        is_highlight_active <- TRUE
      } else {
        cli::cli_alert_danger("None of your explicit highlight parameters exist inside the current dataset slice.")
      }
    }
  }

  # --- Type 1: Heatmap Visualizations ---
  if(type == 1){
    if(!isTRUE(average)){
      cli::cli_warn("'average' argument was deprecated as of metan 1.19.0. Use 'row_col' instead")
      row_col <- average
    }
    if(isTRUE(row_col)){
      row_col_type <- rlang::arg_match(row_col_type)
      if(row_col_type == "average"){
        mat <-
          dplyr::select(.data,
                        ENV = {{env}},
                        GEN = {{gen}},
                        Y = {{resp}}) |>
          make_mat(GEN, ENV, Y) |>
          row_col_mean(na.rm = TRUE)
        colnames(mat)[ncol(mat)] <- "Average"
        rownames(mat)[nrow(mat)] <- "Average"
      } else{
        mat <-
          dplyr::select(.data,
                        ENV = {{env}},
                        GEN = {{gen}},
                        Y = {{resp}}) |>
          make_mat(GEN, ENV, Y) |>
          row_col_sum(na.rm = TRUE)
        colnames(mat)[ncol(mat)] <- "Sum"
        rownames(mat)[nrow(mat)] <- "Sum"
      }

      if(is.null(order_e)){
        order_e <- colnames(mat)[-ncol(mat)]
      } else{
        order_e <- order_e
      }
      if(is.null(order_g)){
        order_g <- rownames(mat)[-nrow(mat)]
      } else{
        order_g <- order_g
      }
      lbl <- if(row_col_type == "average") "Average" else "Sum"
      df_long <-
        make_long(mat) |>
        as_factor(1:2) |>
        mutate(ENV = factor(ENV, levels = c(order_e, lbl)),
               GEN = factor(GEN, levels = c(lbl, order_g)))
    } else{
      df_long <-
        dplyr::select(.data,
                      ENV = {{env}},
                      GEN = {{gen}},
                      Y = {{resp}}) |>
        mean_by(ENV, GEN, na.rm = TRUE) |>
        as_factor(ENV, GEN)
      if(is.null(order_e)){
        order_e <- levels(df_long$ENV)
      } else{
        order_e <- order_e
      }
      if(is.null(order_g)){
        order_g <- levels(df_long$GEN)
      } else{
        order_g <- order_g
      }
      df_long <-
        df_long |>
        mutate(ENV = factor(ENV, levels = order_e),
               GEN = factor(GEN, levels = order_g))
    }

    p <-
      ggplot(df_long, aes(ENV, GEN, fill = Y)) +
      geom_tile(color = "black")+
      {if(isTRUE(row_col)) geom_hline(yintercept = 1.5, linewidth = 1.2, color = "black")} +
      {if(isTRUE(row_col)) geom_vline(xintercept = length(order_e) + 0.5, linewidth = 1.2, color = "black")} +
      {if(text_row_pos == "left")
        scale_y_discrete(expand = expansion(mult = c(0,0)))}+
      {if(text_row_pos != "left")
        scale_y_discrete(expand = expansion(mult = c(0,0)),
                         position = "right")}+
      {if(text_col_pos != "top")
        scale_x_discrete(expand = expansion(mult = c(0,0)))} +
      {if(text_col_pos == "top")
        scale_x_discrete(position = "top",
                         expand = expansion(0))} +
      scale_fill_gradient2(
        low = "red", mid = "white", high = "blue",
        midpoint = mean(df_long$Y, na.rm = TRUE)
      ) +
      {if(isTRUE(values))geom_text(aes(label = round(Y, 1)),
                                   color = "black",
                                   size = 3)} +
      guides(fill = guide_colourbar(label = TRUE,
                                    draw.ulim = TRUE,
                                    draw.llim = TRUE,
                                    frame.colour = "black",
                                    ticks = TRUE,
                                    nbin = 10,
                                    label.position = "right",
                                    barwidth = width_bar,
                                    barheight = heigth_bar,
                                    direction = 'vertical'))+
      plot_theme %+replace%
      theme(legend.position = "right",
            legend.title = element_blank()) +
      labs(x = xlab,
           y = ylab)

    if(text_col_pos == "top"){
      p <- p + theme(axis.text.x.top = element_text(angle = 90, vjust = 0.5, hjust = 0))
    } else{
      p <- p + theme(axis.text.x.bottom = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
    if (is_highlight_active) {
      y_labels <- levels(df_long$GEN)
      y_faces <- ifelse(y_labels %in% highlighted_gens, "bold", "plain")
      p <- p + theme(axis.text.y = element_text(face = y_faces))
    }
  }

  # --- Type 2: Line / Interaction Plot Visualizations ---
  if(type == 2){
    if (is_highlight_active) {
      if (is.null(xlab)) xlab <- rlang::as_label(rlang::enquo(env))
      if (is.null(ylab)) ylab <- rlang::as_label(rlang::enquo(resp))

      df_plot <- dplyr::select(.data, ENV = {{env}}, GEN = {{gen}}, Y = {{resp}}) |>
        dplyr::mutate(
          GEN_chr = as.character(GEN),
          is_top = ifelse(GEN_chr %in% highlighted_gens, GEN_chr, "Other"),
          is_top = factor(is_top, levels = c(highlighted_gens, "Other")),
          alpha_val = ifelse(GEN_chr %in% highlighted_gens, 1.0, 0.4),
          size_val  = ifelse(GEN_chr %in% highlighted_gens, 1.0, 0.5)
        )

      n_highlighted <- length(highlighted_gens)
      color_palette <- ggplot2::scale_color_manual(
        values = stats::setNames(
          c(grDevices::hcl.colors(n_highlighted, palette = "Dark 2"), "gray70"),
          c(highlighted_gens, "Other")
        )
      )

      p <- ggplot(df_plot, aes(x = ENV, y = Y, group = GEN)) +
        stat_summary(aes(colour = is_top,
                         alpha = alpha_val,
                         linewidth = size_val),
                     fun = mean,
                     geom = "line",
                     na.rm = TRUE) +
        stat_summary(aes(colour = is_top,
                         alpha = alpha_val),
                     fun = mean,
                     geom = "point",
                     size = 3,
                     shape = 18,
                     na.rm = TRUE) +
        ggplot2::scale_alpha_identity() +
        ggplot2::scale_linewidth_identity() +
        color_palette +
        plot_theme %+replace%
        theme(legend.position = "right") +
        labs(color = "Highlighted Genotypes")

    } else {
      p <- ggplot(.data, aes(x = {{env}}, y = {{resp}}))
      if (colour == TRUE) {
        p <- p +
          stat_summary(aes(colour = {{gen}},
                           group = {{gen}}),
                       fun = mean,
                       geom = "line",
                       na.rm = TRUE)
      } else {
        p <- p +
          stat_summary(aes(group = {{gen}}),
                       fun = mean,
                       geom = "line",
                       colour = "black",
                       na.rm = TRUE)
      }
      p <- p + geom_point(stat = "summary",
                          fun = mean,
                          size = 3,
                          shape = 18) +
        plot_theme %+replace%
        theme(legend.position = "right")
    }

    # --- ADD THE ENVIRONMENTAL MEAN LAYER ---
    if (isTRUE(plot_env_mean)) {
      p <- p +
        stat_summary(aes(group = 1), # group = 1 ignores genotype grouping
                     fun = mean,
                     geom = "line",
                     linetype = "dashed",
                     colour = "black",
                     linewidth = 1.2,
                     na.rm = TRUE) +
        stat_summary(aes(group = 1),
                     fun = mean,
                     geom = "point",
                     shape = 15,     # 15 is a solid square mark
                     size = 4,
                     colour = "black",
                     na.rm = TRUE)
    }
  }

  return(p + labs(x = xlab, y = ylab))
}

