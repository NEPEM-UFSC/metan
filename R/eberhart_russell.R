#' Eberhart and Russell (1966) Stability Analysis
#'
#' @description
#' `r badge('stable')`
#' `eberhart_russell()` implements the classic joint regression model proposed by
#' Eberhart and Russell (1966) to evaluate phenotypic stability and adaptability
#' across multi-environment trials (MET).
#'
#' The model partitions the genotype-environment (GxE) interaction into a linear
#' response to an environmental index (adaptability, \eqn{b_1}) and non-linear
#' deviations from the regression line (stability, \eqn{s^2_{di}}).
#'
#' The phenotypic mean of genotype \eqn{i} in environment \eqn{j} is modeled as:
#' \deqn{Y_{ij} = b_0 + b_1 I_j + \delta_{ij} + \bar{\varepsilon}_{ij}}
#'
#' Where:
#' * \eqn{Y_{ij}} is the mean performance of genotype \eqn{i} in environment \eqn{j}.
#' * \eqn{b_0} is the intercept (general mean of genotype \eqn{i}).
#' * \eqn{b_1} is the regression coefficient (slope), measuring phenotypic response/adaptability.
#' * \eqn{I_j} is the environmental index of site \eqn{j}, computed as \eqn{I_j = \bar{Y}_{.j} - \bar{Y}_{..}}.
#' * \eqn{\delta_{ij}} represents the deviation from the regression of genotype \eqn{i} in environment \eqn{j}.
#' * \eqn{\bar{\varepsilon}_{ij}} is the pooled experimental error associated with the cell mean.
#'
#' @param .data A data frame or tibble containing the multi-environment trial dataset.
#' @param env Unquoted column name designating trial environmental factor levels.
#' @param gen Unquoted column name designating genotype/variety factor levels.
#' @param rep Unquoted column name designating replication or block assignments. Used to
#'   extract the pooled residual error from within-environment analysis of variance.
#' @param resp Tidyselection expression identifying one or more continuous response columns to
#'   analyze simultaneously (e.g., `c(Yield, TKW)`, `starts_with("Trait")`, or `everything()`).
#' @param verbose Logical. If `FALSE`, console process tracking indicators and
#'   summary log alerts are suppressed. Default is `TRUE`.
#'
#' @details
#' Two critical statistical tests are executed for each genotype:
#' 1. A **Student's t-test** under \eqn{H_0: b_1 = 1} to assess if the genotype's
#'    response tracks the exact population average (\eqn{b_1 > 1} indicates response to premium environments;
#'    \eqn{b_1 < 1} indicates resistance/predictability in poor environments).
#' 2. An **F-test** under \eqn{H_0: s^2_{di} = 0} using the pooled experimental error
#'    as the denominator. Significantly high deviations indicate poor stability (low predictability).
#'
#' @return An object of S3 class `eberhart_russell`, structured as a named list for each
#'   evaluated trait containing:
#' * `data`: A tibble tracking cell means per GxE combination alongside the calculated
#'   environmental index (`I_j`).
#' * `anova`: Joint analysis of variance table decomposing Degrees of Freedom, Sum of Squares,
#'   and Mean Squares across Genotypes, Environments, GxE, Regression, and Deviations.
#' * `regression`: A summary parameter tibble containing:
#'   * `GEN`: Genotype identification factor levels.
#'   * `b0`: Intercept (estimated genotype phenotypic arithmetic mean).
#'   * `b1`: True phenotypic adaptability regression slope response.
#'   * `t_value`: Calculated t-statistic evaluating \eqn{H_0: b_1 = 1}.
#'   * `pval_t`: Two-tailed probability significance value for the t-test.
#'   * `s2di`: Deviations mean square parameter variance (\eqn{s^2_{di}}).
#'   * `F_value`: Calculated F-statistic evaluating \eqn{H_0: s^2_{di} = 0}.
#'   * `pval_f`: Probability significance value for the F-test.
#'   * `RMSE`: Root-Mean-Square Error of individual regression tracks.
#'   * `R2`: Coefficient of determination indicating fitness quality of the linear trend.
#' * `b0_variance`: Estimated standard error variance for parameter intercept calculations.
#' * `b1_variance`: Estimated standard error variance for parameter slope calculations.
#' @md
#' @references Eberhart, S.A., and W.A. Russell. 1966. Stability parameters for comparing varieties. Crop Sci. 6:36-40. \doi{10.2135/cropsci1966.0011183X000600010011x}
#'
#' @seealso [finlay_wilkinson()], [metan::ge_factanal()],  [superiority()], [ecovalence()], [ge_stats()]
#' @author Tiago Olivoto, \email{tiagoolivoto@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' library(metan)
#'reg <-
#'  eberhart_russell(data_ge2,
#'                   env = ENV,
#'                   gen = GEN,
#'                   rep = REP,
#'                   resp = PH)
#'plot(reg)
#'
#'}
eberhart_russell <- function(.data,
                             env,
                             gen,
                             rep,
                             resp,
                             verbose = TRUE){
  factors  <-
    .data |>
    select({{env}}, {{gen}}, {{rep}}) |>
    mutate(across(everything(), as.factor))
  vars <-
    .data |>
    select({{resp}}, -names(factors)) |>
    select_numeric_cols()
  factors <- factors |> set_names("ENV", "GEN", "REP")
  listres <- list()
  nvar <- ncol(vars)
  if (verbose == TRUE) {
    var <- 0
    pb <- cli::cli_progress_bar(
      total = nvar,
      format = "{cli::pb_spin} Evaluating trait {.strong {names(vars[var])}} | {cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}] | ETA: {cli::pb_eta}"
    )
  }
  for (var in 1:nvar) {
    data <-
      factors |>
      mutate(Y = vars[[var]])
    if(has_na(data)){
      data <- remove_rows_na(data)
      has_text_in_num(data)
    }
    data2 <-
      data |>
      mean_by(ENV, GEN) |>
      as.data.frame()
    model1 <- lm(Y ~ GEN + ENV + ENV/REP + ENV * GEN, data = data)
    modav <- anova(model1)
    mydf <-
      data |>
      mean_by(GEN, ENV)
    iamb <-
      data |>
      mean_by(ENV) |>
      add_cols(IndAmb = Y - mean(Y)) |>
      dplyr::select(ENV, IndAmb)
    iamb2 <-
      data |>
      mean_by(ENV, GEN) |>
      left_join(iamb, by = "ENV")
    meandf <- make_mat(mydf, GEN, ENV, Y) |> rownames_to_column("GEN")
    matx <- make_mat(mydf, GEN, ENV, Y) |> as.matrix()
    iij <- apply(matx, 2, mean, na.rm = TRUE) - mean(matx, na.rm = TRUE)
    sumij2 <- sum((iij)^2)
    YiIj <- matx %*% iij
    if(has_na(matx)){
      missing <- which(apply(is.na(matx), 1, function(x){any(x) == TRUE}) == TRUE)
      YiIj_complete <- NULL
      for(i in seq_along(missing)){
        YiIj_complete[i] <- matx[missing[i],][!is.na(matx[missing[i],])] %*% iij[!is.na(matx[missing[i],])]
      }
      YiIj[which(is.na(YiIj))] <- YiIj_complete
      warning("Genotypes ", paste(names(missing), collapse = ", "), " missing in some environments")
      warning("Regression parameters computed after removing missing values")
    }
    bij <- YiIj/sumij2
    svar <- (apply(matx^2, 1, sum, na.rm = TRUE)) - (((apply(matx, 1, sum, na.rm = TRUE))^2)/ncol(matx))
    bYijIj <- bij * YiIj
    dij <- svar - bYijIj
    pred <- apply(matx, 1, mean, na.rm = TRUE) + bij %*% iij
    gof <- function(x, y){
      R2 = NULL
      RMSE = NULL
      for (i in 1:nrow(x)){
        R2[i] =  cor(x[i, ], y[i, ], use = "complete.obs")^2
        RMSE[i] = sqrt(sum((x[i, ] - y[i, ])^2, na.rm = TRUE)/ncol(x))
      }
      return(list(R2 = R2, RMSE = RMSE))
    }
    gof <- gof(pred, matx)
    S2e <- modav$"Mean Sq"[5]
    nrep <- length(levels(data$REP))
    en <- length(levels(data$ENV))
    ge <- length(levels(mydf$GEN))
    S2di <- (dij/(en - 2)) - (S2e/nrep)
    amod2 <- anova(lm(Y ~ GEN + ENV, data = data2))
    # amod2 <- anova(model2)
    SSL <- amod2$"Sum Sq"[2]
    SSGxL <- amod2$"Sum Sq"[3]
    SS.L.GxL <- SSL + SSGxL
    SSL.Linear <- (1/length(levels(data$GEN))) * (colSums(matx, na.rm = TRUE) %*% iij)^2/sum(iij^2)
    SS.L.GxL.linear <- sum(bYijIj) - SSL.Linear
    Df <- c(en * ge - 1,
            ge - 1,
            ge * (en - 1),
            1,
            ge - 1,
            ge * (en - 2),
            replicate(length(dij), en - 2),
            en*(nrep - 1) * (ge - 1))
    poolerr <- modav$"Sum Sq"[5]/nrep
    sigma2 <- modav$"Mean Sq"[5]
    dferr <- modav$"Df"[5]
    vbo <- sigma2 / (en * nrep)
    vb1 <- sigma2 / (nrep * sumij2)
    tcal <- (bij - 1) / sqrt(vb1)
    ptcal <- 2 * pt(abs(tcal), dferr, lower.tail = FALSE)
    SSS <- c(sum(amod2$"Sum Sq"),
             amod2$"Sum Sq"[1],
             SSL + SSGxL,
             SSL.Linear,
             SS.L.GxL.linear,
             sum(dij),
             dij,
             poolerr) * nrep
    MSSS <- (SSS/Df)
    FVAL <- c(NA,
              MSSS[2]/MSSS[6],
              NA,
              NA,
              MSSS[5]/MSSS[6],
              NA,
              MSSS[7:(length(MSSS) - 1)]/MSSS[length(MSSS)],
              NA)
    PLINES <- 1 - pf(FVAL[7:(length(MSSS) - 1)], Df[7], Df[length(Df)])
    pval <- c(NA,
              1 - pf(FVAL[2], Df[2], Df[6]),
              NA,
              NA,
              1 - pf(FVAL[5], Df[5], Df[6]),
              NA,
              PLINES,
              NA)
    anovadf <- data.frame(Df,
                          `Sum Sq` = SSS,
                          `Mean Sq` = MSSS,
                          `F value` = FVAL,
                          `Pr(>F)` = pval,
                          check.names = FALSE)
    rownames(anovadf) <- c("Total", "GEN", "ENV + (GEN x ENV)", "ENV (linear)",
                           " GEN x ENV (linear)", "Pooled deviation",
                           levels(data$GEN), "Pooled error")
    temp <- structure(list(data = iamb2,
                           anova = as_tibble(rownames_to_column(anovadf, "SV")),
                           regression = tibble(GEN = levels(mydf$GEN),
                                               b0 = apply(matx, 1, mean, na.rm = TRUE),
                                               b1 = as.numeric(bij),
                                               `t(b1=1)` = tcal,
                                               pval_t = ptcal,
                                               s2di = as.numeric(S2di),
                                               `F(s2di=0)` = FVAL[7:(length(FVAL) - 1)],
                                               pval_f = PLINES,
                                               RMSE = gof$RMSE,
                                               R2 = gof$R2),
                           bo_variance = vbo,
                           b1_variance = vb1),
                      class = "eberhart_russell")
    if (verbose == TRUE) {
      cli::cli_progress_update(id = pb, set = var, force = TRUE)
    }
    listres[[paste(names(vars[var]))]] <- temp
  }
  return(structure(listres, class = "eberhart_russell"))
}

#' Regression-based stability analysis (Legacy Alias)
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#' `ge_reg()` was deprecated to match cleaner, un-prefixed snake_case naming conventions.
#' Please transition script targets over to using `eberhart_russell()`.
#'
#' @inheritParams eberhart_russell
#' @export
#' @keywords internal
ge_reg <- function(.data, env, gen, rep, resp, verbose = TRUE) {
  deprecated_warning("1.20.0", "ge_reg()", "eberhart_russell()")
  eberhart_russell(
    .data = .data,
    env = {{env}},
    gen = {{gen}},
    rep = {{rep}},
    resp = {{resp}},
    verbose = verbose
  )
}

#' Plot an object of class eberhart_russell
#'
#' Plot the regression model generated by the function `eberhart_russell`.
#'
#'
#' @param x An object of class `ge_factanal`
#' @param var The variable to plot. Defaults to `var = 1` the first
#'   variable of `x`.
#' @param type The type of plot to show. `type = 1` produces a plot with
#'   the environmental index in the x axis and the genotype mean yield in the y
#'   axis. `type = 2` produces a plot with the response variable in the x
#'   axis and the slope/deviations of the regression in the y axis.
#' @param plot_theme The graphical theme of the plot. Default is
#'   `plot_theme = theme_metan_minimal()`. For more details, see
#'   [ggplot2::theme()].
#' @param x.lim The range of x-axis. Default is `NULL` (maximum and minimum
#'   values of the data set). New arguments can be inserted as `x.lim =
#'   c(x.min, x.max)`.
#' @param x.breaks The breaks to be plotted in the x-axis. Default is
#'   `authomatic breaks`. New arguments can be inserted as `x.breaks =
#'   c(breaks)`
#' @param x.lab The label of x-axis. Each plot has a default value. New
#'   arguments can be inserted as `x.lab = "my label"`.
#' @param y.lim The range of x-axis. Default is `NULL`. The same arguments
#'   than `x.lim` can be used.
#' @param y.breaks The breaks to be plotted in the x-axis. Default is
#'   `authomatic breaks`. The same arguments than `x.breaks` can be
#'   used.
#' @param y.lab The label of y-axis. Each plot has a default value. New
#'   arguments can be inserted as `y.lab = "my label"`.
#' @param leg.position The position of the legend.
#' @param size.tex.lab The size of the text in the axes text and labels. Default
#'   is `12`.
#' @param ... Currently not used..
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @seealso [ge_factanal()]
#' @method plot eberhart_russell
#' @return An object of class `gg, ggplot`.
#' @export
#' @examples
#' \donttest{
#' library(metan)
#' model <- ge_reg(data_ge2, ENV, GEN, REP, PH)
#' plot(model)
#' }
#'
plot.eberhart_russell <- function(x,
                        var = 1,
                        type = 1,
                        plot_theme = theme_metan_minimal(),
                        x.lim = NULL,
                        x.breaks = waiver(),
                        x.lab = NULL,
                        y.lim = NULL,
                        y.breaks = waiver(),
                        y.lab = NULL,
                        leg.position = "right",
                        size.tex.lab = 12,
                        ...){
  x <- x[[var]]
  if(!type  %in% c(1, 2)){
    cli::cli_abort("Argument 'type' must be either 1 or 2")
  }
  if(type == 1){
    y.lab <- ifelse(missing(y.lab), "Response variable", y.lab)
    x.lab <- ifelse(missing(x.lab), "Environmental index", x.lab)
    p <-
      ggplot(x$data, aes(x = IndAmb, y = Y))+
      geom_point(aes(colour = GEN), size = 1.5)+
      geom_smooth(aes(colour = GEN), method = "lm", formula = y ~ x, se = FALSE)+
      theme_bw()+
      labs(x = x.lab, y = y.lab)+
      plot_theme %+replace%
      theme(axis.text = element_text(size = size.tex.lab, colour = "black"),
            axis.title = element_text(size = size.tex.lab, colour = "black"),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(.2, "cm"),
            legend.position = leg.position)
    return(p)
  }
  if(type == 2){
    y.lab <- ifelse(missing(y.lab), "Slope of the regression", y.lab)
    x.lab <- ifelse(missing(x.lab), "Response variable", x.lab)
    p <-
      ggplot(x$regression, aes(x = b0, y = b1))+
      geom_point(size = 1.5)+
      geom_hline(yintercept = mean(x$regression$b1))+
      geom_text_repel(aes(label = GEN))+
      labs(x = x.lab, y = y.lab) +
      plot_theme

    p2 <-
      ggplot(x$regression, aes(x = b0, y = s2di))+
      geom_point(size = 1.5)+
      geom_hline(yintercept = mean(x$regression$s2di))+
      geom_text_repel(aes(label = GEN))+
      labs(x = x.lab, y = "Deviations from the regression") +
      plot_theme

    p + p2

  }
}

#' Print an object of class eberhart_russell
#'
#' Print the `eberhart_russell` object in two ways. By default, the results are shown
#' in the R console. The results can also be exported to the directory into a
#' *.txt file.
#'
#' @param x An object of class `eberhart_russell`.
#' @param export A logical argument. If `TRUE`, a *.txt file is exported to
#'   the working directory.
#' @param file.name The name of the file if `export = TRUE`
#' @param digits The significant digits to be shown.
#' @param ... Options used by the tibble package to format the output. See
#'   [`tibble::print()`][tibble::formatting] for more details.
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @method print eberhart_russell
#' @export
#' @examples
#' \donttest{
#'
#' library(metan)
#' model <- ge_reg(data_ge2, ENV, GEN, REP, PH)
#' print(model)
#' }
print.eberhart_russell <- function(x, export = FALSE, file.name = NULL, digits = 3, ...) {
  opar <- options(pillar.sigfig = digits)
  on.exit(options(opar))
  if (export == TRUE) {
    file.name <- ifelse(is.null(file.name) == TRUE, "ge_reg print", file.name)
    sink(paste0(file.name, ".txt"))
  }
  for (i in 1:length(x)) {
    var <- x[[i]]
    cli::cli_h1("Variable {names(x)[i]}")
    cli::cli_h2("Joint-regression Analysis of variance")
    print(var$anova)
    cli::cli_h2("Regression parameters")
    print(var$regression)
    cli::cli_inform("Variance of b0: {var[['bo_variance']]}")
    cli::cli_inform("Variance of b1: {var[['b1_variance']]}")
    cli::cli_text("")
  }
  if (export == TRUE) {
    sink()
  }
}

