#' Stability analysis based on Wricke's model
#' @description
#' `r badge('stable')`
#'
#' The function computes the ecovalence (\eqn{W_i^2}) proposed by Wricke (1962, 1965)
#' for stability analysis. Ecovalence quantifies the structural contribution of
#' individual genotypes to the total Genotype-by-Environment (GxE) interaction
#' sum of squares (\eqn{SS_{G\times E}}).
#'
#' In unbalanced multi-environment networks characterized by varying cell replications
#' or missing observations, the algorithm automatically adjusts by calculating the
#' interaction deviations from predicted Least-Square Means (LS-means/EM-means) and
#' weighting individual contributions by their respective environmental cell replication size.
#'
#' @details
#' Under balanced designs, Wricke's Ecovalence (\eqn{W_i^2}) for the \eqn{i}-th genotype
#' evaluated across \eqn{E} environments is calculated as follows:
#'
#' \deqn{W_i^2 = \sum_{j=1}^{E} (Y_{ij} - \bar{Y}_{i\cdot} - \bar{Y}_{\cdot j} + \bar{Y}_{\cdot \cdot})^2}{W_i^2 = \sum (Y_{ij} - Y_{i.} - Y_{.j} + Y_{..})^2}
#'
#' Where \eqn{Y_{ij}} is the observed mean phenotypic value of genotype \eqn{i} in
#' environment \eqn{j}; \eqn{\bar{Y}_{i\cdot}} is the marginal mean of genotype \eqn{i} across
#' all environments; \eqn{\bar{Y}_{\cdot j}} is the marginal mean of environment \eqn{j}
#' across all genotypes; and \eqn{\bar{Y}_{\cdot \cdot}} is the overall grand mean.
#'
#' For unbalanced designs, the non-orthogonal nature of the data is accounted for
#' by extracting adjusted cell predictions (\eqn{\hat{Y}_{ij}}) from a linear mixed model
#' framework, executing the following weighted formulation:
#'
#' \deqn{W_i^2 = \sum_{j=1}^{E} n_{ij} \left( \hat{Y}_{ij} - \hat{\mu}_{i\cdot} - \hat{\mu}_{\cdot j} + \hat{\mu}_{\cdot\cdot} \right)^2}{W_i^2 = \sum n_{ij} (\hat{Y}_{ij} - \hat{\mu}_{i.} - \hat{\mu}_{.j} + \hat{\mu}_{..})^2}
#'
#' Where \eqn{n_{ij}} represents the specific number of replicates/cells for genotype \eqn{i}
#' within environment \eqn{j}, and the parameters wrapped with hats (\eqn{\hat{\cdot}}) denote
#' the predicted marginal parameters estimated over a balanced reference grid. Genotypes
#' exhibiting lower ecovalence values possess smaller deviations from the main additive
#' effects, indicating high performance predictability and structural stability.
#'
#' @param .data The dataset containing the columns related to Environments,
#'   Genotypes, replication/block and response variable(s).
#' @param env The name of the column that contains the levels of the
#'   environments.
#' @param gen The name of the column that contains the levels of the genotypes.
#' @param rep The name of the column that contains the levels of the
#'   replications/blocks.
#' @param resp The response variable(s). To analyze multiple variables in a
#'   single procedure use, for example, `resp = c(var1, var2, var3)`.
#' @param verbose Logical argument. If `verbose = FALSE` the code will run
#'   silently.
#' @return An object of class `ecovalence` containing the results for each
#'   variable used in the argument `resp`.
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @references Wricke, G. 1962. Über eine Methode zur Erfassung der ökologischen
#'   Streubreite in Feldversuchen. Z. Pflanzenzuecht. 47: 92-96.
#'
#'  Wricke, G. 1965. Zur berechnung der okovalenz bei sommerweizen
#'   und hafer. Z. Pflanzenzuchtg 52:127-138.
#' @export
#' @examples
#' \donttest{
#' library(metan)
#'out <- ecovalence(data_ge2,
#'                  env = ENV,
#'                  gen = GEN,
#'                  rep = REP,
#'                  resp = PH)
#'}
#'
ecovalence <- function(.data, env, gen, rep = NULL, resp, verbose = TRUE) {
  if (missing(rep)) {
    factors  <-
      .data |>
      select({{env}}, {{gen}}) |>
      mutate(across(everything(), as.factor))
    factors <- factors |> set_names("ENV", "GEN")
  } else {
    factors  <-
      .data |>
      select({{env}}, {{gen}}, {{rep}}) |>
      mutate(across(everything(), as.factor))
    factors <- factors |> set_names("ENV", "GEN", "REP")
  }
  vars <-
    .data |>
    select({{resp}}) |>
    select_numeric_cols()
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
    data <- factors |>
      mutate(Y = vars[[var]])
    if(has_na(data)){
      data <- remove_rows_na(data)
      has_text_in_num(data)
    }
    data2 <- data |>
      mean_by(ENV, GEN) |>
      as.data.frame()
    data3 <- mutate(data2,
                    ge = residuals(lm(Y ~ ENV + GEN, data = data2)))
    ge_effect <- make_mat(data3, GEN, ENV, ge)
    if (missing(rep)) {
      Ecoval <- rowSums(ge_effect^2, na.rm = TRUE)
    } else {
      mat_cells <- xtabs(~ GEN + ENV, data = data2)
      Ecoval <- rowSums(ge_effect^2 * mat_cells, na.rm = TRUE)
    }
    Ecov_perc <- (Ecoval/sum(Ecoval)) * 100
    rank <- rank(Ecoval)
    temp <- cbind(ge_effect, Ecoval, Ecov_perc, rank) |>
      as_tibble(rownames = NA) |>
      rownames_to_column("GEN")
    if (verbose == TRUE) {
      cli::cli_progress_update(id = pb, set = var, force = TRUE)
    }
    listres[[paste(names(vars[var]))]] <- temp
  }
  return(structure(listres, class = "ecovalence"))
}


#' Print an object of class ecovalence
#'
#' Print the `ecovalence` object in two ways. By default, the results
#' are shown in the R console. The results can also be exported to the directory
#' into a *.txt file.
#'
#'
#' @param x The `ecovalence` x
#' @param export A logical argument. If `TRUE`, a *.txt file is exported to
#'   the working directory.
#' @param file.name The name of the file if `export = TRUE`
#' @param digits The significant digits to be shown.
#' @param ... Options used by the tibble package to format the output. See
#'   [`tibble::print()`][tibble::formatting] for more details.
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @method print ecovalence
#' @export
#' @examples
#' \donttest{
#' library(metan)
#' eco <- ecovalence(data_ge2,
#'                   env = ENV,
#'                   gen = GEN,
#'                   rep = REP,
#'                   resp = PH)
#' print(eco)
#' }
print.ecovalence <- function(x, export = FALSE, file.name = NULL, digits = 3, ...) {
  opar <- options(pillar.sigfig = digits)
  on.exit(options(opar))
  if (export == TRUE) {
    file.name <- ifelse(is.null(file.name) == TRUE, "ecovalence print", file.name)
    sink(paste0(file.name, ".txt"))
  }
  for (i in 1:length(x)) {
    var <- x[[i]]
    cli::cli_h1("Variable {names(x)[i]}")
    print(var)
  }
  cli::cli_text("")
  if (export == TRUE) {
    sink()
  }
}
