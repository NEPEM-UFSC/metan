#' Stability analysis based on Plaisted and Peterson (1959)
#' @description
#' `r badge('stable')`
#'
#' The function computes phenotypic stability based on the method proposed by
#' Plaisted and Peterson (1959). It characterizes stability as the arithmetic mean
#' of the variance components of the genotype-environment interaction isolated
#' pairwise between a given genotype and all other remaining genotypes in the trial.
#'
#' @details
#' The Plaisted and Peterson method operates under a static (biological) concept of
#' stability. A genotype is classified as stable if its presence within the population
#' minimizes the total variance of the GxE interaction matrix.
#'
#' For each pairwise combination of genotype \eqn{i} and genotype \eqn{i'}, the interaction
#' variance component \eqn{\sigma^2_{ga_{ii'}}} is partitioned. The overall stability index
#' (\eqn{\theta_i}) for a single cultivar is calculated as:
#'
#' \deqn{\theta_i = \frac{1}{I - 1} \sum_{i' \neq i} \sigma^2_{ga_{ii'}}}
#'
#' Where \eqn{I} is the total number of genotypes. The individual pairwise variance
#' components are computed by subtracting the pooled experimental error (\eqn{QMR})
#' from the pairwise interaction Mean Square:
#'
#' \deqn{\sigma^2_{ga_{ii'}} = \frac{\left[ \frac{SS(g_{ii'} \times A)}{J - 1} \right] - QMR}{K}}
#'
#' Where \eqn{J} is the total number of environments, \eqn{QMR} is the residual mean square
#' from the joint global ANOVA table, and \eqn{K} is the replication factor. The pairwise
#' interaction sum of squares is given by:
#'
#' \deqn{SS(g_{ii'} \times A) = \frac{K}{2} \left[ d^2_{ii'} - \frac{1}{J}(Y_{i.} - Y_{i'.})^2 \right]}
#'
#' Where \eqn{d^2_{ii'}} is the squared Euclidean distance between the performance profiles
#' of the two genotypes across environments:
#'
#' \deqn{d^2_{ii'} = \sum_{j} (Y_{ij} - Y_{i'j})^2}
#'
#' Cultivars displaying the lowest values of \eqn{\theta_i} contribute less to the population's
#' GxE noise and are interpreted as the most stable genotypes.
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
#' @return An object of class `plaisted_peterson` containing the results for each
#'   variable used in the argument `resp`.
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @references Plaisted, R.L., and L.C. Peterson. 1959. A technique for
#'   evaluating the ability of selections to yield consistently in different
#'   locations or seasons. American Potato Journal 36(11): 381–385.
#'   \doi{10.1007/BF02852735}

#' @export
#' @examples
#' \donttest{
#' library(metan)
#'  plaisted_peterson(data_ge, ENV, GEN, REP, GY)
#'}
#'
plaisted_peterson <- function(.data, env, gen, rep, resp, verbose = TRUE) {
  factors  <-
    .data |>
    select({{env}}, {{gen}}, {{rep}}) |>
    mutate(across(everything(), as.factor))
  vars <-
    .data |>
    select({{resp}}, -names(factors)) |>
    select_numeric_cols()
  factors <- factors |> set_names(c("ENV", "GEN", "REP"))
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
    joint_an <- anova_joint(data, ENV, GEN, REP, Y, verbose = FALSE)
    ngen <- nlevels(data$GEN)

    # Expected Mean Square for error from ANOVA
    if ("Residuals" %in% joint_an$Y$anova$Source) {
      qmr <- joint_an$Y$anova |> dplyr::filter(Source == "Residuals") |> dplyr::pull("Mean Sq")
    } else {
      qmr <- joint_an$Y$anova[nrow(joint_an$Y$anova) - 3, 4]
    }

    combs <- combn(ngen, 2)
    mat_cells <- stats::xtabs(~ GEN + ENV, data = data)
    df2 <- make_mat(data, GEN, ENV, Y)
    gemat <- as.matrix(df2)
    gens <- rownames(gemat)
    dists <- numeric(ncol(combs))

    for (j in 1:ncol(combs)) {
      genA <- combs[1, j]
      genB <- combs[2, j]

      y_A <- gemat[genA, ]
      y_B <- gemat[genB, ]

      n_A <- mat_cells[genA, ]
      n_B <- mat_cells[genB, ]

      # Filtra os ambientes onde AMBOS os genótipos estão presentes (Evita NAs)
      valid <- !is.na(y_A) & !is.na(y_B) & (n_A > 0) & (n_B > 0)
      J_v <- sum(valid)

      if (J_v > 1) {
        # Diferença real entre as médias no ambiente
        D <- y_A[valid] - y_B[valid]

        # Peso Harmônico efetivo da casela: (nA * nB) / (nA + nB)
        W <- (n_A[valid] * n_B[valid]) / (n_A[valid] + n_B[valid])
        sum_W <- sum(W)

        # Soma de Quadrados de GxE Exata para o Par (Ponderada)
        SS_GE_pair <- sum(W * D^2) - (sum(W * D)^2 / sum_W)

        # Quadrado Médio de GxE do Par
        MS_GE_pair <- SS_GE_pair / (J_v - 1)

        # Coeficiente K efetivo para extração do componente de variância
        K_eff <- (sum_W - sum(W^2) / sum_W) / (J_v - 1)

        # Componente de variância par a par (theta_ij)
        # Dividido por (2 * K_eff) para espelhar a álgebra do divisor 'K' original
        dists[j] <- (MS_GE_pair - qmr) / (2 * K_eff)
      } else {
        dists[j] <- NA # Caso extremo de genótipos sem ambientes em comum
      }
    }
    mat <- matrix(NA, nrow = ngen, ncol = ngen)
    mat[lower.tri(mat)] <- dists
    mat <- make_sym(mat)
    mat_res <-
      as.data.frame(mat) |>
      mutate(theta = apply(mat, 1, mean, na.rm = TRUE),
             theta_perc = theta / sum(theta) * 100)
    rownames(mat_res) <- c(gens)
    colnames(mat_res) <- c(gens, "theta", "theta_prop")
    if (verbose == TRUE) {
      cli::cli_progress_update(id = pb, set = var, force = TRUE)
    }
    listres[[paste(names(vars[var]))]] <- mat_res |> rownames_to_column("GEN")
  }
  return(structure(listres, class = "plaisted_peterson"))
}



#' Print an object of class plaisted_peterson
#'
#' Print the `plaisted_peterson` object in two ways. By default, the results
#' are shown in the R console. The results can also be exported to the directory
#' into a *.txt file.
#'
#'
#' @param x The `plaisted_peterson` x
#' @param export A logical argument. If `TRUE`, a *.txt file is exported to
#'   the working directory.
#' @param file.name The name of the file if `export = TRUE`
#' @param digits The significant digits to be shown.
#' @param ... Options used by the tibble package to format the output. See
#'   [`tibble::print()`][tibble::formatting] for more details.
#' @author Tiago Olivoto \email{tiagoolivoto@@gmail.com}
#' @method print plaisted_peterson
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
print.plaisted_peterson <- function(x, export = FALSE, file.name = NULL, digits = 3, ...) {
  opar <- options(pillar.sigfig = digits)
  on.exit(options(opar))
  if (export == TRUE) {
    file.name <- ifelse(is.null(file.name) == TRUE, "plaisted_peterson print", file.name)
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
