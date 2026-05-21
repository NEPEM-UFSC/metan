#' Calculate Growing Degree Days (GDD) and Fraction of Radiation Use Efficiency (FRUE)
#'
#' Computes daily thermal accumulation using Ometto's non-linear method along with
#' a scaled Fraction of Radiation Use Efficiency (FRUE) penalty modifier based on daily
#' mean temperatures.
#'
#' @param df A data frame containing meteorological variables. Must contain \code{T2M_MAX}
#'   and \code{T2M_MIN}, or alternatively an aggregate \code{T2M} mean column.
#' @param Tbase Numeric. Lower base temperature threshold (minimum growth temperature).
#'   Default is \code{10}.
#' @param Tceil Numeric. Upper ceiling cutoff temperature threshold (growth ceases).
#'   Default is \code{40}.
#' @param Topt1 Numeric. Lower boundary of the optimal thermal range for growth.
#'   Default is \code{26}.
#' @param Topt2 Numeric. Upper boundary of the optimal thermal range for growth.
#'   Default is \code{32}.
#'
#' @return A data frame containing calculated columns:
#'   \item{Tmed}{Daily mean calculated temperature.}
#'   \item{GDD}{Daily thermal accumulation calculation based on the Ometto equations.}
#'   \item{FRUE}{Fraction of radiation use efficiency scalar clamped strictly between 0 and 1.}
#'   \item{GDD_CUMSUM}{Cumulative GDD accumulation grouped dynamically by \code{ENV} (if available).}
#'   \item{RTA_CUMSUM}{Optional cumulative calculation for standard thermal metrics (if \code{RTA} is present).}
#' @export
#'
#' @examples
#' \dontrun{
#' weather_data <- data.frame(
#'   ENV = "Location_A",
#'   DATE = seq.Date(as.Date("2026-01-01"), as.Date("2026-01-05"), by = "day"),
#'   T2M_MAX = c(22, 28, 35, 41, 12),
#'   T2M_MIN = c(12, 16, 22, 25, 8)
#' )
#' gdd_ometto_frue(weather_data)
#' }
gdd_ometto_frue <- function(df,
                            Tbase = 10,
                            Tceil = 40,
                            Topt1 = 26,
                            Topt2 = 32) {

  # Ensure required columns exist
  required_cols <- c("T2M_MAX", "T2M_MIN")
  if (!all(required_cols %in% names(df))) {
    if ("T2M" %in% names(df)) {
      warning("T2M_MIN/T2M_MAX not found, attempting to use T2M assuming it represents daily mean.", call. = FALSE)
      df <- df |> dplyr::mutate(T2M_MAX = T2M, T2M_MIN = T2M)
    } else {
      stop("Required columns for GDD calculation are missing: T2M_MIN, T2M_MAX. ",
           "Ensure they are selected or derived (e.g., from hourly T2M).")
    }
  }

  Tb <- Tbase
  TB <- Tceil

  df_intermediate <- df |>
    dplyr::mutate(
      T2M_MAX_num = suppressWarnings(as.numeric(T2M_MAX)),
      T2M_MIN_num = suppressWarnings(as.numeric(T2M_MIN)),
      T2M_num = if ("T2M" %in% names(df)) suppressWarnings(as.numeric(T2M)) else NA_real_
    )

  df_intermediate <- df_intermediate |>
    dplyr::mutate(
      Tmed = dplyr::case_when(
        !is.na(T2M_num) ~ T2M_num,
        !is.na(T2M_MAX_num) & !is.na(T2M_MIN_num) ~ (T2M_MAX_num + T2M_MIN_num) / 2,
        TRUE ~ NA_real_
      )
    )

  df_out <- df_intermediate |>
    dplyr::mutate(
      GDD = dplyr::case_when(
        is.na(T2M_MAX_num) | is.na(T2M_MIN_num) ~ NA_real_,
        T2M_MAX_num == T2M_MIN_num & T2M_MAX_num >= Tb & T2M_MAX_num <= TB ~ T2M_MAX_num - Tb,
        T2M_MAX_num == T2M_MIN_num & T2M_MAX_num < Tb ~ 0,
        T2M_MAX_num == T2M_MIN_num & T2M_MAX_num > TB ~ TB - Tb,
        TB > T2M_MAX_num & T2M_MAX_num > T2M_MIN_num & T2M_MIN_num >= Tb ~ (T2M_MAX_num + T2M_MIN_num) / 2 - Tb,
        TB > T2M_MAX_num & T2M_MAX_num > Tb & Tb > T2M_MIN_num ~ ((T2M_MAX_num - Tb)^2) / (2 * (T2M_MAX_num - T2M_MIN_num)),
        TB > Tb & Tb >= T2M_MAX_num & T2M_MAX_num >= T2M_MIN_num ~ 0,
        T2M_MAX_num > TB & TB > T2M_MIN_num & T2M_MIN_num >= Tb ~
          ( (T2M_MAX_num + T2M_MIN_num)/2 - Tb ) - ( (T2M_MAX_num - TB)^2 / (2*(T2M_MAX_num - T2M_MIN_num)) ),
        T2M_MAX_num > TB & TB > Tb & Tb > T2M_MIN_num ~
          ( (T2M_MAX_num - Tb)^2 - (T2M_MAX_num - TB)^2 ) / (2 * (T2M_MAX_num - T2M_MIN_num)),
        T2M_MAX_num >= Tb & T2M_MIN_num >= TB ~ TB - Tb,
        T2M_MAX_num >= TB & T2M_MIN_num < Tb ~
          ( (T2M_MAX_num - Tb)^2 - (T2M_MAX_num - TB)^2 ) / (2 * (T2M_MAX_num - T2M_MIN_num)),
        TRUE ~ 0
      ),
      GDD = pmax(0, GDD),

      FRUE = dplyr::case_when(
        is.na(Tmed) | Tmed <= Tb | Tmed >= TB ~ 0,
        Tmed < Topt1 ~ (Tmed - Tb) / (Topt1 - Tb),
        Tmed > Topt2 ~ (TB - Tmed) / (TB - Topt2),
        TRUE ~ 1
      ),
      FRUE = pmax(0, pmin(FRUE, 1))
    ) |>
    dplyr::select(-dplyr::any_of(c("T2M_MAX_num", "T2M_MIN_num", "T2M_num")))

  if ("ENV" %in% names(df_out) && "DATE" %in% names(df_out)) {
    df_out <- df_out |>
      dplyr::arrange(ENV, DATE) |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(GDD_CUMSUM = cumsum(ifelse(is.na(GDD), 0, GDD))) |>
      dplyr::ungroup()

    if ("RTA" %in% names(df_out)) {
      df_out <- df_out |>
        dplyr::arrange(ENV, DATE) |>
        dplyr::group_by(ENV) |>
        dplyr::mutate(RTA_CUMSUM = cumsum(ifelse(is.na(RTA), 0, RTA))) |>
        dplyr::ungroup()
    }
  } else if ("DATE" %in% names(df_out)) {
    df_out <- df_out |>
      dplyr::arrange(DATE) |>
      dplyr::mutate(GDD_CUMSUM = cumsum(ifelse(is.na(GDD), 0, GDD)))
    if ("RTA" %in% names(df_out)) {
      df_out <- df_out |>
        dplyr::arrange(DATE) |>
        dplyr::mutate(RTA_CUMSUM = cumsum(ifelse(is.na(RTA), 0, RTA)))
    }
  } else {
    warning("Cannot calculate cumulative sums without ENV and/or DATE columns.", call. = FALSE)
  }

  return(df_out)
}


#' Calculate Chilling Hours using Weinberger's Method
#'
#' Accumulates chilling hours by summing the hours where temperatures drop strictly below
#' 7.2 degrees Celsius using hourly observational datasets.
#'
#' @param data A data frame structured at an hourly scale. Must include an hourly column
#'   (\code{HR} or \code{HOUR}) and temperature matrix (\code{T2M}).
#'
#' @return A data frame with parsed daily sums (\code{ch_w_daily}) and continuous run tracking totals
#'   (\code{ch_w_accum}).
#' @export
#'
calculate_weinberger_ch <- function(data) {
  if (!("HR" %in% colnames(data) || "HOUR" %in% colnames(data))) {
    cli::cli_abort("Data is not at an hourly scale. Use {.code get_climate(..., scale = 'hourly')}")
  }
  if (!("T2M" %in% colnames(data))) {
    cli::cli_abort("{.field T2M} column is required for chilling hours calculation (Weinberger).")
  }

  hr_col <- if ("HR" %in% colnames(data)) "HR" else "HOUR"
  has_env <- "ENV" %in% colnames(data)
  if (!has_env) {
    cli::cli_alert_warning("{.field ENV} column not found. Calculating accumulation globally.")
  }

  ch_data <- data |>
    dplyr::mutate(ch_w = ifelse(!is.na(T2M) & T2M < 7.2, 1, 0))

  if (!"DATE" %in% colnames(ch_data)) {
    if (all(c("YEAR", "MO", "DY") %in% colnames(ch_data))) {
      date_str <- paste(ch_data$YEAR, formatC(as.numeric(ch_data$MO), width = 2, flag = "0"), formatC(as.numeric(ch_data$DY), width = 2, flag = "0"), sep = "-")
      ch_data$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
      if(any(is.na(ch_data$DATE))) cli::cli_warn("Could not create {.field DATE} column reliably for daily CH grouping.")
    } else {
      cli::cli_warn("Cannot group by day for daily CH sum (Weinberger): Missing {.field YEAR}, {.field MO}, {.field DY} or {.field DATE} columns.")
    }
  }

  if ("DATE" %in% colnames(ch_data) && !any(is.na(ch_data$DATE))) {
    grouping_vars <- if(has_env) c("ENV", "DATE") else "DATE"
    ch_data <- ch_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
      dplyr::mutate(ch_w_daily = sum(ch_w, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  if (has_env) {
    ch_data <- ch_data |>
      dplyr::arrange(ENV, DATE, .data[[hr_col]]) |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(ch_w_accum = cumsum(ch_w)) |>
      dplyr::ungroup()
  } else {
    ch_data <- ch_data |>
      dplyr::arrange(DATE, .data[[hr_col]]) |>
      dplyr::mutate(ch_w_accum = cumsum(ch_w))
  }

  return(ch_data)
}


#' Calculate Chilling Units using the Utah Model
#'
#' Evaluates hourly temperature matrix fields against weighted chilling efficiency profiles.
#' Accumulations are bounded at 0 to avoid continuous net negative metrics.
#'
#' @param data A data frame structured at an hourly scale containing temperature values (\code{T2M}).
#'
#' @return A data frame containing calculated columns \code{CH_Utah}, \code{CH_Utah_daily}, and non-negative cumulative values \code{CH_Utah_accum}.
#' @export
#'
calculate_utah_ch <- function(data) {
  if (!("HR" %in% colnames(data) || "HOUR" %in% colnames(data))) {
    cli::cli_abort("Data is not at an hourly scale. Use {.code get_climate(..., scale = 'hourly')}")
  }
  if (!("T2M" %in% colnames(data))) {
    cli::cli_abort("{.field T2M} column is required for chilling hours calculation (Utah).")
  }

  hr_col <- if ("HR" %in% colnames(data)) "HR" else "HOUR"
  has_env <- "ENV" %in% colnames(data)
  if (!has_env) {
    cli::cli_alert_warning("{.field ENV} column not found for Utah calculation. Calculating accumulation globally.")
  }

  ch_data <- data |>
    dplyr::mutate(
      CH_Utah = dplyr::case_when(
        is.na(T2M) ~ 0,
        T2M < 1.4 ~ 0,
        T2M >= 1.4 & T2M < 2.5 ~ 0.5,
        T2M >= 2.5 & T2M < 9.1 ~ 1.0,
        T2M >= 9.1 & T2M < 12.4 ~ 0.5,
        T2M >= 12.4 & T2M < 15.9 ~ 0,
        T2M >= 15.9 & T2M < 18.0 ~ -0.5,
        T2M >= 18.0 ~ -1.0,
        TRUE ~ 0
      )
    )

  if (!"DATE" %in% colnames(ch_data)) {
    if (all(c("YEAR", "MO", "DY") %in% colnames(ch_data))) {
      date_str <- paste(ch_data$YEAR, formatC(as.numeric(ch_data$MO), width = 2, flag = "0"), formatC(as.numeric(ch_data$DY), width = 2, flag = "0"), sep = "-")
      ch_data$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
      if(any(is.na(ch_data$DATE))) cli::cli_warn("Could not create {.field DATE} column reliably for daily CH grouping (Utah).")
    } else {
      cli::cli_warn("Cannot group by day for daily CH sum (Utah): Missing {.field YEAR}, {.field MO}, {.field DY} or {.field DATE} columns.")
    }
  }

  if ("DATE" %in% colnames(ch_data) && !any(is.na(ch_data$DATE))) {
    grouping_vars <- if(has_env) c("ENV", "DATE") else "DATE"
    ch_data <- ch_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
      dplyr::mutate(CH_Utah_daily = sum(CH_Utah, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  if (has_env) {
    ch_data <- ch_data |>
      dplyr::arrange(ENV, DATE, .data[[hr_col]]) |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(CH_Utah_accum = cumsum(CH_Utah)) |>
      dplyr::mutate(CH_Utah_accum = pmax(0, CH_Utah_accum)) |>
      dplyr::ungroup()
  } else {
    ch_data <- ch_data |>
      dplyr::arrange(DATE, .data[[hr_col]]) |>
      dplyr::mutate(CH_Utah_accum = cumsum(CH_Utah)) |>
      dplyr::mutate(CH_Utah_accum = pmax(0, CH_Utah_accum))
  }

  return(ch_data)
}


#' Calculate Chilling Units using the North Carolina Model
#'
#' Estimates cold units (CU) specifically adjusted for mild subtropical winter profiles.
#' Unlike Utah models, accumulated indices can remain net negative to accurately reflect
#' heat-induced dormancy loss.
#'
#' @param data A data frame structured at an hourly scale containing temperature values (\code{T2M}).
#'
#' @return A data frame containing calculated columns \code{CH_NC}, \code{CH_NC_daily}, and cumulative run outputs \code{CH_NC_accum}.
#' @export
#'
calculate_nc_ch <- function(data) {
  if (!("HR" %in% colnames(data) || "HOUR" %in% colnames(data))) {
    cli::cli_abort("Data is not at an hourly scale. Use {.code get_climate(..., scale = 'hourly')}")
  }
  if (!("T2M" %in% colnames(data))) {
    cli::cli_abort("{.field T2M} column is required for chilling hours calculation (North Carolina).")
  }

  hr_col <- if ("HR" %in% colnames(data)) "HR" else "HOUR"
  has_env <- "ENV" %in% colnames(data)
  if (!has_env) {
    cli::cli_alert_info("{.field ENV} column not found for North Carolina calculation. Calculating accumulation globally.")
  }

  ch_data <- data |>
    dplyr::mutate(
      CH_NC = dplyr::case_when(
        is.na(T2M) ~ 0,
        T2M < 1.4 ~ 0,
        T2M >= 1.4 & T2M < 7.2 ~ 1.0,
        T2M >= 7.2 & T2M < 13.0 ~ 0.5,
        T2M >= 13.0 & T2M < 16.5 ~ 0.0,
        T2M >= 16.5 & T2M < 19.0 ~ -0.5,
        T2M >= 19.0 & T2M < 20.7 ~ -1.0,
        T2M >= 20.7 ~ -2.0,
        TRUE ~ 0
      )
    )

  if (!"DATE" %in% colnames(ch_data)) {
    if (all(c("YEAR", "MO", "DY") %in% colnames(ch_data))) {
      date_str <- paste(ch_data$YEAR, formatC(as.numeric(ch_data$MO), width = 2, flag = "0"), formatC(as.numeric(ch_data$DY), width = 2, flag = "0"), sep = "-")
      ch_data$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
      if (any(is.na(ch_data$DATE))) cli::cli_warn("Could not create {.field DATE} column reliably for daily CH grouping (NC).")
    } else {
      cli::cli_warn("Cannot group by day for daily CH sum (NC): Missing {.field YEAR}, {.field MO}, {.field DY} or {.field DATE} columns.")
    }
  }

  if ("DATE" %in% colnames(ch_data) && !any(is.na(ch_data$DATE))) {
    grouping_vars <- if (has_env) c("ENV", "DATE") else "DATE"
    ch_data <- ch_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
      dplyr::mutate(CH_NC_daily = sum(CH_NC, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  if (has_env) {
    ch_data <- ch_data |>
      dplyr::arrange(ENV, DATE, .data[[hr_col]]) |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(CH_NC_accum = cumsum(CH_NC)) |>
      dplyr::ungroup()
  } else {
    ch_data <- ch_data |>
      dplyr::arrange(DATE, .data[[hr_col]]) |>
      dplyr::mutate(CH_NC_accum = cumsum(CH_NC))
  }

  return(ch_data)
}


#' Fetch and Process NASA POWER Agro-Climatology API Queries
#'
#' Orchestrates programmatic single or multi-point spatial downloads from the
#' NASA POWER meteorological repository with modular caching, automatic retry infrastructure,
#' and flexible concurrent parallel execution.
#'
#' @param env Optional vector of unique character identifiers matching spatial coordinates.
#' @param lat Numeric vector representing point location latitudes.
#' @param lon Numeric vector representing point location longitudes.
#' @param start Character or single date object marking retrieval boundaries.
#' @param end Character or single date object marking closure boundaries.
#' @param params Character array specifying parameter short abbreviations to download.
#' @param scale Character selection matching database structures: \code{hourly}, \code{daily}, \code{monthly}, or \code{climatology}.
#' @param cache_service Optional background object validating storage pipelines.
#' @param progress Logical flag showing console indicator animations.
#' @param parallel Logical setting handling parallel process execution tasks.
#' @param workers Integer threshold controlling core distribution limits.
#' @param nasaparams Optional parsed local configuration matrix filtering variables.
#' @param cache_key Optional text label string tracking cache file namespaces.
#'
#' @return A unified, systematically parsed data frame containing fetched parameter vectors.
#' @export
#' @examples
#' library(metan)
#' \dontrun{
#' clim <-
#' get_climate(
#'  env   = c("Anapolis", "Jatai", "Sorriso"),
#'  lat   = c(-16.3, -17.9, -12.5),
#'  lon   = c(-49,   -51.7, -55.7),
#'  start = c("2025-10-01", "2025-10-12", "2025-10-08"),
#'  end   = c("2026-01-19", "2026-02-09", "2026-01-26"),
#'  scale = "daily"
#')
#'}
#'
get_climate <- function(env = NULL, lat, lon, start, end,
                        params = c("T2M", "T2M_MIN", "T2M_MAX", "T2M_RANGE",
                                   "PRECTOTCORR", "RH2M", "WS2M", "GWETTOP",
                                   "WD2M", "WS2M_MAX", "WS2M_MIN", "WS2M_RANGE",
                                   "ALLSKY_SFC_SW_DWN"),
                        scale = c("daily", "hourly", "monthly", "climatology"),
                        cache_service = NULL,
                        progress = TRUE,
                        parallel = FALSE,
                        workers = 2,
                        nasaparams = NULL,
                        cache_key = NULL) {

  rlang::check_installed(c("glue", "httr2"))

  deg2rad <- function(deg) (deg * pi) / 180
  Ra_fun <- function(J, lat) {
    rlat <- deg2rad(lat)
    fi <- 0.409 * sin((2 * pi / 365) * J - 1.39)
    dr <- 1 + 0.033 * cos(2 * pi / 365 * J)
    ws <- acos(-tan(rlat) * tan(fi))
    Ra <- (1440 / pi) * 0.082 * dr * (ws * sin(rlat) * sin(fi) + cos(rlat) * cos(fi) * sin(ws))
    P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (J - 186)))))
    P_arg <- (sin(0.8333 * pi / 180) + sin(lat * pi / 180) * sin(P)) /
      (cos(lat * pi / 180) * cos(P))
    P_arg <- pmin(pmax(P_arg, -1), 1)
    DL <- 24 - (24 / pi) * acos(P_arg)
    data.frame(Ra = Ra, N = DL)
  }
  vpd <- function(temp, rh) {
    es <- 0.61078 * exp((17.27 * temp) / (temp + 237.3))
    ea <- es * (rh / 100)
    data.frame(ES = es, EA = ea, VPD = es - ea)
  }
  slope_svp <- function(tmed) {
    4098 * (0.6108 * exp((17.27 * tmed) / (tmed + 237.3))) / (tmed + 237.3)^2
  }

  if (length(lat) != length(lon)) {
    cli::cli_abort("Coordinate length mismatch: {.field lat} ({length(lat)}) and {.field lon} ({length(lon)}) must be identical.")
  }
  if (is.null(env)) env <- paste0("ENV", seq_along(lat))
  if (length(env) != length(lat)) {
    cli::cli_abort("Environment name mismatch: {.field env} length must match coordinate size ({length(lat)}).")
  }

  scale    <- rlang::arg_match(scale)
  n_points <- length(lat)
  start_vec <- if (length(start) == 1) rep(start, n_points) else start
  end_vec   <- if (length(end)   == 1) rep(end,   n_points) else end

  nasaparams_path <- system.file("app/www/nasaparams.csv", package = "HTPverse", mustWork = FALSE)
  if (nasaparams_path == "") {
    cli::cli_abort("{.file nasaparams.csv} file not found within package structure.")
  }
  nasaparams <- utils::read.csv(nasaparams_path, stringsAsFactors = FALSE)

  fetch_data_point <- function(lat_i, lon_i, env_i, start_i, end_i, nasaparams, params, scale) {
    tryCatch({
      scale_lowercase <- tolower(scale)
      api_scale <- if (scale_lowercase == "climatology") "climatology" else scale_lowercase

      current_date <- Sys.Date()
      start_date <- as.Date(start_i)
      end_date   <- as.Date(end_i)

      if (start_date > current_date || end_date > current_date) {
        cli::cli_warn("Future dates detected for point ({lat_i}, {lon_i}). Adjusting boundaries back.")
        if (api_scale != "climatology") {
          end_date   <- current_date - 1
          start_date <- end_date - 30
        }
      }

      suitable <- nasaparams[nasaparams$level == scale_lowercase, "abbreviation"]
      valid_params <- intersect(params, suitable)
      if (length(valid_params) == 0) return(NULL)
      params_str_i <- paste(valid_params, collapse = ",")

      base_url <- "https://power.larc.nasa.gov/api/temporal"
      if (api_scale == "monthly") {
        start_fmt <- format(start_date, "%Y")
        end_fmt   <- format(end_date,   "%Y")
        current_year <- as.numeric(format(current_date, "%Y"))
        if (as.numeric(end_fmt) > current_year) end_fmt <- format(current_date, "%Y")
        url <- glue::glue("{base_url}/{api_scale}/point?parameters={params_str_i}&community=AG&longitude={lon_i}&latitude={lat_i}&start={start_fmt}&end={end_fmt}&format=CSV")
      } else if (api_scale %in% c("hourly", "daily")) {
        start_fmt <- format(start_date, "%Y%m%d")
        end_fmt   <- format(end_date,   "%Y%m%d")
        url <- glue::glue("{base_url}/{api_scale}/point?parameters={params_str_i}&community=AG&longitude={lon_i}&latitude={lat_i}&start={start_fmt}&end={end_fmt}&format=CSV")
      } else {
        url <- glue::glue("{base_url}/climatology/point?parameters={params_str_i}&community=AG&longitude={lon_i}&latitude={lat_i}&format=CSV")
      }

      req  <- httr2::request(url) |> httr2::req_options(timeout = 60, ssl_verifypeer = 0)
      resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
      if (is.null(resp) || httr2::resp_status(resp) >= 400) return(NULL)

      content <- httr2::resp_body_string(resp)
      if (grepl("No data was found that matched your query", content, ignore.case = TRUE)) return(NULL)

      tf <- tempfile(fileext = ".csv")
      on.exit(unlink(tf), add = TRUE)
      writeLines(content, tf)

      linhas <- readLines(tf)
      linha_inicio_dados <- which(grepl("-END HEADER-", linhas)) + 1
      dados <- if (length(linha_inicio_dados)) {
        tryCatch(utils::read.csv(tf, skip = linha_inicio_dados - 1, check.names = FALSE), error = function(e) NULL)
      } else {
        tryCatch(utils::read.csv(tf, check.names = FALSE), error = function(e) NULL)
      }
      if (is.null(dados) || !nrow(dados)) return(NULL)

      dados$ENV <- env_i; dados$LAT <- lat_i; dados$LON <- lon_i
      dados[dados == -999] <- NA; dados[dados == -99] <- NA

      if ("YEAR" %in% names(dados) && "DOY" %in% names(dados)) {
        dados$DATE <- tryCatch(as.Date(paste0(dados$YEAR, "-", dados$DOY), format = "%Y-%j"), error = function(e) NA)
      } else if ("YYYYMMDD" %in% names(dados)) {
        dados$DATE <- tryCatch(as.Date(as.character(dados$YYYYMMDD), format = "%Y%m%d"), error = function(e) NA)
      } else if (all(c("YEAR", "MO", "DY") %in% names(dados))) {
        date_str <- paste(dados$YEAR,
                          sprintf("%02d", as.integer(dados$MO)),
                          sprintf("%02d", as.integer(dados$DY)), sep = "-")
        dados$DATE <- tryCatch(as.Date(date_str), error = function(e) NA)
      }

      if ("PRECTOTCORR" %in% names(dados))
        names(dados)[names(dados) == "PRECTOTCORR"] <- "PRECTOT"

      dados
    }, error = function(e) NULL)
  }

  if (!is.null(cache_service)) {
    request_params <- list(
      coordinates = data.frame(env = env, lat = lat, lon = lon, start = start_vec, end = end_vec),
      params = params,
      scale = scale
    )
    cache_result <- cache_service$getOrFetch(request_params)
    if (!isTRUE(cache_result$needs_fetch)) return(cache_result$data)
    cache_key <- cache_result$cache_key
  }

  tasks <- data.frame(
    env   = env,
    lat   = lat,
    lon   = lon,
    start = start_vec,
    end   = end_vec,
    stringsAsFactors = FALSE
  )

  result_list <- NULL

  if (isTRUE(parallel)) {
    rlang::check_installed("mirai")
    ncores <- min(nrow(tasks), if(is.null(workers)) ceiling(parallel::detectCores()*0.5) else workers)
    tasks$chunk <- rep(seq_len(ncores), length.out = nrow(tasks))
    chunks_list <- split(tasks, tasks$chunk)

    cli::cli_alert_info("Starting parallel processing over {.val {ncores}} core(s)...")
    mirai::daemons(n = ncores)
    on.exit(mirai::daemons(n = 0), add = TRUE)

    mirai_tasks <- lapply(seq_along(chunks_list), function(i) {
      mirai::mirai(
        {
          lapply(seq_len(nrow(chunk_data)), function(j) {
            fn_fetch(
              lat_i = chunk_data$lat[j],
              lon_i = chunk_data$lon[j],
              env_i = chunk_data$env[j],
              start_i = chunk_data$start[j],
              end_i = chunk_data$end[j],
              nasaparams = n_params,
              params = p_list,
              scale = s_scale
            )
          }) |> dplyr::bind_rows()
        },
        chunk_data = chunks_list[[i]],
        fn_fetch   = fetch_data_point,
        n_params   = nasaparams,
        p_list     = params,
        s_scale    = scale
      )
    })

    result_list <- vector("list", length(mirai_tasks))
    results_ready <- rep(FALSE, length(mirai_tasks))

    while (!all(results_ready)) {
      for (i in seq_along(mirai_tasks)) {
        if (!results_ready[i]) {
          if (!mirai::unresolved(mirai_tasks[[i]])) {
            res <- mirai_tasks[[i]][]
            if (inherits(res, "miraiError") || inherits(res, "errorValue")) {
              cli::cli_abort("Worker error encountered: {as.character(res)}")
            }
            result_list[[i]] <- res
            results_ready[i] <- TRUE
          }
        }
      }
      if (!all(results_ready)) Sys.sleep(0.05)
    }

  } else {
    if (isTRUE(progress)) {
      pb_id <- cli::cli_progress_bar(
        name = "Fetching NASA POWER data",
        total = nrow(tasks)
      )
      result_list <- lapply(seq_len(nrow(tasks)), function(i) {
        cli::cli_progress_update(
          id = pb_id,
          status = sprintf("Fetching %s", tasks$env[i])
        )
        fetch_data_point(tasks$lat[i], tasks$lon[i], tasks$env[i], tasks$start[i], tasks$end[i], nasaparams, params, scale)
      })
      cli::cli_progress_done(id = pb_id)
    } else {
      cli::cli_alert_info("Fetching climate variables sequentially...")
      result_list <- lapply(seq_len(nrow(tasks)), function(i) {
        fetch_data_point(tasks$lat[i], tasks$lon[i], tasks$env[i], tasks$start[i], tasks$end[i], nasaparams, params, scale)
      })
    }
  }

  result_list <- result_list[!vapply(result_list, is.null, logical(1))]
  if (!length(result_list)) {
    cli::cli_warn("Zero dataset observations compiled. Verify input coordinates or internet connectivity.")
    return(NULL)
  }

  final_df <- tryCatch(dplyr::bind_rows(result_list), error = function(e) NULL)
  if (is.null(final_df) || !nrow(final_df)) return(NULL)

  if (!is.null(cache_service)) cache_service$save(cache_key, final_df)

  if (identical(scale, "daily")) {
    final_df <-
      final_df |>
      dplyr::relocate(ENV, LAT, LON, DATE, .before = 1) |>
      dplyr::select(-dplyr::any_of(c("YEAR", "MO", "DY"))) |>
      tidyr::separate_wider_delim(DATE, names = c("YEAR", "MO", "DY"), delim = "-") |>
      dplyr::group_by(ENV) |>
      dplyr::mutate(DFS = dplyr::row_number(), .after = DOY,
                    RA = Ra_fun(DOY, LAT)$Ra,
                    N  = Ra_fun(DOY, LAT)$N) |>
      dplyr::ungroup() |>
      tidyr::unite("DATE", YEAR, MO, DY, sep = "-", remove = FALSE)

    temp_col <- if ("T2M" %in% names(final_df)) "T2M" else if ("T2M_MAX" %in% names(final_df)) "T2M_MAX" else NULL
    rh_col   <- if ("RH2M" %in% names(final_df)) "RH2M" else NULL
    if (!is.null(temp_col) && !is.null(rh_col)) {
      vpd_results <- vpd(final_df[[temp_col]], final_df[[rh_col]])
      final_df$ES <- vpd_results$ES; final_df$EA <- vpd_results$EA; final_df$VPD <- vpd_results$VPD
    }
    if (!is.null(temp_col)) final_df$SLOPE_SVP <- slope_svp(final_df[[temp_col]])
  } else if (identical(scale, "hourly")) {
    final_df <- final_df |> dplyr::relocate(ENV, LAT, LON, DATE, .before = 1)
    temp_col <- if ("T2M" %in% names(final_df)) "T2M" else if ("T2M_MAX" %in% names(final_df)) "T2M_MAX" else NULL
    rh_col   <- if ("RH2M" %in% names(final_df)) "RH2M" else NULL
    if (!is.null(temp_col) && !is.null(rh_col)) {
      vpd_results <- vpd(final_df[[temp_col]], final_df[[rh_col]])
      final_df$ES <- vpd_results$ES
      final_df$EA <- vpd_results$EA
      final_df$VPD <- vpd_results$VPD
    }
    if (!is.null(temp_col)) final_df$SLOPE_SVP <- slope_svp(final_df[[temp_col]])
  } else if (scale %in% c("monthly", "climatology")) {
    final_df <- final_df |> dplyr::relocate(ENV, LAT, LON, .before = 1)
  }
  final_df
}

#' Generate Macro-Envirotype Profiles across Phenological Crop Stages
#'
#' @description
#' `envirotype()` converts continuous historical weather matrices into discrete,
#' macro-environmental profile frequencies intersected with phenological growth segments.
#'
#' By establishing structural baseline rules per variable, the function quantifies the
#' relative occurrence (`fr`) of specific meteorological cohorts across growth
#' periods. Threshold grouping intervals can either be defined explicitly via manual
#' intervals (`breaks`) or dynamically segmented using quantiles (`probs`).
#'
#' @param data A data frame containing multi-environment parsed historical weather.
#'   Must feature unique location vectors (\code{ENV}) and consecutive days from start
#'   retrieval variables (\code{DFS}).
#' @param dates Numeric vector defining lower boundaries (inclusive day limits)
#'   matching development shifts.
#' @param stages Character vector containing explicit nomenclature prefixes for crop growth
#'   periods. Default is \code{c("S01 Establishing", "S02 Vegetative", "S03 Flowering", "S04 Reproductive")}.
#' @param vars Character vector containing target column strings to categorize.
#'   Default is \code{"T2M_MAX"}.
#' @param var Single character to choose the variable to plot.
#' @param breaks Optional numeric vector indicating hard-coded static classification markers.
#'   If passed, it unifies boundaries across \code{vars} and appends infinite tails (\code{-Inf}, \code{Inf}).
#' @param probs Numeric vector specifying quantile thresholds evaluated over the general data
#'   population when \code{breaks = NULL}. Default is standard quartiles: \code{c(0, 0.25, 0.50, 0.75, 1.0)}.
#' @param labels Optional custom string character array used to rename destination classification levels.
#' @param format Character selection declaring outcome table layout shapes. Use \code{"long"} to preserve
#'   tidy observation matrices (ideal for plotting) or \code{"wide"} to spread combinations horizontally into columns
#'   (ideal for downstream genomic/multi-environment predictive models).
#' @param x An object of S3 class \code{envirotype}.
#' @param ncol The number of columns to plot.
#' @param ... Unused trailing method arguments passed down to nested S3 dependencies.
#'
#' @return \code{envirotype()}: An object of class \code{envirotype} and \code{data.frame}.
#'   If \code{format = "long"}, it preserves standard columns (\code{ENV}, \code{stage}, \code{window},
#'   \code{var}, \code{xcut}, \code{Freq}, \code{fr}). If \code{format = "wide"}, rows match distinct
#'   environments, and columns represent combined factors.
#' @return \code{plot()}: A faceted \code{ggplot} object tracking structural relative frequency columns
#'   arranged vertically by stage blocks.
#'
#' @export envirotype
#' @export plot.envirotype
#' @rdname envirotype
#'
#' @examples
#' \dontrun{
#' library(metan)
#' clim <- get_climate(
#'   env   = c("Anapolis", "Jatai", "Sorriso"),
#'   lat   = c(-16.3, -17.9, -12.5),
#'   lon   = c(-49,   -51.7, -55.7),
#'   start = c("2025-10-01", "2025-10-12", "2025-10-08"),
#'   end   = c("2026-01-19", "2026-02-09", "2026-01-26"),
#'   scale = "daily"
#' )
#'
#' # Mode 1: Group variables into automatic global dataset quantiles (Long Format)
#' df_enviro_long <- envirotype(
#'   data   = clim,
#'   dates  = c(1, 15, 45, 80),
#'   vars   = c("T2M_MAX", "VPD"),
#'   format = "long"
#' )
#' plot(df_enviro_long)
#'
#' # Mode 2: Apply manual threshold rules across multi-variables expanded horizontally (Wide Format)
#' df_enviro_wide <- envirotype(
#'   data   = clim,
#'   dates  = c(1, 15, 45, 80),
#'   vars   = c("T2M_MAX"),
#'   breaks = c(15, 22, 30, 35),
#'   format = "wide"
#' )
#' }
envirotype <- function(data,
                       dates,
                       stages = c("S01 Establishing", "S02 Vegetative", "S03 Flowering", "S04 Reproductive"),
                       vars = c("T2M"),
                       breaks = NULL,
                       probs = c(0, 0.25, 0.50, 0.75, 1.0),
                       labels = NULL,
                       format = c("long", "wide")){
  format <- rlang::arg_match(format)

  # --- Helper 1: Creation of stages and DFS windows ---
  create_stage_labels <- function(df, dates, stages) {
    if (length(stages) != length(dates)) {
      cli::cli_abort("The length of {.field stages} ({length(stages)}) must be equal to the length of {.field dates} ({length(dates)}).")
    }
    lim_inf <- dates
    lim_sup <- c(dates[-1] - 1, Inf)

    window_labels <- purrr::map2_chr(lim_inf, lim_sup, function(from, to) {
      if (is.infinite(to)) glue::glue("{from}+") else glue::glue("{from}-{to}")
    })

    stage_exprs <- purrr::map2(lim_inf, lim_sup, function(from, to) {
      rlang::expr(dplyr::between(DFS, !!from, !!to) ~ !!stages[which(lim_inf == from)])
    })

    window_exprs <- purrr::map2(lim_inf, lim_sup, function(from, to) {
      idx <- which(lim_inf == from)
      rlang::expr(dplyr::between(DFS, !!from, !!to) ~ !!window_labels[idx])
    })

    stage_case_when  <- rlang::expr(dplyr::case_when(!!!stage_exprs))
    window_case_when <- rlang::expr(dplyr::case_when(!!!window_exprs))

    df |>
      dplyr::mutate(
        stage = !!stage_case_when,
        window = !!window_case_when
      )
  }

  # Assign initial stages and windows
  data_labeled <- create_stage_labels(data, dates = dates, stages = stages)

  # Convert data to long format to unify processing of variables
  data_long <- data_labeled |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "var",
      values_to = "value"
    )

  # --- CRITICAL STEP MODIFIED: Definition of Breaks (Manual vs Quantiles) ---
  global_breaks_list <- data_long |>
    dplyr::group_by(var) |>
    dplyr::group_split() |>
    purrr::map(function(sub) {
      x <- stats::na.omit(sub$value)

      # If the user provided a numeric vector in 'breaks', use it.
      # Otherwise, calculate global population quantiles.
      if (!is.null(breaks) && is.numeric(breaks)) {
        brks <- unique(c(-Inf, breaks, Inf))
      } else {
        brks <- unique(stats::quantile(x, probs = probs, na.rm = TRUE))
        # Safeguard for zero variance data
        if (length(brks) < 2) {
          brks <- c(brks - 0.001, brks + 0.001)
        }
      }

      # Generate custom automated labels if not provided by the user
      if (is.null(labels) || length(labels) != (length(brks) - 1)) {
        lbls <- purrr::map_chr(seq_along(brks[-1]), function(i) {
          left  <- round(brks[i], 2)
          right <- round(brks[i + 1], 2)

          if (is.infinite(left)) return(glue::glue("< {right}"))
          if (is.infinite(right)) return(glue::glue("\u2265 {left}"))
          return(glue::glue("{left}-{right}"))
        })
      } else {
        lbls <- labels
      }

      list(var = unique(sub$var), breaks = brks, labels = lbls)
    }) |>
    purrr::set_names(purrr::map_chr(vars, ~ .x))

  # --- Helper 2: Frequency using rules saved in the global list ---
  create_class <- function(sub_data, current_var) {
    x <- stats::na.omit(sub_data$value)
    if (length(x) == 0) return(data.frame())

    # Retrieve the structured rules (breaks and labels) for this specific variable
    v_rules <- global_breaks_list[[current_var]]

    xcut <- cut(x, breaks = v_rules$breaks, labels = v_rules$labels, include.lowest = TRUE, right = FALSE)

    data.frame(xcut = xcut) |>
      dplyr::group_by(xcut) |>
      dplyr::summarise(Freq = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(fr = Freq / sum(Freq))
  }

  # --- Pipeline de Agrupamento Final ---
  res <-
    data_long |>
    dplyr::group_by(ENV, stage, window, var) |>
    dplyr::group_modify(~ create_class(.x, current_var = .y$var), .ungroup = TRUE) |>
    dplyr::relocate(ENV, stage, window, var) |>
    as.data.frame()

  if(format == "wide"){
    res <-
      res |>
      dplyr::select(ENV, stage, var, xcut, fr) |>
      pivot_wider(names_from = c(stage, var, xcut),
                  values_from = fr)
  }

  class(res) <- c("envirotype", class(res))
  return(res)
}

#' @rdname envirotype
#' @method plot envirotype
plot.envirotype <- function(x, var = NULL, ncol = 1, ...) {
  if(is.null(var)){
    var <- x$var[[1]]
  }
  vartoplot <- var
  x <-
    x |>
    dplyr::filter(var == vartoplot) |>
    dplyr::mutate(facet = paste0(stage, " [", window, "]"))
  ggplot2::ggplot(x, ggplot2::aes(x = fr, y = ENV, fill = xcut)) +
    ggplot2::geom_col(position = "stack", width = 0.9, color = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(~facet, ncol = ncol, scales = "free_y") +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_x_continuous(expand = expansion(mult = 0)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = 'Relative Frequency', y = "Environment", fill = 'Envirotype') +
    ggplot2::theme(legend.position = "bottom",
                   strip.text = ggplot2::element_text(face = "bold"),
                   strip.background = ggplot2::element_rect(fill = "gray90", color = NA),
                   panel.spacing.y = ggplot2::unit(1, "lines"))
}
