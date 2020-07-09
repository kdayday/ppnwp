#' Get time-series forecast using EMOS post-processing of NWP ensembles
#'
#' This method trains EMOS models for each time-point using a sliding or
#' time-of-day training method, then forecasts using the current
#' NWP ensemble.
#' @family ts_training_forecast
#' @param issue A time stamp
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   telemetry time indices
#' @param ens_test [time x member] matrix of ensemble data for test period
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_emos_ts <- function(issue, t_idx_series, ens_test, ensemble, telemetry, sun_up,
                        site, AC_rating, metadata){

  # Train
  models <- train_emos(t_idx_series, issue, ensemble, telemetry, sun_up, site,
                        AC_rating, metadata)
  # Forecast
  ts <- forecasting::ts_forecast(ens_test, issue + lubridate::hours(ifelse(metadata$is_rolling, 0, metadata$lead_time)),
                                 time_step=metadata$resolution, scale='site',
                                 location=site, method = 'emos',
                                 MoreTSArgs = list(model=models), max_power=AC_rating)
  return(ts)
}

#' Subfunction to get EMOS models using sliding or time-of-day training
#'
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   telemetry time indices
#' @param issue Time stamp of issue time
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
train_emos <- function(t_idx_series, issue, ensemble, telemetry, sun_up, site,
                       AC_rating, metadata) {
  tictoc::tic("Total EMOS model fit time: ")

  if (metadata$is_rolling & metadata$forecast_type == "emos_sliding") {
    model <- train_emos_subfunc(min(which(sun_up[t_idx_series])), t_idx_series, issue,
                                ensemble, telemetry, sun_up,
                               site, AC_rating, metadata)

    models <- lapply(t_idx_series, FUN=function(t) {if (!sun_up[t]) {return(NA)} else {model}})
  } else {
    models <- vector(mode="list", length=length(t_idx_series)) # initalize empty list

    # Cycle through time_points in the benchmark. Use for loop rather than
    # apply in order to feed last model into next one.
    for (i in seq_along(t_idx_series)) {
      # For first time point, use default initial parameter values for optim.
      # Else, re-use the last time point. Is last point is NA, defaults will be reused
      if (i == 1) { par_init <- NA} else {par_init <- models[[i-1]]}

      model <- train_emos_subfunc(i, t_idx_series, ensemble, telemetry, sun_up,
                                  site, AC_rating, metadata)

      models[[i]] <- model
    }
  }

  tictoc::toc()
  return(models)
}

#' Subfunction to get EMOS model for individual time-point using sliding or
#' time-of-day training
#'
#' @param step Step (index) in this forecast run
#' @param t_idx_series Series of time indices in forecast run, relative to the
#'   telemetry time indices
#' @param issue Time stamp of issue time
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
train_emos_subfunc <- function(step, t_idx_series, issue, ensemble, telemetry, sun_up,
                               site, AC_rating, metadata) {

  time_idx_forecast <- t_idx_series[step]

  model <- tryCatch({
    # Skip training if sun is down
    if (!sun_up[time_idx_forecast]) {
      model <- NA
    } else {

      training_data <- get_training_subsets(time_idx_forecast, issue, step, metadata, ensemble, telemetry)
      # Subset. No normalization as in BMA training.
      ens_subset <- training_data$ens_subset
      tel_subset <- training_data$tel_subset

      # Do not train if data is missing. There must be at least 2 data points for regression and observations can't be 0 only.
      if (sum(apply(X=ens_subset, MARGIN = 1, FUN = function(v) {any(v>0 & !is.na(v))}) & (!is.na(tel_subset) & tel_subset > 0)) < 2) {
        model <- NA
      } else {
        model <- emos_model(tel_subset, ens_subset, max_power=AC_rating, par_init=par_init)
      }
    }
  }, error = function(e) {
    e$message <- paste(e$message, "in training time index", time_idx_forecast, "for site", site)
    # browser()
    stop(e)
  })
}
