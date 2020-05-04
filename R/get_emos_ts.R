#' Get time-series forecast using EMOS post-processing of NWP ensembles
#'
#' This method trains EMOS models for each time-point using a sliding or
#' time-of-day training method, then forecasts using the current
#' NWP ensemble.
#' @family ts_training_forecast
#' @param first_valid_time A time stamp
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
get_emos_ts <- function(first_valid_time, t_idx_series, ens_test, ensemble, telemetry, sun_up,
                        site, AC_rating, metadata){

  # Train
  models <- train_emos(t_idx_series, ensemble, telemetry, sun_up, site,
                        AC_rating, metadata)
  # Forecast
  ts <- forecasting::ts_forecast(ens_test, first_valid_time,
                                 time_step=metadata$resolution, scale='site',
                                 location=site, method = 'emos',
                                 MoreTSArgs = list(model=models), max_power=AC_rating)
  return(ts)
}

#' Subfunction to get EMOS models using sliding or time-of-day training
#'
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   telemetry time indices
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
train_emos <- function(t_idx_series, ensemble, telemetry, sun_up, site,
                       AC_rating, metadata) {
  tictoc::tic("Total EMOS model fit time: ")

  models <- vector(mode="list", length=length(t_idx_series)) # initalize empty list

  # Cycle through time_points in the benchmark. Use for loop rather than
  # apply in order to feed last model into next one.
  for (i in seq_along(t_idx_series)) {
    # For first time point, use default initial parameter values for optim.
    # Else, re-use the last time point. Is last point is NA, defaults will be reused
    if (i == 1) { par_init <- NA} else {par_init <- models[[i-1]]}

    model <- tryCatch({
      # Skip training if sun is down
      if (!sun_up[t_idx_series[i]]) {
        model <- NA
      } else {
        if (metadata$forecast_type == "sliding_emos") {
          time_idx_train <- sort(t_idx_series[i] - seq_len(metadata$training_window))
        } else {  # metadata$forecast_type == "time-of-day"
          time_idx_train <- sort(t_idx_series[i] + c(-365*metadata$ts_per_day + seq(-metadata$ts_per_day*metadata$training_window,
                                                                                    length.out = 2*metadata$training_window+1, by=+metadata$ts_per_day),
                                                     seq(-metadata$ts_per_day, length.out = metadata$training_window, by=-metadata$ts_per_day)))
        }
        time_idx_train <- time_idx_train[sun_up[time_idx_train]]
        # Subset. No normalization as in BMA training.
        ens_subset <- get_training_ensemble_from_validtimes(time_idx_train, ensemble, metadata)
        tel_subset <- telemetry$data[time_idx_train]
        # Do not train if data is missing. There must be at least 2 data points for regression and observations can't be 0 only.
        if (sum(apply(X=ens_subset, MARGIN = 1, FUN = function(v) {any(v>0 & !is.na(v))}) & (!is.na(tel_subset) & tel_subset > 0)) < 2) {
          model <- NA
        } else {
          model <- emos_model(tel_subset, ens_subset, max_power=AC_rating, par_init=par_init)
        }
      }
    }, error = function(e) {
      e$message <- paste(e$message, "in training time index", t_idx_series[i], "for site", site)
      # browser()
      stop(e)
    })
    models[[i]] <- model
  }

  tictoc::toc()
  return(models)
}
