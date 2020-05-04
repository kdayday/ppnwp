#' Get time-series forecast using BMA post-processing of NWP ensembles
#'
#' This method trains BMA models for each time-point using a sliding,
#' time-of-day, or constant training method, then forecasts using the current
#' NWP ensemble.
#' @family ts_training_forecast
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
#' @param lm_formula Formula for BMA linear regression
#' @return A ts_forecast object
#' @export
get_bma_ts <- function(t_idx_series, ens_test, ensemble, telemetry, sun_up,
                       site, AC_rating, metadata, lm_formula){
  # Train
  if (metadata$forecast_type %in% c("sliding", "time-of-day")) {
    models <- train_bma(t_idx_series, ensemble, telemetry, sun_up, site,
                        AC_rating, metadata, lm_formula)
  } else if (metadata$forecast_type=="constant_bma") {
    models <- train_constant_bma(t_idx_series, ensemble, telemetry, sun_up,
                                 site, AC_rating, metadata, lm_formula)
  } else stop("Unrecognized BMA training method.")

  # Forecast
  ts <- forecasting::ts_forecast(ens_test, metadata$date_benchmark_start,
                    time_step=metadata$resolution, scale='site',
                    location=site,
                    method = 'bma', MoreTSArgs = list(model=models),
                    max_power=AC_rating,
                    bma_distribution=metadata$bma_distribution)
  return(ts)
}


#' Subfunction to get BMA models using sliding or time-of-day training
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
#' @param lm_formula Formula for BMA linear regression
train_bma <- function(t_idx_series, ensemble, telemetry, sun_up, site, AC_rating,
                      metadata, lm_formula) {
  tictoc::tic("Total BMA model fit time: ")

  # Cycle through time_points in the benchmark
  bma_models <- lapply(t_idx_series, FUN=train_bma_subfunc, ensemble=ensemble, telemetry=telemetry,
                       sun_up=sun_up, site=site, AC_rating=AC_rating,
                       metadata=metadata, lm_formula=lm_formula)
  tictoc::toc()
  return(bma_models)
}

#' Subfunction to get BMA model for individual time-point using sliding or
#' time-of-day training
#'
#' @param time_idx_forecast Time index
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @param lm_formula Formula for BMA linear regression
train_bma_subfunc <- function(time_idx_forecast, ensemble, telemetry, sun_up,
                              site, AC_rating, metadata, lm_formula) {
  model <- tryCatch({
    # Skip training if sun is down
    if (!sun_up[time_idx_forecast]) {
      model <- NA
    } else {
      if (metadata$forecast_type == "sliding") {
        time_idx_train <- sort(time_idx_forecast - seq_len(metadata$training_window))
      } else {  # metadata$forecast_type == "time-of-day"
        time_idx_train <- sort(time_idx_forecast + c(-365*ts_per_day + seq(-ts_per_day*metadata$training_window, length.out = 2*metadata$training_window+1, by=+ts_per_day),
                                                     seq(-ts_per_day, length.out = metadata$training_window, by=-ts_per_day)))
      }
      # Subset right into normalize
      ens_subset <- t(ens_data[site_idx, , time_idx_train]/AC_rating)
      tel_subset <- telemetry$data[time_idx_train]/AC_rating
      # Do not train if data is missing. There must be at least 2 data points for regression and observations can't be 0 only.
      if (sum(apply(X=ens_subset, MARGIN = 1, FUN = function(v) {any(v>0 & !is.na(v))}) & (!is.na(tel_subset) & tel_subset > 0)) < 2) {return(NA)}
      # MoreTSArgs has to come before the other optional inputs
      model <- bma_ens_models(tel_subset, ens_subset, bma_distribution=metadata$bma_distribution,
                              max_power=AC_rating, lm_formula=lm_formula,
                              maxiter=200, eps=1e-4, percent_clipping_threshold=metadata$percent_clipping_threshold)
    }

    return(model)
  }, error = function(e) {
    e$message <- paste(e$message, "in training time index", time_idx_forecast, "for site", site)
    # browser()
    stop(e)
  })
}

#' Subfunction to get single, constant BMA model for all time-points
#'
#' @param time_idx_forecast Time index
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @param lm_formula Formula for BMA linear regression
train_constant_bma <- function(t_idx_series, ensemble, telemetry, sun_up,
                                     site, AC_rating, metadata,
                                     lm_formula) {
  tictoc::tic("Total BMA model fit time: ")

  ens_subset <- t(ens_data[site_idx, , 1:(t_start_idx-1)]/AC_rating)
  time_idx_train <- 1:(t_idx_series[1]-1)

  tel_subset <- telemetry$data[time_idx_train]/AC_rating
  tictoc::tic("Single model model fit time: ")
  model <- bma_ens_models(tel_subset, ens_subset, bma_distribution=metadata$bma_distribution,
                          max_power=AC_rating, lm_formula=lm_formula,
                          maxiter=200, eps=1e-4, percent_clipping_threshold=metadata$percent_clipping_threshold)
  tictoc::toc()
  # Cycle through time_points in the benchmark
  bma_models <- lapply(t_idx_series, FUN=function(t) {if (!sun_up[t]) {return(NA)} else {model}})
  tictoc::toc()
  return(bma_models)
}
