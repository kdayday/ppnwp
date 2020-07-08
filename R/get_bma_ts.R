#' Get time-series forecast using BMA post-processing of NWP ensembles
#'
#' This method trains BMA models for each time-point using a sliding,
#' time-of-day, or constant training method, then forecasts using the current
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
#' @param lm_formula Formula for BMA linear regression
#' @return A ts_forecast object
#' @export
get_bma_ts <- function(first_valid_time, t_idx_series, ens_test, ensemble, telemetry, sun_up,
                       site, AC_rating, metadata, lm_formula){
  # Train
  if (metadata$forecast_type %in% c("bma_sliding", "bma_time-of-day")) {
    models <- train_bma(t_idx_series, ensemble, telemetry, sun_up, site,
                        AC_rating, metadata, lm_formula)
  } else if (metadata$forecast_type=="bma_constant") {
    models <- train_constant_bma(t_idx_series, ensemble, telemetry, sun_up,
                                 site, AC_rating, metadata, lm_formula)
  } else stop("Unrecognized BMA training method.")

  # Forecast
  ts <- forecasting::ts_forecast(ens_test, first_valid_time,
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
  bma_models <- lapply(seq_along(t_idx_series), FUN=train_bma_subfunc, t_idx_series=t_idx_series,
                       ensemble=ensemble, telemetry=telemetry,
                       sun_up=sun_up, site=site, AC_rating=AC_rating,
                       metadata=metadata, lm_formula=lm_formula)
  tictoc::toc()
  return(bma_models)
}

#' Subfunction to get BMA model for individual time-point using sliding or
#' time-of-day training
#'
#' @param step Step (index) in this forecast run
#' @param t_idx_series Series of time indices in forecast run, relative to the
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
train_bma_subfunc <- function(step, t_idx_series, ensemble, telemetry, sun_up,
                              site, AC_rating, metadata, lm_formula) {
  time_idx_forecast <- t_idx_series[step]

  model <- tryCatch({
    # Skip training if sun is down
    if (!sun_up[time_idx_forecast]) {
      model <- NA
    } else {

      training_data <- get_training_subsets(time_idx_forecast, step, metadata, ensemble, telemetry)
      # Normalize subsets
      ens_subset <- training_data$ens_subset/AC_rating
      tel_subset <- training_data$tel_subset/AC_rating

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
train_constant_bma <- function(t_idx_series, ensemble, telemetry, sun_up,
                                     site, AC_rating, metadata,
                                     lm_formula) {
  tictoc::tic("Total BMA model fit time: ")

  time_idx_train <- 1:(t_idx_series[1]-1)

  if (metadata$is_rolling) {
    # ensemble sizing is [1 x all steps x member]
    ens_subset <- ensemble$data[1, time_idx_train, ]/AC_rating
  } else {
    ens_subset <- t(sapply(time_idx_train, FUN=function(valid) {
        ind <- valid_2_issue_index(telemetry$validtime[valid], metadata, ensemble)
        ensemble$data[ind[1], ind[2],]}))/AC_rating
  }

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
