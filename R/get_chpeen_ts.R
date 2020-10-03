#' Get time-series forecast using CH-PeEn method
#'
#' This method generates a static 24-hour or similar forecast using all data at
#' the same time of day in the training period only. This is a static forecast
#' that does not evolve as more observations are realized in the evaluation
#' period.
#' @family ts_training_forecast
#' @param issue A time stamp
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   telemetry time indices
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param max_power Site's AC power rating or maximum load
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_chpeen_ts <- function(issue, t_idx_series, telemetry, sun_up, site,
                          max_power, metadata){

  warning("CH-PeEn is currently ignores issue time and assumes most recent measurements are available.")

  # Train
  data.input <- train_ch_peen(t_idx_series, telemetry, sun_up, metadata)

  # Forecast
  ts <- forecasting::ts_forecast(data.input, issue + lubridate::hours(ifelse(metadata$is_rolling, 0, metadata$lead_time)),
                    time_step=metadata$resolution, scale='site',
                    location=site,
                    method = 'empirical',
                    max_power=max_power,
                    quantiles=seq(0.01, 0.99, by=0.01))
  return(ts)
}

#' Subfunction to get input data for CH-PeEn forecast
#'
#' Get matrix of static, complete-history PeEn data
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   telemetry time indices
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param metadata A data.frame of forecast parameters
train_ch_peen <- function(t_idx_series, telemetry, sun_up, metadata) {

  daily_matrix <- t(sapply(seq_len(metadata$ts_per_day),
                           FUN=function(i) return(telemetry$data[seq(i, t_idx_series[1]-metadata$ts_per_day+i,
                                                                             by=metadata$ts_per_day)]),
                           simplify="array"))
  # For comparing across forecast methods, restrict data to the same times when the ensemble forecasts the sun is up
  data_matrix <- t(sapply(t_idx_series,
                          FUN = function(i) {if (sun_up[i]) {daily_matrix[(i-1)%%metadata$ts_per_day+1,]}
                            else {return(rep(0, times=dim(daily_matrix)[2]))}},
                          simplify="array"))
  return(data_matrix)
}
