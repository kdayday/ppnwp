#' Get time-series forecast using persistence ensemble (PeEn) method
#'
#' This method enforces a set of training data of length training_window; if
#' there is missing data, it continues back through the historical record until
#' it has a full set (unless it runs out of data)
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
get_peen_ts <- function(issue, t_idx_series, telemetry, sun_up, site,
                        max_power, metadata){

  warning("PeEn is currently ignores issue time and assumes most recent measurements are available.")

  # Train
  data.input <- train_peen_data(t_idx_series, telemetry, sun_up,
                                metadata)

  # Forecast
  ts <- forecasting::ts_forecast(data.input, issue + lubridate::hours(ifelse(metadata$is_rolling, 0, metadata$lead_time)),
                    time_step=metadata$resolution, scale='site',
                    location=site,
                    method = 'empirical',
                    max_power=max_power)
  return(ts)
}

#' Get matrix of evolving PeEn data
#'
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   telemetry time indices
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param metadata A data.frame of forecast parameters
train_peen_data <- function(t_idx_series, telemetry, sun_up, metadata) {
  # Cycle through time_points in the benchmark
  data_matrix <- t(sapply(t_idx_series, FUN=train_peen_subfunc,
                          telemetry=telemetry, sun_up=sun_up,
                          metadata=metadata, simplify="array"))
  return(data_matrix)
}

train_peen_subfunc <- function(time_idx_forecast, telemetry, sun_up, metadata) {
  if (!sun_up[time_idx_forecast]) {
    return(rep(0, times=metadata$training_window))
  } else {
    indices <- c()
    potential_idx <- time_idx_forecast - metadata$ts_per_day
    while (length(indices) < metadata$training_window & potential_idx > 0) {
      if (!is.na(telemetry$data[potential_idx])) indices <- c(indices, potential_idx)
      potential_idx <- potential_idx - metadata$ts_per_day
    }
    peen_data <- telemetry$data[indices]
    if (length(peen_data) < metadata$training_window)
      peen_data <- c(peen_data, rep(NA, times=metadata$training_window-length(peen_data)))
    return(peen_data)
  }
}
