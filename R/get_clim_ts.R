#' Get time-series forecast using climatology
#'
#' This method generates a static forecast for all times when the sun is up.
#' The training telemetry could either be the test sample or a historical
#' period.
#' @family ts_training_forecast
#' @param issue A time stamp
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A vector of booleans, indexed by telemetry valid times
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_clim_ts <- function(issue, telemetry, sun_up, site, AC_rating, metadata){

  warning("Climatology treats each issue time as a separate training set.")

  # Train
  valid_idx <- sun_up & !is.na(telemetry$data)
  data.input <- t(sapply(sun_up,
                         FUN=function(m) if (m) telemetry$data[valid_idx]
                         else rep(0, times=sum(valid_idx)), simplify="array"))

  # Forecast
  ts <- forecasting::ts_forecast(data.input, issue + lubridate::hours(ifelse(metadata$is_rolling, 0, metadata$lead_time)),
                    time_step=metadata$resolution, scale='site',
                    location=site,
                    method = 'empirical',
                    max_power=AC_rating)
  return(ts)
}
