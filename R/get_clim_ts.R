#' Get time-series forecast using climatology
#'
#' This method generates a static forecast for all times when the sun is up,
#' *based on the climatology of the test sample*, rather than a historical
#' period.
#' @family ts_training_forecast
#' @param tel_test A vector of the test period telemetry for this site
#' @param sun_up A [site x time] matrix of booleans
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_clim_ts <- function(tel_test, sun_up, sites, site_idx,
                        site_max_p, metadata){

  # Train
  valid_idx <- sun_up[site_idx, ] & !is.na(tel_test)
  data.input <- t(sapply(sun_up[site_idx, ],
                         FUN=function(m) if (m) tel_test[valid_idx]
                         else rep(0, times=sum(valid_idx)), simplify="array"))

  # Forecast
  ts <- forecasting::ts_forecast(data.input, metadata$date_benchmark_start,
                    time_step=metadata$resolution, scale='site',
                    location=paste("Site", site, sep=" "),
                    method = 'empirical',
                    max_power=AC_rating)
  return(ts)
}
