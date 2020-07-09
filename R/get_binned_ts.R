#' Get time-series forecast of binned probability forecast
#'
#' This method does a basic linear interpolation of the raw NWP ensemble.
#' @family ts_training_forecast
#' @param issue A time stamp
#' @param ens_test [time x member] matrix of ensemble data
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_binned_ts <- function(issue, ens_test, site, AC_rating, metadata){

  # No Training

  # Forecast
  ts <- forecasting::ts_forecast(ens_test, issue + lubridate::hours(ifelse(metadata$is_rolling, 0, metadata$lead_time)),
                    time_step=metadata$resolution, scale='site',
                    location=site,
                    method = 'binned',
                    max_power=AC_rating)
  return(ts)
}

