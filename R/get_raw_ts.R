#' Get time-series forecast of raw NWP ensemble
#'
#' This method does a basic empirical CDF based on the raw NWP ensemble.
#' @family ts_training_forecast
#' @param first_valid_time A time stamp
#' @param ens_test [time x member] matrix of ensemble data for test period
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_raw_ts <- function(first_valid_time, ens_test, site, AC_rating, metadata){

  # No Training

  # Forecast
  ts <- forecasting::ts_forecast(ens_test, first_valid_time,
                    time_step=metadata$resolution, scale='site',
                    location=site,
                    method = 'empirical',
                    max_power=AC_rating)
  return(ts)
}

