#' Get time-series forecast using persistence ensemble (PeEn) method
#'
#' This method enforces a set of training data of length training_window; if
#' there is missing data, it continues back through the historical record until
#' it has a full set (unless it runs out of data)
#' @family ts_training_forecast
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   all_site_tel time indices
#' @param all_site_tel A [site x time] matrix of telemetry
#' @param sun_up A [site x time] matrix of booleans
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @return A ts_forecast object
#' @export
get_peen_ts <- function(t_idx_series, all_site_tel, sun_up, sites,
                        site_idx, site_max_p, metadata){

  # Train
  data.input <- train_peen_data(t_idx_series, all_site_tel, sun_up,
                                site_idx, metadata)

  # Forecast
  ts <- forecasting::ts_forecast(data.input, metadata$date_benchmark_start,
                    time_step=metadata$resolution, scale='site',
                    location=paste("Site", site, sep=" "),
                    method = 'empirical',
                    max_power=AC_rating)
  return(ts)
}

#' Get matrix of evolving PeEn data
#'
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   all_site_tel time indices
#' @param all_site_tel A [site x time] matrix of telemetry
#' @param sun_up A [site x time] matrix of booleans
#' @param site_idx Index of which site is currently being forecasted, relative
#'   to the order in all_site_tel
#' @param metadata A data.frame of forecast parameters
train_peen_data <- function(t_idx_series, all_site_tel, sun_up, site_idx, metadata) {
  # Cycle through time_points in the benchmark
  data_matrix <- t(sapply(t_idx_series, FUN=sub_func, simplify="array"))
  return(data_matrix)
}

train_peen_subfunc <- function(time_idx_forecast, all_site_tel, sun_up, site_idx, metadata) {
  if (!sun_up[site_idx, time_idx_forecast]) {
    return(rep(0, times=metadata$training_window))
  } else {
    indices <- c()
    potential_idx <- time_idx_forecast - metadata$ts_per_day
    while (length(indices) < metadata$training_window & potential_idx > 0) {
      if (!is.na(all_site_tel[site_idx, potential_idx])) indices <- c(indices, potential_idx)
      potential_idx <- potential_idx - metadata$ts_per_day
    }
    peen_data <- all_site_tel[site_idx, indices]
    if (length(peen_data) < metadata$training_window)
      peen_data <- c(peen_data, rep(NA, times=metadata$training_window-length(peen_data)))
    return(peen_data)
  }
}
