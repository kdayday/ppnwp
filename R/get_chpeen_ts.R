#' Get time-series forecast using CH-PeEn method
#'
#' This method generates a static 24-hour or similar forecast using all data at
#' the same time of day in the training period only. This is a static forecast
#' that does not evolve as more observations are realized in the evaluation
#' period.
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
get_chpeen_ts <- function(t_idx_series, all_site_tel, sun_up, sites,
                          site_idx, site_max_p, metadata){

  # Train
  data.input <- train_ch_peen(t_idx_series, all_site_tel, sun_up, site_idx, metadata)

  # Forecast
  ts <- forecasting::ts_forecast(data.input, metadata$date_benchmark_start,
                    time_step=metadata$resolution, scale='site',
                    location=paste("Site", site, sep=" "),
                    method = 'empirical',
                    max_power=AC_rating)
  return(ts)
}

#' Subfunction to get input data for CH-PeEn forecast
#'
#' Get matrix of static, complete-history PeEn data
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   all_site_tel time indices
#' @param all_site_tel A [site x time] matrix of telemetry
#' @param site String, site name
#' @param metadata A data.frame of forecast parameters
train_ch_peen <- function(t_idx_series, all_site_tel, sun_up, site_idx, metadata) {

  daily_matrix <- t(sapply(seq_len(metadata$ts_per_day),
                           FUN=function(i) return(all_site_tel[site_idx, seq(i, t_start_idx-metadata$ts_per_day+i,
                                                                             by=metadata$ts_per_day)]),
                           simplify="array"))
  # For comparing across forecast methods, restrict data to the same times when the ensemble forecasts the sun is up
  data_matrix <- t(sapply(t_idx_series,
                          FUN = function(i) {if (sun_up[site_idx, i]) {daily_matrix[(i-1)%%metadata$ts_per_day+1,]}
                            else {return(rep(0, times=dim(daily_matrix)[2]))}},
                          simplify="array"))
  return(data_matrix)
}
