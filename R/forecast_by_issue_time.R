#' Conduct site-level forecasting with the desired method
#'
#' For the given forecasting method, this function conducts the appropriate
#' training method and forecasts for the test period. Exports .Rdata file of
#' results to the "Runtime data" directory
#' @param t_idx_series Series of time indices to forecast, relative to the
#'   all_site_tel time indices
#' @param ens_data [site x member x time] array of all ensemble data (historical
#'   + test)
#' @param all_site_tel A [site x time] matrix of telemetry
#' @param sun_up A [site x time] matrix of booleans
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @param lm_formula Formula for BMA linear regression
#' @param runtime_data_dir Directory to save .Rdata results
#' @export
forecast_by_issue_time <- function(t_idx_series, ens_data, all_site_tel,
                             sun_up, site, AC_rating, metadata,
                             lm_formula, runtime_data_dir){

  tictoc::tic(paste("Total computation time for site ", site, sep=''))

  # Test telemetry and ensemble test logic are the same for all methods
  # TODO LOAD IN SHOULD ONLY BE 1 SITE AT A TIME
  # tel_test <- all_site_tel[site_idx, t_idx_series]
  # ens_test <- t(ens_data[site_idx,,t_idx_series])
  # TODO ALSO FIX site_index logic in get_bma_ts

  # TODO WHEN RETURN: FIX DOCUMENTATION IN _ts functions

  ts <- switch(metadata$forecast_type,
               "sliding"=get_bma_ts(t_idx_series, ens_test, ens_data, all_site_tel,
                                    sun_up, site, AC_rating, metadata,
                                    lm_formula),
               "sliding_emos"=get_emos_ts(t_idx_series, ens_test, ens_data, all_site_tel,
                                          sun_up, site, AC_rating, metadata),
               "constant_bma"=get_bma_ts(t_idx_series, ens_test, ens_data, all_site_tel,
                                          sun_up, site, AC_rating, metadata,
                                          lm_formula),
               "raw" = get_raw_ts(ens_test, site, AC_rating, metadata),
               "binned" =get_binned_ts(ens_test, site, AC_rating, metadata),
               "time-of-day"=get_bma_ts(t_idx_series, ens_test, ens_data, all_site_tel,
                                        sun_up, site, AC_rating, metadata,
                                        lm_formula),
               "time-of-day_emos"=get_emos_ts(t_idx_series, ens_test, ens_data,
                                              all_site_tel, sun_up, site,
                                              AC_rating, metadata),
               "climate" =get_clim_ts(tel_test, sun_up, site, AC_rating, metadata),
               "peen" = get_peen_ts(t_idx_series, all_site_tel, sun_up, site,
                                               AC_rating, metadata),
               "ch-peen"=get_chpeen_ts(t_idx_series, all_site_tel, sun_up, site,
                                                   AC_rating, metadata),
               stop("unknown forecast type"))

  t_f <- tictoc::toc() # Forecast time
  runtime <- t_f$toc - t_f$tic

  # Save the run time data in R
  data_fname <- paste("data site ", site, ".RData", sep="")

  # In the future, this may be moved out so all forecasts are saved in single file
  save(ts, tel_test, ens_test, runtime, AC_rating, file=file.path(runtime_data_dir, data_fname))

}
