#' Conduct site-level forecasting with the desired method
#'
#' For the given forecasting method, this function conducts the appropriate
#' training method and forecasts for the test period. Exports .Rdata file of
#' results to the "Runtime data" directory
#' @param issue A lubridate time stamp of the issue time
#' @param ensemble A list of data=[issue x step x member] array of all
#'   ensemble data (historical + test) and issuetime=vector of POSIXct time
#'   stamps
#' @param telemetry A list of data=vector of telemetry and validtime=vector of
#'   POSIXct times
#' @param sun_up A [day x issue x step] matrix of booleans
#' @param site String, site name
#' @param AC_rating Site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @param lm_formula Formula for BMA linear regression
#' @param runtime_data_dir Directory to save .Rdata results
#' @export
forecast_by_issue_time <- function(issue, ensemble, telemetry,
                                   sun_up, site, AC_rating, metadata,
                                   lm_formula, runtime_data_dir){

  tictoc::tic(paste("Total computation time for site ", site, sep=''))

  # Define the series of time indices in the test period
  t_idx_series <- sapply(1:metadata$horizon, FUN=issue_2_valid_index,
                         issue=issue, metadata=metadata, telemetry=telemetry)

  ens_test <- if (metadata$is_rolling) {
    ensemble$data[1, t_idx_series, ]
    } else {ensemble$data[which(issue==ensemble$issuetime), , ]}
  first_valid <- issue + hours(ifelse(metadata$is_rolling, 0, metadata$lead_time))

  ts <- switch(metadata$forecast_type,
               "sliding"=get_bma_ts(first_valid, t_idx_series, ens_test, ensemble, telemetry,
                                    sun_up, site, AC_rating, metadata,
                                    lm_formula),
               "sliding_emos"=get_emos_ts(first_valid, t_idx_series, ens_test, ensemble, telemetry,
                                          sun_up, site, AC_rating, metadata),
               "constant_bma"=get_bma_ts(first_valid, t_idx_series, ens_test, ensemble, telemetry,
                                          sun_up, site, AC_rating, metadata,
                                          lm_formula),
               "raw" = get_raw_ts(first_valid, ens_test, site, AC_rating, metadata),
               "binned" =get_binned_ts(first_valid, ens_test, site, AC_rating, metadata),
               "time-of-day"=get_bma_ts(first_valid, t_idx_series, ens_test, ensemble, telemetry,
                                        sun_up, site, AC_rating, metadata,
                                        lm_formula),
               "time-of-day_emos"=get_emos_ts(first_valid, t_idx_series, ens_test, ensemble,
                                              telemetry, sun_up, site,
                                               AC_rating, metadata),
               "climate" =get_clim_ts(first_valid, telemetry, sun_up, site, AC_rating, metadata),
               "peen" = get_peen_ts(first_valid, t_idx_series, telemetry, sun_up, site,
                                               AC_rating, metadata),
               "ch-peen"=get_chpeen_ts(first_valid, t_idx_series, telemetry, sun_up, site,
                                                   AC_rating, metadata),
               stop("unknown forecast type"))

  t_f <- tictoc::toc() # Forecast time
  runtime <- t_f$toc - t_f$tic

  # Test telemetry and ensemble test logic are the same for all methods
  tel_test <- telemetry$data[t_idx_series]
  # Save the run time data in R
  data_fname <- paste("data site ", site, ".RData", sep="")

  # In the future, this may be moved out so all forecasts are saved in single file
  save(ts, tel_test, ensemble, runtime, AC_rating, file=file.path(runtime_data_dir, data_fname))

}
