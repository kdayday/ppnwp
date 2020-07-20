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
#' @param sun_up An [issue x step] matrix of booleans
#' @param site String, site name
#' @param max_power Maximum load or site's AC power rating
#' @param metadata A data.frame of forecast parameters
#' @param lm_formula Formula for BMA linear regression
#' @export
forecast_by_issue_time <- function(issue, ensemble, telemetry,
                                   sun_up, site, max_power, metadata,
                                   lm_formula){

  # Define the series of time indices in the test period
  t_idx_series <- sapply(1:metadata$horizon, FUN=issue_2_valid_index,
                         issue=issue, metadata=metadata, telemetry=telemetry)

  ens_test <- if (metadata$is_rolling) {
    ensemble$data[1, t_idx_series, ]
    } else {ensemble$data[which(issue==ensemble$issuetime), , ]}

  ts <- switch(metadata$forecast_type,
               "bma_sliding"=get_bma_ts(issue, t_idx_series, ens_test, ensemble, telemetry,
                                    sun_up, site, max_power, metadata,
                                    lm_formula),
               "emos_sliding"=get_emos_ts(issue, t_idx_series, ens_test, ensemble, telemetry,
                                          sun_up, site, max_power, metadata),
               "bma_constant"=get_bma_ts(issue, t_idx_series, ens_test, ensemble, telemetry,
                                          sun_up, site, max_power, metadata,
                                          lm_formula),
               "raw" = get_raw_ts(issue, ens_test, site, max_power, metadata),
               "binned" =get_binned_ts(issue, ens_test, site, max_power, metadata),
               "bma_time-of-day"=get_bma_ts(issue, t_idx_series, ens_test, ensemble, telemetry,
                                        sun_up, site, max_power, metadata,
                                        lm_formula),
               "emos_time-of-day"=get_emos_ts(issue, t_idx_series, ens_test, ensemble,
                                              telemetry, sun_up, site,
                                               max_power, metadata),
               "climate" =get_clim_ts(issue, telemetry, sun_up, site, max_power, metadata),
               "peen" = get_peen_ts(issue, t_idx_series, telemetry, sun_up, site,
                                               max_power, metadata),
               "ch-peen"=get_chpeen_ts(issue, t_idx_series, telemetry, sun_up, site,
                                                   max_power, metadata),
               stop("unknown forecast type"))

}
