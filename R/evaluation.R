#' Export forecasting metrics to .csv
#'
#' Cycles through forecasts in the runtime_data_dir (presumably organized by
#' site) to assess metrics, including CRPS (weighted and unweighted) and data
#' quality metrics.
#'
#' @param out_dir Main output directory to save .csv
#' @param runtime_data_dir Directory where .Rdata results are saved
#' @export
export_metrics_to_csv <- function(out_dir, runtime_data_dir){

  df <- data.frame()

  files <- list.files(runtime_data_dir)

  for(f in seq_along(files)){

    # Load in forecast_runs, telemetry, ensemble, AC_rating
    load(file.path(runtime_data_dir , files[f]))

    # Include in for loop so that additions aren't appended every round
    cnames <- c("missing_sunup_rate", "number_validatable_forecasts",
                "CRPS", "CRPS tails", "CRPS left", "CRPS right",
                "CRPS center", "90% interval width", "MAE")
    # Each of these metrics should either be summed or averaged
    aggregate_function <- c(sum, sum, mean, mean, mean, mean, mean, mean, mean)

    if (metadata$forecast_type %in% c("bma_sliding", "bma_time-of-day")) {
      cnames <- c(cnames, names(forecast_runs[[1]]$forecasts[!is.na(forecast_runs[[1]]$forecasts)][[1]]$geometry_codes))
      aggregate_function <- c(aggregate_function, sum, sum, sum, sum, sum)
    }

    df <- add_metrics_to_dataframe(df, forecast_runs, issue_times,
                                   telemetry, f, AC_rating,
                                  runtime, metadata, aggregate_function)
  }

  colnames(df)[1+seq_along(cnames)] <- cnames

  write.csv(df, file=file.path(out_dir, "metrics.csv"))
}

#' Fill one row of data.frame df with this site's metrics
#'
#' CRPS, interval width, and MAE are normalized by the site's AC power rating.
#' @param df A data frame to fill with metrics for all the sites
#' @param forecast_runs A list of ts_forecast objects
#' @param issue_times A list of timestamps, same length as forecast_runs
#' @param telemetry Vector of telemetry
#' @param df_idx Index of data frame row (likely index of site)
#' @param AC_rating Site's AC rating
#' @param runtime A value in seconds
#' @param metadata A data.frame of forecast parameters
#' @param aggregate_function A list of functions (mean or sum) for how to
#'   aggregate the metrics over the forecast runs
#' @return data.frame df with a row added for this site
#' @export
add_metrics_to_dataframe <- function(df, forecast_runs, issue_times,
                                     telemetry, df_idx, AC_rating,
                                     runtime, metadata, aggregate_function) {

  df[df_idx, "Runtime [sec]"] <- runtime
  quantiles=seq(0.01, 0.99, by=0.01)

  # Collect average metrics across the forecast runs
  forecast_run_metrics <- sapply(seq_along(issue_times), FUN=get_metrics_for_single_run,
                                 forecast_runs=forecast_runs, issue_times=issue_times,
                                 metadata=metadata, telemetry=telemetry)
  # Average of averages (or sum or sums)
  average_metrics <- sapply(seq_len(dim(forecast_run_metrics)[1]),
                            FUN=function(i) do.call(aggregate_function[[i]], list(forecast_run_metrics[i,])))

  df[df_idx, 1+seq_along(average_metrics)] <- average_metrics

  # Name this row by site
  rownames(df)[df_idx] <- forecast_runs[[1]]$location
  return(df)
}


#' Get a vector of the average metrics over a single forecast run
#'
#' @param i Index of forecast run in the list of forecast_runs and issue_times
#' @param forecast_runs A list of ts_forecast object
#' @param issue_times A list of timestamps, same length as forecast_runs
#' @param metadata A data.frame of forecast parameters
#' @param telemetry Vector of telemetry
#' @return A vector of average metrics
get_metrics_for_single_run <- function(i, forecast_runs, issue_times, metadata, telemetry) {

  # Find the corresponding subset of telemetry data for this forecast run
  t_idx_series <- sapply(1:metadata$horizon, FUN=issue_2_valid_index,
                         issue=issue_times[[i]], metadata=metadata, telemetry=telemetry)
  tel_test <- telemetry$data[t_idx_series]

  # Calculate results
  stats <- forecasting::get_sundown_and_NaN_stats(forecast_runs[[i]], tel_test)

  qs <- forecasting::QS(forecast_runs[[i]], tel_test, quantiles)
  crps_unweighted <- forecasting::qwCRPS(forecast_runs[[i]], tel_test, weighting="none", quantiles=quantiles, qs=qs)/AC_rating
  crps_tails <- forecasting::qwCRPS(forecast_runs[[i]], tel_test, weighting="tails", quantiles=quantiles, qs=qs)/AC_rating
  crps_left <- forecasting::qwCRPS(forecast_runs[[i]], tel_test, weighting="left", quantiles=quantiles, qs=qs)/AC_rating
  crps_right <- forecasting::qwCRPS(forecast_runs[[i]], tel_test, weighting="right", quantiles=quantiles, qs=qs)/AC_rating
  crps_center <- forecasting::qwCRPS(forecast_runs[[i]], tel_test, weighting="center", quantiles=quantiles, qs=qs)/AC_rating
  interval_width <- forecasting::sharpness_avg(forecast_runs[[i]], tel_test, alpha=.10, normalize.by=AC_rating)$mean
  mae <- forecasting::MAE(forecast_runs[[i]], tel_test, normalize.by=AC_rating)

  results <- c(stats$`Sunup missing telemetry rate`, stats$`Validatable forecasts`,
               crps_unweighted, crps_tails, crps_left, crps_right, crps_center,
               interval_width, mae)

  # Extract geometry code summary statistics
  if (metadata$forecast_type %in% c("bma_sliding", "bma_time-of-day")) {
    for (key in names(forecast_runs[[i]]$forecasts[!is.na(forecast_runs[[i]]$forecasts)][[1]]$geometry_codes))
      results <- c(results, sum(sapply(forecast_runs[[i]]$forecasts[!is.na(forecast_runs[[i]]$forecasts)],
                                   FUN=function(fc) return(fc$geometry_codes[[key]]))))
  }
  return(results)
}

#' Export a [issue time x step x quantile] array of quantiles to HDF5 format
#'
#' Quantile forecasts are extracted from the list of ts_forecast objects and
#' exported in array format to an HDF5 file. Can equivalently export a file
#' with a NetCDF extension; ncdf4 package is used for NetCDF/HDF5 handling.
#'
#' @param forecast_runs A list of ts_forecast objects
#' @param fname Path to output file
#' @export
export_quantiles_to_h5 <- function(forecast_runs, fname) {
  results <- get_quantile_array(forecast_runs)

  dims <- mapply(ncdf4::ncdim_def, name=c('Issue_index', 'Step_index', "Percentile"), units=c('', '', '%'),
                 vals=list(seq(dim(results$quantiles)[1]), seq(dim(results$quantiles)[2]), results$percents),
                 longname=c("Index in the sequence of issue times",
                            "Index in the number of steps from the issue time",
                            "Percentile (out of 100%)"),
                 SIMPLIFY = FALSE)
  qvar <- ncdf4::ncvar_def("Power", "MW", dims, missval=NA, longname="Power", compression = 9)

  nc <- ncdf4::nc_create(fname, qvar, force_v4=TRUE)
  ncdf4::ncvar_put(nc, qvar, results$quantiles, count=qvar[['varsize']])
  ncdf4::nc_close(nc)
}

#' Extract an [issue time x step x quantile] array of quantile forecasts
#'
#' Quantile forecasts are extracted from the list of ts_forecast objects (each
#' an individual forecast run), at the resolution within the prob_forecast
#' objects (defaults to 0.1%)
#'
#' @param forecast_runs A list of ts_forecast objects
#' @return a [issue time x step x quantile] array
#' @export
get_quantile_array <- function(forecast_runs) {
  percents <- 100*forecast_runs[[1]]$forecasts[[which(sapply(forecast_runs[[1]]$forecasts, is.prob_forecast))[1]]]$quantiles$q
  num_quantiles <- length(percents)
  q <- sapply(forecast_runs, FUN = function(ts) sapply(ts$forecasts, FUN = function(f) {
    if (is.prob_forecast(f)) return(f$quantiles$x) else return(rep(0, times=num_quantiles))
  }, simplify="array"), simplify="array")
  q <- aperm(q, c(3,2,1))
  return(list(percents=percents, quantiles=q))
}
