#' Export forecasting metrics to .csv
#'
#' Cycles through forecasts in the runtime_data_dir (presumably organized by
#' site) to assess metrics, including CRPS (weighted and unweighted) and data
#' quality metrics. If "export_plots" is selected, this will generate
#' reliability, sharpness, quantile score, and PIT histogram plots *for each
#' site*.
#' @param out_dir Main output directory to save graphs
#' @param runtime_data_dir Directory where .Rdata results are saved
#' @param metadata A data.frame of forecast parameters
#' @param export_plots Boolean flag (default=False)
#' @export
export_metrics_to_csv <- function(out_dir, runtime_data_dir,
                                   metadata, export_plots=F){

  df <- data.frame()
  if (export_plots) {
    qs_dir <- file.path(out_dir, "Quantile score plots")
    dir.create(qs_dir, showWarnings = FALSE)

    rel_dir <- file.path(out_dir, "Reliability plots")
    dir.create(rel_dir, showWarnings = FALSE)

    iw_dir <- file.path(out_dir, "Interval width plots")
    dir.create(iw_dir, showWarnings = FALSE)

    pit_dir <- file.path(out_dir, "PIT histograms")
    dir.create(pit_dir, showWarnings = FALSE)
  }

  files <- list.files(runtime_data_dir)

  for(f in seq_along(files)){

    # Load in ts, tel_test, ens_test, AC_rating
    load(file.path(runtime_data_dir , files[f]))

    df <- add_metrics_to_dataframe(df, ts, tel_test, f, AC_rating,
                                  metadata$forecast_type, runtime)

    if (export_plots) {
      plot_reliability(ts, tel_test,
                       fname=file.path(rel_dir, paste("Site", ts$location, "reliability.pdf", sep="_")))
      plot_quantile_score(ts, tel_test,
                          fname=file.path(qs_dir, paste("Site", ts$location, "qs.pdf", sep="_")))
      plot_PIT_histogram(ts, tel_test, nbins=20,
                         fname=file.path(pit_dir, paste("Site", ts$location, "PIT.pdf", sep="_")))
      plot_interval_width(ts, tel_test, normalize.by=AC_rating,
                          fname=file.path(iw_dir, paste("Site", ts$location, "interval_width.pdf", sep="_")))
    }
  }

  write.csv(df, file=file.path(out_dir, "metrics.csv"))
}

#' Fill one row of data.frame df with this site's metrics
#'
#' CRPS, interval width, and MAE are normalized by the site's AC power rating.
#' @param df A data frame to fill with metrics for all the sites
#' @param ts A ts_forecast object
#' @param tel_test Vector of telemetry for the test period, corresponding to the length of ts
#' @param df_idx Index of data frame row (likely index of site)
#' @param AC_rating Site's AC rating
#' @param forecast_type Name of forecast method, e.g., "raw"
#' @param runtime A value in seconds
#' @return data.frame df with a row added for this site
#' @export
add_metrics_to_dataframe <- function(df, ts, tel_test, df_idx, AC_rating,
                                     forecast_type, runtime) {

  df[df_idx, "Runtime [sec]"] <- runtime

  # Calculate results
  stats <- forecasting::get_sundown_and_NaN_stats(ts, tel_test, agg=TRUE)
  df[df_idx, "missing_sunup_rate_1hr"] <- stats$`Sunup missing telemetry rate`
  df[df_idx, "number_validatable_forecasts"] <- stats$`Validatable forecasts`
  quantiles=seq(0.01, 0.99, by=0.01)
  qs <- forecasting::QS(ts, tel_test, quantiles)
  df[df_idx, "CRPS"] <- forecasting::qwCRPS(ts, tel_test, weighting="none", quantiles=quantiles, qs=qs)/AC_rating
  df[df_idx, "CRPS tails"] <- forecasting::qwCRPS(ts, tel_test, weighting="tails", quantiles=quantiles, qs=qs)/AC_rating
  df[df_idx, "CRPS left"] <- forecasting::qwCRPS(ts, tel_test, weighting="left", quantiles=quantiles, qs=qs)/AC_rating
  df[df_idx, "CRPS right"] <- forecasting::qwCRPS(ts, tel_test, weighting="right", quantiles=quantiles, qs=qs)/AC_rating
  df[df_idx, "CRPS center"] <- forecasting::qwCRPS(ts, tel_test, weighting="center", quantiles=quantiles, qs=qs)/AC_rating
  df[df_idx, "90% interval width"] <- forecasting::sharpness_avg(ts, tel_test, alpha=.10, normalize.by=AC_rating, agg=TRUE)$mean
  df[df_idx, "MAE"] <- forecasting::MAE(ts, tel_test, normalize.by=AC_rating, agg=TRUE)
  # Extract geometry code summary statistics
  if (forecast_type %in% c("sliding", "time-of-day", "analog")) {
    for (key in names(ts$forecasts[!is.na(ts$forecasts)][[1]]$geometry_codes))
      df[df_idx, key] <- sum(sapply(ts$forecasts[!is.na(ts$forecasts)], FUN=function(fc) return(fc$geometry_codes[[key]])))
  }
  # Name this row by site
  rownames(df)[df_idx] <- ts$location
  return(df)
}
