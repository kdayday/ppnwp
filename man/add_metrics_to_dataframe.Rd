% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluation.R
\name{add_metrics_to_dataframe}
\alias{add_metrics_to_dataframe}
\title{Fill one row of data.frame df with this site's metrics}
\usage{
add_metrics_to_dataframe(df, forecast_runs, issue_times, telemetry, df_idx,
  max_power, runtime, metadata, aggregate_function)
}
\arguments{
\item{df}{A data frame to fill with metrics for all the sites}

\item{forecast_runs}{A list of ts_forecast objects}

\item{issue_times}{A list of timestamps, same length as forecast_runs}

\item{telemetry}{Vector of telemetry}

\item{df_idx}{Index of data frame row (likely index of site)}

\item{max_power}{Site's AC rating or maximum load power}

\item{runtime}{A value in seconds}

\item{metadata}{A data.frame of forecast parameters}

\item{aggregate_function}{A list of functions (mean or sum) for how to
aggregate the metrics over the forecast runs}
}
\value{
data.frame df with a row added for this site
}
\description{
CRPS, interval width, and MAE are normalized by the site's AC power rating.
}
