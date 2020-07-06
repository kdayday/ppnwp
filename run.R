# The forecasting package contains all the generic functions for calculating
# probabilistic forecasts at individual time-points and time-series of
# forecasts (a string of forecasts at a single issue time).
library(forecasting)
# The ppnwp package contains functions for slicing and dicing the input
# or training data, for use by the generic functions in forecasting.
library(ppnwp)
library(lubridate)


# ------------------------------------------------------
# Get inputs
# ------------------------------------------------------

# Determine the default constants.  Types are inferred by the defaults in the list
# forecast_type -> one of : "bma_sliding" "bma_time-of-day" "raw" "binned" "climate" "peen" "ch-peen"
#                           "bma_constant" "emos_sliding" "emos_time-of-day"
# bma_distribution -> one of "beta" or "truncnorm" Defines what distribution type should be used
#                     for a "bma_sliding" or "bma_time-of-day" BMA forecast.
# lead_time -> Forecast lead time in hours (L). If rolling forecast, this time is the same between
#              issue and valid time for each forecast. If not a rolling forecast, this is the time
#             between issue and the first forecast in the run.
# resolution -> Forecast temporal resolution in hours (R)
# horizon -> Forecast temporal horizon in hours (H)
# update_rate -> Time in hours between issuance of subsequent forecasts (U)
# is_rolling -> Boolean. If true, this makes a single ts_forecast for ease of calculating metrics.
#               Must have horizon equal to update rate. Horizon must be consistent
#               with start and end dates.
# members -> List of ensemble member indices to include.
# site -> Power plant site index.
# percent_clipping_threshold -> % of power plant rating to use as clipping threshold, (0,1)
# date_first_issue -> First forecast issue time (ymd_h). For the rolling forecast, this equals the
#                     start of the benchmark. Training data will be selected backwards from there.
# date_last_valid -> Last valid time to include in the benchmark (ymd_h). The first valid time and last
#                     issue time are backcalculated from date_first_issue, date_last_valid, and the temporal parameters.
# training_window -> If a sliding window is used, the training window is a sliding windows in *hours*.
#                 -> If a time-of-day window is used, the appropriate hour will be cherry picked from
#                     this number of *days* this year,
#                     plus a window twice this length the year before
#                 -> If a persistence ensemble is used, this is the number previous non-NA measurements
#                     at the same time-of-day. Ignored for a complete-history PeEn, which uses a static 1 year of data.
#                 -> If a constant BMA forecast is used, the training data is the previous year,
#                    resulting in a constant BMA *model*, though the resulting forecast is not constant.
# lm_intercept -> Boolean. Whether to include a non-zero intercept in BMA linear regression or not (default)
# telemetry_file -> Name of telemetry NetCDF file in the /data folder
# ensemble_file -> Name of enemble forecast NetCDF file in the /data folder
# group_directory -> Desired output folder name, used to group results runs together. Defaults to unique UUID.

defaults <- list(forecast_type="raw",
                 bma_distribution= "beta",
                 lead_time=4,
                 resolution=1,
                 horizon=72,
                 update_rate=72,
                 is_rolling=T,
                 members = "1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41",
                 site = 1,
                 percent_clipping_threshold = 0.995,
                 date_first_issue = "20180101_00",
                 date_last_valid = "20181231_23",
                 training_window = 72,
                 lm_intercept = FALSE,
                 telemetry_file="telemetry.nc",
                 ensemble_file="fcst_members_powernew.nc",
                 group_directory=uuid::UUIDgenerate(TRUE)
)

# Vector arguments should be strings "1,2,3" and will be post-processed to vectors
args <- R.utils::commandArgs(asValues=T, trailingOnly=T, defaults=defaults)
metadata <- data.frame(row.names = "Value")

# Extract all
metadata$forecast_type <- args$forecast_type
metadata$bma_distribution <- args$bma_distribution
metadata$lead_time <- args$lead_time
metadata$resolution <- args$resolution
metadata$horizon <- args$horizon
metadata$is_rolling <- args$is_rolling
metadata$update_rate <- args$update_rate
members <- as.numeric(unlist(strsplit(args$members, ",")))
site <- args$site
metadata$percent_clipping_threshold <- args$percent_clipping_threshold
metadata$date_first_issue <- as.POSIXlt(lubridate::ymd_h(args$date_first_issue))
metadata$date_last_valid <- as.POSIXlt(lubridate::ymd_h(args$date_last_valid))
metadata$training_window <- args$training_window
if (args$lm_intercept) {
  lm_formula <- y~x
} else lm_formula <- y~x+0
ens_name <- args$ensemble_file
tel_name <- args$telemetry_file
group_directory <- args$group_directory

# ------------------------------------------------------
# Calculate remaining temporal constants
# ------------------------------------------------------

metadata$date_first_valid <- if (metadata$is_rolling) {metadata$date_first_issue} else {
metadata$date_first_issue + lubridate::hours(metadata$lead_time)
}
metadata$date_last_issue<- if (metadata$is_rolling) {NULL} else {
  tail(seq(from=metadata$date_first_issue, to=metadata$date_last_valid,
      by=paste(metadata$update_rate, "hours")),1)
}

date_training_start <- switch(metadata$forecast_type,
                              "raw" = metadata$date_first_issue, # No training period
                              "binned" = metadata$date_first_issue, # No training period
                              "climate" = metadata$date_first_issue, # No training period
                              "peen" = metadata$date_first_issue - 2*days(metadata$training_window), # Add an expanded window to ensure enough non-NA points are available
                              "ch-peen" = metadata$date_first_issue-years(1),
                              "bma_constant" = metadata$date_first_issue-years(1),
                              "bma_sliding" = metadata$date_first_issue - days(ceiling(metadata$training_window/24)),
                              "emos_sliding" = metadata$date_first_issue - days(ceiling(metadata$training_window/24)),
                              "bma_time-of-day" = metadata$date_first_issue - years(1) - days(metadata$training_window),
                              "emos_time-of-day" = metadata$date_first_issue - years(1) - days(metadata$training_window),
                              stop("unknown forecast type"))

metadata$ts_per_day <- 24/metadata$resolution

# ------------------------------------------------------
# Set up directories
# ------------------------------------------------------

forecast_name <- switch(metadata$forecast_type,
                        "bma_sliding" = paste("Discrete-", metadata$bma_distribution, " BMA forecast with sliding window", sep=""),
                        "emos_sliding" = "Truncated normal EMOS forecast with sliding window",
                        "bma_constant" = paste("Discrete-", metadata$bma_distribution, " constant BMA forecast", sep=""),
                        "raw" = "Raw ensemble",
                        "binned" = "Binned probability forecast",
                        "bma_time-of-day" = paste("Discrete-", metadata$bma_distribution, " BMA forecast with time-of-day window", sep=""),
                        "emos_time-of-day" = "Truncated normal EMOS forecast with time-of-day window",
                        "climate" = "Climatology forecast",
                        "peen" = "Persistence ensemble",
                        "ch-peen" = "Complete-history persistence ensemble",
                        stop("unknown forecast type"))

# Directory and file locations
data_dir <- here::here("data")
main_dir <- here::here("Results")
dir.create(main_dir, showWarnings = FALSE)
out_dir_parent <- file.path(main_dir, forecast_name)
dir.create(out_dir_parent, showWarnings = FALSE)
AC_rating <- unlist(read.csv(file.path(data_dir, "Site_max_power.csv"), header=F))[site]

out_dir <- file.path(out_dir_parent, group_directory)
dir.create(out_dir, showWarnings = FALSE)

runtime_data_dir <- file.path(out_dir, "Runtime data")
dir.create(runtime_data_dir, showWarnings = FALSE)

# Begin by saving metadata to file separately
write.csv(metadata, file=file.path(out_dir, "metadata.csv"))

# ----------------------------------------------------------------------
# Time-series data load in
# ----------------------------------------------------------------------
tictoc::tic("Time-series data load-in")

# Ensemble data: [issue x step x member]
ensemble <- get_forecast_data(file.path(data_dir, ens_name), members,
                              site, metadata, date_training_start,
                              AC_rating=AC_rating)

# Load telemetry list, including data as data vector over time and a validtime vector
telemetry <- get_telemetry_data(file.path(data_dir, tel_name), site,
                                metadata,
                                date_training_start + lubridate::hours(ifelse(metadata$is_rolling, 0, metadata$lead_time)))

tictoc::toc()

# ----------------------------------------------------------------------
# Loop over forecasts
# ----------------------------------------------------------------------
tictoc::tic("Full forecast and analysis runtime")

warning("Might need to add new NA member handling because NA's no longer removed from input data by lead time.")

# Generate a [issue x step] estimation of whether the sun is up, to avoid forecasting when it is not
# Continuing to test based on power for internal consistency with current ts_forecast practice
sun_up <- apply(ensemble$data, MARGIN = c(1, 2), FUN = check_sunup)

# Translate sun_up to valid time and get issue times of the validation set
if (metadata$is_rolling) {
  sun_up <- sun_up[1,]
  issue_times <- metadata$date_first_valid
} else {
  sun_up <- sapply(telemetry$validtime, FUN=function(valid) {
    sun_up[valid_2_issue_index(valid, metadata, ensemble)]})
  issue_times <- ensemble$issuetime[which(ensemble$issuetime==metadata$date_first_issue):length(ensemble$issuetime)]
}

forecast_runs <- vector(mode = "list", length = length(issue_times))

tictoc::tic(paste("Total computation time for site ", site, sep=''))
# Conduct forecast for each issue time (must sequence along or will return integer)
for (i in seq_along(issue_times)){
  forecast_runs[[i]] <- forecast_by_issue_time(issue_times[i], ensemble, telemetry,
                         sun_up, site, AC_rating, metadata, lm_formula)
}
t_f <- tictoc::toc() # Forecast time
runtime <- t_f$toc - t_f$tic

export_quantiles_to_h5(forecast_runs,
                      fname=file.path(runtime_data_dir, paste("quantiles site ", site, ".h5", sep="")))


# Save the run time data in R
data_fname <- paste("data site ", site, ".RData", sep="")
save(forecast_runs, issue_times, telemetry, ensemble, runtime, AC_rating, metadata,
     file=file.path(runtime_data_dir, data_fname))

export_metrics_to_csv(out_dir, runtime_data_dir, metadata)

tictoc::toc()
