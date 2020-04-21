#' Load ensemble forecast data
#'
#' Loads in ensemble forecast data into the form: [day x issue x step x member]
#'
#' If input file is in Maxar form, assumes a NETCDF file of the dimensions: [Day
#' x Hour x Site x Lead time x member] Maxar data can be loaded to either match
#' the form above or in a "rolling" format over the course of the year in the
#' form [1 x 1 x all steps x member] to make annual average metrics easier
#'
#' If input file in in ECMWF format, assumes a NETCDF file of the dimensions:
#' [day x issue x step x member]
#'
#' @param fname file name
#' @param members A vector of member indices
#' @param site Site index
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @param date_start A lubridate: Start date of data to load
#' @param ... Additional parameters to load-in subfunctions
#' @export
get_forecast_data <- function(fname, members, site, metadata, date_start, ...) {

  # Open file
  nc <- ncdf4::nc_open(fname)

  data = tryCatch({
    # Is this Maxar's format?
    if (all(names(nc$dim)==c("lon",  "lat",  "lev",  "time", "ens" ))){
      data <- get_maxar_ensemble(nc, members, site, metadata, date_start, ...)
    } else stop("Unrecognized forecast file format; ECMWF format not implemented")
    # TODO Megan to add ECMWF option
  },  finally = {
    # Close the file!
    ncdf4::nc_close(nc)
  })

  return(data)
}

#' Subfunction to load in ECMWF data
#'
#' Loads in ensemble forecast data into the form: [day x issue x step x member]
#' If input file is in Maxar form, assumes a NETCDF file of the dimensions: [Day
#' x Hour x Site x Lead time x member] Maxar data can be loaded to either match
#' the form above or in a "rolling" format over the course of the year in the
#' form [1 x 1 x all steps x member] to make annual average metrics easier
#' @param nc An open NetCDF object
#' @param members A vector of member indices
#' @param site Site index
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @param date_start A lubridate: Start date of data to load
#' @param vname NetCDF variable name
#' @param truncate Boolean: Whether or not to truncate the forecasts at the site
#'   maximum power
#' @param date_data_start A lubridate: Date of first day in file
#' @param AC_rating AC power rating
get_maxar_ensemble <- function(nc, members, site, metadata, date_start,
                           vname="powernew", truncate=T,
                           date_data_start=lubridate::ymd(20160101),
                           AC_rating=NULL) {

  check_maxar_parameters(nc, metadata, site)

  if (truncate & is.null(AC_rating)) stop("Site maximum power required to truncate forecasts.")

  # Calculate netcdf date constants
  ndays <- get_ndays(date_start, metadata$date_benchmark_end)
  start_day <- get_start_day(date_data_start, date_start)

  if (metadata$is_rolling) {

    dim_counts <- c(ndays, metadata$ts_per_day, 1, 1, 1)

    # Get a matrix for this member, site, and lead time
    member_data <- function(member) {
      dim_starts <- c(start_day,1, site, metadata$lead_time, member)
      return(as.vector(t(ncdf4::ncvar_get(nc, varid=vname,
                                          start=dim_starts, count=dim_counts))))
    }

    # Get a [time x member] matrix at this site
    # (time is rolling along day, hour)
    data <- sapply(members, FUN = member_data, simplify ="array")
    if (truncate) {
      data[which(data > AC_rating)] <- AC_rating
    }

    # Reformat to [day x issue x step x member] format, but use
    # only a single day/issue time so that metrics for entire
    # year can be calculated all at once
    data <- array(data, dim=c(1, 1, ndays*ts_per_day, length(members)))
  } else {

    tictoc::tic("Ensemble load-in time along the diagonal")
    # Get the minimum rectangle of data from the NetCDF that contains the desired data,
    # to be extracted along the diagonals of the matrix
    dim_counts <- c(ndays, metadata$ts_per_day, 1, metadata$horizon, max(members))
    dim_starts <- c(start_day, 1, site, metadata$lead_time, 1)
    # [days x hours x lead time x member]
    data_rectangle <- array(ncdf4::ncvar_get(nc, varid=vname, start=dim_starts, count=dim_counts),
                            dim=c(ndays, metadata$ts_per_day, metadata$horizon, max(members)))

    data <- sapply(members, FUN=get_maxar_data_by_issue, data_rectangle=data_rectangle,
                   date_start=date_start, metadata=metadata, simplify="array")
    tictoc::toc()

    # [step x issue (rolling) x member] to [day x issue (per day) x step x member]
    data <- aperm(array(aperm(data, perm = c(1,3,2)),
                        dim=c(metadata$horizon, length(members),
                              metadata$ts_per_day/metadata$update_rate, ndays)),
                  perm=c(4,3,1,2))
  }
  return(data)
}

#' Subfunction to get single data point by issue and horizon
#'
#' Presumes 1-hour resolution of indices in data_rectangle
#' @param h Horizon index
#' @param issue A lubridate: issue time and hour
#' @param member Member index
#' @param data_rectangle Array of data from the NetCDF file
#' @param date_start A lubridate: Start date of data to load
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @keywords internal
get_maxar_data_by_horizon <- function(h, issue, member, data_rectangle,
                                      date_start, metadata) {

  return(data_rectangle[get_ndays(date_start, issue) + floor(h/metadata$ts_per_day),
                        (lubridate::hour(issue) + metadata$lead_time + h -2 )%%metadata$ts_per_day + 1,
                        h, member])
}

#' Subfunction to get diagonal data by issue time
#'
#' @param member Member index
#' @param data_rectangle Array of data from the NetCDF file
#' @param date_start A lubridate: Start date of data to load
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @keywords internal
get_maxar_data_by_issue <- function(member, data_rectangle,
                                    date_start, metadata) {
  return(sapply(as.list(seq(from=as.POSIXlt(date_start),
                            to=as.POSIXlt(metadata$date_benchmark_end +
                                            lubridate::days(1) -
                                            lubridate::hours(metadata$update_rate)),
                            by=paste(metadata$update_rate, "hours"))),
                FUN = function(issue, member) {sapply(1:metadata$horizon,
                                                      FUN=get_maxar_data_by_horizon,
                                                      issue=issue, member=member,
                                                      data_rectangle=data_rectangle,
                                                      date_start=date_start, metadata=metadata)},
                member=member, simplify="array"))
}

get_ecmwf_data <- function() {
  # TODO Megan to implement
  stop("Not implemented")
}

#' Subfunction to error-check temporal parameters for Maxar load-in
#'
#' @param nc An open NetCDF object
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @param site Site index
check_maxar_parameters <- function(nc, metadata, site) {
  if (metadata$update_rate < 1 || metadata$update_rate%%1!=0) {stop("Update rate must be hourly, by at least 1 hour")}
  if (metadata$resolution !=1) stop("Maxar lookup function assemes resolution of 1 hour.")
  if (metadata$horizon%%metadata$resolution!=0) stop("Horizon must be a multiple of resolution")
  if (metadata$horizon > nc$dim[[4]]$len) stop("Horizon cannot be longer than available lead times in Maxar matrix")
  if (!(site %in% nc$dim[[3]]$vals)) stop("Site index not valid")
}

#' Get a vector of telemetry data
#'
#' Selects either Maxar or NSRDB format and loads data vector
#' Time-point selection is a consecutive sequence
#' @param fname file name
#' @param site Site index
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @param date_start A lubridate: Start date of data to load
#' @return A vector of telemetry
#' @export
get_telemetry_data <- function(fname, site, metadata, date_start, ...) {

  # Open file
  nc <- ncdf4::nc_open(fname)

  data = tryCatch({
    # Is this Maxar's format?
    if (all(names(nc$dim)==c('Day', 'Hour', 'SiteID'))){
      data <- get_maxar_telemetry(nc, site, metadata, date_start, ...)
    } else stop("Unrecognized forecast file format; NSRDB format not implemented")
    # TODO Megan to add NSRDB option
  },  finally = {
    # Close the file!
    ncdf4::nc_close(nc)
  })

}

#' Load data from a NETCDF file of telemetry
#'
#' Assumed file dimensions: Day x Hour x Site
#' Time-point selection is a consecutive sequence
#' @param nc Open NetCDF file
#' @param site Site index
#' @param metadata Metadata list including date end, temporal parameters,
#'   time-steps per day, rolling or not, etc.
#' @param date_start A lubridate: Start date of data to load
#' @param date_data_start A lubridate: Date of first day in file
#' @param vname NetCDF variable name
#' @return A vector of telemetry
#' @export
get_maxar_telemetry <- function(nc, site, metadata, date_start,
                                date_data_start=lubridate::ymd(20160101),
                                vname="hsl_power") {

  # Calculate netcdf date constants
  ndays <- get_ndays(date_start, metadata$date_benchmark_end)
  start_day <- get_start_day(date_data_start, date_start)
  dim_counts <- c(ndays, metadata$ts_per_day, 1)

  data <- ncdf4::ncvar_get(nc, varid=vname, start=c(start_day,1,site), count=dim_counts)

  return(as.vector(t(data)))
}

#' Calculate number of days in the sequence
#' @param date_start A lubridate: Start date of data to load
#' @param date_end A lubridate: End date of data to load
#' @return Number of days in requested data sequence
#' @export
get_ndays <- function(date_start,date_end) {
  lubridate::interval(date_start, date_end)/days(1) + 1
}

#' Calculate start day's index since the beginning of data availability
#' @param date_data_start A lubridate: Date of first day in file
#' @param date_start A lubridate: Start date of data to load
#' @return Index number of first requested day
#' @export
get_start_day <- function(date_data_start, date_start){
  lubridate::interval(date_data_start, date_start)/days(1) + 1
}
