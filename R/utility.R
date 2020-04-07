#' Load data from a NETCDF file of ensemble forecasts Assumed file dimensions:
#' Day x Hour x Site x Lead time x Ensemble member Returns an array of data:
#' [site x member x time] or [site x member x time x lead time] Site selection
#' and member selection can all be vectors of non-consecutive values Time-point
#' selection is a consecutive sequence
#' @param fname file name
#' @param members A vector of member indices
#' @param sites A vector of sites
#' @param lead_times Forecast lead time or a set of lead times
#' @param date_start A lubridate: Start date of data to load
#' @param date_end A lubridate: End date of data to load
#' @param date_data_start A lubridate: Date of first day in file
#' @param truncate Boolean: Whether or not to truncate the forecasts at the site
#'   maximum power
#' @param site_max_power A vector of the maximum power at ALL sites (not just
#'   those listed in sites)
#' @export

# TODO UPDATE




get_forecast_data <- function(format, ...) {

  # Open file
  nc <- ncdf4::nc_open(fname)

  # TODO CAN i PUT A TRY CATCH BLOCK AROUNDT THIS?

  # Is this Maxar's format?
  if (all(names(nc$dim)==c("lon",  "lat",  "lev",  "time", "ens" ))){
    data <- get_maxar_data(, ...)
  } else stop("Unrecognized forecast file format")
  # TODO ADD ECMWF OPTION

  # Close the file!
  ncdf4::nc_close(nc)

  return(data)
}

get_maxar_data <- function(nc, members, site,  date_start, date_end,
                           update_rate, horizon, lead_time,
                          date_data_start=lubridate::ymd(20160101),
                          ts_per_day=24, vname="powernew", truncate=F,
                          site_max_power=NULL, is_rolling=F) {

  check_maxar_parameters(nc, horizon, update_rate, lead_time, date_start)

  if (truncate & is.null(site_max_power)) stop("Site maximum power required to truncate forecasts.")

  # Calculate netcdf date constants
  ndays <- get_ndays(date_start,date_end)
  start_day <- get_start_day(date_data_start, date_start)

  if (is_rolling) {

    dim_counts <- c(ndays, ts_per_day, 1, 1, 1)

    # Get a matrix for this member, site, and lead time
    member_data <- function(member) {
      dim_starts <- c(start_day,1, site, lead_time, member)
      return(as.vector(t(ncdf4::ncvar_get(nc, varid=vname, start=dim_starts, count=dim_counts))))
    }

    # Get a [time x member] matrix at this site
    # (time is rolling along day, hour)
    data <- sapply(members, FUN = member_data, simplify ="array")
    data[which(data > site_max_power)] <- site_max_power

    # Reformat to [day x issue x step x member] format, but use
    # only a single day/issue time so that metrics for entire
    # year can be calculated all at once
    data <- array(data, dim=c(1, 1, ndays*ts_per_day, length(members)))
  } else {

    # Define subfunction
    get_data_by_horizon <- function(i, issue, member) {
      # Hours are 1-24, not 0-23
      return(data_rectangle[get_ndays(date_start, issue),
                            hour(issue) + 1, lead_time + i - 1, member])
    }
    get_data_by_issue <- function(member) {
      return(sapply(as.list(seq(from=as.POSIXlt(date_start),
                                to=as.POSIXlt(date_end + days(1) - hours(update_rate)),
                                by=paste(update_rate, "hours"))),
                    FUN = function(issue, member) {sapply(1:horizon, FUN=get_data_by_horizon,
                                                          issue=issue, member=member)},
                    member=member, simplify="array"))
    }

    tictoc::tic("Ensemble load-in time along the diagonal")
    # Get the minimum rectangle of data from the NetCDF that contains the desired data,
    # to be extracted along the diagonals of the matrix
    dim_counts <- c(ndays, ts_per_day, 1, horizon, max(members))
    dim_starts <- c(start_day, 1, site, lead_time, 1)
    # [days x hours x lead time x member]
    data_rectangle <- array(ncdf4::ncvar_get(nc, varid=vname, start=dim_starts, count=dim_counts),
                            dim=c(ndays, ts_per_day, horizon, max(members)))

    data <- sapply(members, FUN=get_data_by_issue, simplify="array")
    tictoc::toc()

    # [step x issue (rolling) x member] to [day x issue (per day) x step x member]
    data <- aperm(array(aperm(data, perm = c(1,3,2)), dim=c(horizon, length(members),
                                                        ts_per_day/update_rate, ndays)),
                  perm=c(4,3,1,2))
  }
  return(data)
}

get_ecmwf_data <- function() {
  stop("Not implemented")
}

checK_maxar_parameters <- function(nc, update_rate, resolution, horizon) {
  if (update_rate < 1 || update_rate%%1!=0) {stop("Update rate must be hourly, by at least 1 hour")}
  if (resolution !=1) stop("Maxar lookup function assemes resolution of 1 hour.")
  if (horizon%%resolution!=0) stop("horizon must be a multiple of resolution")
  if (horizon > nc$dim[[4]]$len) stop("Horizon cannot be longer than available lead times in Maxar matrix")
  stop("not implemented")
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
