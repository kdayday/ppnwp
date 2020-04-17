# # This script preprocesses the netcdf files to:
# 1. Replace night time values with 0's instead of NaN's
# (so night time values can be easily identified and ignored)
# 2. Rename dimensions
# The major preprocessing is for the telemetry file; the
# ensemble file will ignore the if statements and just do a
# renaming of the dimensions.

# This script is somewhat legacy from my (Kate's) original
# exploration of the data. Some data preprocessing happens
# here; some happens in the run.R script (curtailment
# removal). Between this and using the get_netcdf_telemetry_data
# function from the forecasting package, the telemetry
# data gets renamed/reformatted a couple times before
# it's used in the run.R script. Redundant, but so it goes.


library(ncdf4)
library(here)

export_reformatted_netcdf <- function(fname, var_names, units, out_name, dim_names, dim_units, dim_vals, dim_longnames,
                                      replaceNaN=T,
                                      data_dir = here::here("data"),
                                      vname = 'f') { # In this set of data, every file has one variable named 'f'
  dims <- mapply(ncdim_def, name=dim_names, units=dim_units, vals=dim_vals, longname=dim_longnames, SIMPLIFY = FALSE)

  pvars <- mapply(function(v, u) {ncvar_def(v, u, dims, missval=NA, compression = 9)}, var_names, units, SIMPLIFY=FALSE)

  nc_new <- nc_create(file.path(data_dir, out_name), pvars)

  for (i in seq_along(fname)){
    nc <- nc_open(paste(data_dir, fname[i], sep="/"))
    data <- ncvar_get(nc, vname)
    nc_close(nc)

    if (replaceNaN) {
      if (var_names[i]=='Cos zenith'){
        sun_up <- !is.na(data) & data > 0
        data[is.na(data)] <- 0
      }else{
        # Data pre-processing: if the sun is down,
        data[!sun_up & is.na(data)] <- 0
        # Additionally, there appears to very frequently be one power value at the end of the day that is missing
        indices <- arrayInd(which(is.na(data)), dim(data))
        for (j in seq_len(dim(indices)[1])) {
          if (sun_up[indices[j,1], indices[j,2]-1, indices[j,3]] & !(sun_up[indices[j,1], indices[j,2]+1, indices[j,3]])) {
            data[indices[j,1], indices[j,2], indices[j,3]] <- 0
          }
        }
      }
    }
    ncvar_put(nc_new, pvars[[i]], data, count=pvars[[i]][['varsize']])
  }

  # Always close when finished!
  nc_close(nc_new)
}

# -----------------------------------------------------------------------------
# Telemetry dimension data
dim_names <- c('Day', 'Hour', 'SiteID')
dim_units <- c('', '', '')
dim_vals <- list(1:1096, (1:288)/12, 1:18)
dim_longnames <- c("Day number starting Jan 1, 2016", "", "PV plant site ID")


# Times when the sun is down are replaced with 0 values rather than NaN's. NaN's are reserved for actual missing data.
fname <- c("qc1_cos_solar_zenith_av5m.nc", "qc1_hsl5m.nc")
var_names <- c( "Cos zenith", "hsl_power")
units <- c("", "MW")
out_name <- 'telemetry.nc'

export_reformatted_netcdf(fname, var_names, units, out_name, dim_names, dim_units, dim_vals, dim_longnames)


