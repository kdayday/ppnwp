# # This script preprocesses the netcdf files to:
# 1. Rename dimensions
# 2. Replace night time values with 0's instead of NaN's
# (so night time values can be easily identified and ignored)
# 3. Double-check curtailment is removed
# 4. Average hourly

library(ncdf4)
library(here)

replaceNaN <- T
data_dir <- here::here("data")
out_name <- 'telemetry.nc'

# -----------------------------------------------------------------------------
# Telemetry dimension data
# Step 1: Rename dimensions
dim_names <- c('Day', 'Hour', 'SiteID')
dim_units <- c('', '', '')
dim_vals <- list(1:1096, 1:24, 1:18)
dim_longnames <- c("Day number starting Jan 1, 2016", "", "PV plant site ID")
vname  <-  'f' # In this set of data, every file has one variable named 'f'
dims <- mapply(ncdim_def, name=dim_names, units=dim_units, vals=dim_vals, longname=dim_longnames, SIMPLIFY = FALSE)

# Times when the sun is down are replaced with 0 values rather than NaN's. NaN's are reserved for actual missing data.
var_names <- "hsl_power"
units <- "MW"

nc <- nc_open(paste(data_dir, "qc1_cos_solar_zenith_av5m.nc", sep="/"))
zenith <- ncvar_get(nc, vname)
nc_close(nc)


nc <- nc_open(paste(data_dir, "qc1_hsl5m.nc", sep="/"))
data <- ncvar_get(nc, vname)
nc_close(nc)

# ------------------------------------------------------
# Step 2: Replace nighttime values
if (replaceNaN) {
  sun_up <- !is.na(zenith) & zenith > 0
  zenith[is.na(zenith)] <- 0
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

# -------------------------------------------------------
# Step 3: remove curtailment
nc <- nc_open(paste(data_dir, "noqc_curtailflag5m.nc", sep="/"))
curtailed <- ncvar_get(nc, vname)
nc_close(nc)

data[curtailed] <- NaN

# -------------------------------------------------------
# Step 4: Average hourly
data <- aperm(apply(X = data, MARGIN = c(1,3), FUN=function(minutely) {
  sapply(1:24, FUN=function(i) {mean(minutely[(12*(i-1)+1):(12*i)], na.rm = F)})}),
  perm=c(2,1,3))

pvar <- ncvar_def(var_names, units, dims, missval=NA, compression = 9)
nc_new <- nc_create(file.path(data_dir, out_name), pvar)
ncvar_put(nc_new, pvar, data, count=pvar[['varsize']])

# Always close when finished!
nc_close(nc_new)


