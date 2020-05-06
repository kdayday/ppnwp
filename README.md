# ppnwp
Post-Process Numerical Weather Prediction ensembles

This package depends on the R forecasting package.
The ppnwp package does the data cleaning and preparation 
for the training and forecasting functions in the forecasting
package. 
There is an example script showing how to use Maxar data
to use the function in forecasting. This example is set up to 
run rolling forecasts, like those in the submitted Bayesian
model averaging article. To use:

1. Download and install ppnwp: After downloading, open Rstudio
and create a new project in the local ppnwp folder. Under 
Rstudio's Build window, click "Install and Restart" to build 
your local copy of the package.

2. Create a data/ folder within your local ppnwp/ directory.
Populate data with these files from Maxar: "qc1_hsl5m.nc", 
"qc1_cos_solar_zenith_av5m.nc", "noqc_curtailflag5m.nc", 
"fcst_members_powernew.nc". Also add the "Site_max_power.csv" 
file, emailed separately.

3. Run the "data_cleaning.R" script. This creates a new file,
"telemetry.nc", in the data folder. This needs to only be run
once.

4. Finally, you are ready to run the example script, "run.R".
This script can be run from the command line with a variety of
configurations (see the comments on lines 15-47). There are also
two example batch scripts, "run_example_1_week_raw_ensemble.bat"
and "run_example_3_days_BMA.bat" to show a couple configurations 
(namely, a rolling forecast over 1 week based on an empirical 
distribution of the raw ensemble, and a rolling forecast over 3 
days using BMA with a sliding training window)

Some notes about the configuration:
* The script is currently only complete for rolling forecasts, so
only run with is_rolling=TRUE
* The rolling forecast is an alternate format to the issue-time 
based format that we are moving towards, so it has some specific
idiosyncrasies:
* Lead-time will be the same for each valid time you are 
  interested in. A lead-time of 4 will produce a rolling 4-hour-ahead
  forecast.
  * To forecast over a specific horizon, give date_first_issue
     the first valid time you are interested in and date_last_valid
     the last valid time you are interested in.
  * In this case, horizon will be the total number of forecasts you
     make, and it must match the range given by date_first_issue to
     date_last_valid. So if date_first_issue=20180101_00 and 
     date_last_valid=20180101_23, then horizon=24
  * Give update-rate the same value as you give horizon (we do not 
     want to use update-rate in this alternate format). 

The batch scripts show examples of how to deal with these temporal
parameters. A single run of run.R will forecast one site; the batch
scripts iterate over the sites. One last useful parameter is 
group_directory, which you can use to specify a recognizable name
where the results from all the sites will be clustered; otherwise,
they will end up in a randomly UUID-named directory.

Once you run a batch script, you can navigate into the new Results/
directory to find your results, which includes a metrics.csv that
summarizes what happened over the 11 sites. 

Hopefully this script will give you the tools to play around 
with the bma_ens_models function and the fc_bma class (and the 
get_discrete_continuous_model function in particular) in the 
forecasting package. Look at the R/get_bma_ts.R script in the ppnwp 
package for the how those functions were called. As you look around,
you can insert the browser() function in any of these scripts in order
to stop execution at that point so you can look around in the console.
Just make sure to "Install and Restart" the ppnwp or forecasting
package after you modify, before you try to run it again.
