## {{title}}
## Shock calibration -- created by ermeeth2::create_shock() on {{date}}
##
## This script runs with `calib_new_base` (the calibration after the baseline
## has been applied), `firstyear`, `baseyear`, `lastyear` and `shockyear`
## already in scope. It must finish by defining `shock_ch`: a data.frame of
## `year` plus the exogenous variables the shock changes.

# Define the series necessary to calibrate the scenario. Names are lower case.
series <- c("DWD_C01") %>% tolower

# Load the selected series over the simulation range
selection <- calib_new_base %>% select(year, all_of(series))

## Change in exogenous variables
##
## The line below is a placeholder: it multiplies world demand by 1 from the
## shock year onwards, which changes nothing. Replace it with the shock you
## actually want, e.g. a permanent 1% increase:
##
##   dwd_c01 = ifelse(year >= shockyear, dwd_c01 * 1.01, dwd_c01)
##
shock_ch <- mutate(
  selection,
  dwd_c01 = ifelse(year >= shockyear, dwd_c01 * 1, dwd_c01)
)

## To shock every commodity rather than one, loop over the commodity list:
##
##   list_com <- get_sec_com()$commodities
##   for (i in list_com) {
##     selection <- mutate(selection,
##       !!paste0("dwd_", i) := ifelse(year >= shockyear,
##                                     get(paste0("dwd_", i)) * 1.01,
##                                     get(paste0("dwd_", i))))
##   }
##   shock_ch <- selection
