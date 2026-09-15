## {{title}}
## Baseline calibration -- created by ermeeth2::create_baseline() on {{date}}
##
## This script runs with `OGcalib` (the raw calibration), `firstyear`,
## `baseyear`, `lastyear` and `shockyear` already in scope. It must finish by
## defining `baseline_ch`: a data.frame of `year` plus the exogenous variables
## whose baseline trajectory you want to change.

# Load the calibration over the simulation range
calib <- OGcalib %>% filter(year %in% c(firstyear:lastyear))

# Pick the series you are going to change. Names are lower case.
series <- c("DWD_C01") %>% tolower

# Load the selected series over the simulation range
selection <- calib %>% select(year, all_of(series))

## Change in exogenous variables
##
## The line below is a placeholder: it multiplies world demand by 1, which
## changes nothing. Replace it with the trajectory you actually want, e.g.
##
##   dwd_c01 = ifelse(year >= 2030, dwd_c01 * 1.01, dwd_c01)
##
baseline_ch <- mutate(
  selection,
  dwd_c01 = dwd_c01 * 1
)

## `baseline_ch` is what the rest of the pipeline reads. If you build the
## baseline in several pieces, merge them here:
##
##   baseline_ch <- merge(baseline_ch1, baseline_ch2)
