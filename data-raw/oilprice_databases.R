oilprice_fra <- readRDS("tests/oilprice_fra_c28_s32.rds")

usethis::use_data(oilprice_fra, overwrite = TRUE)

oilprice_fra_agg <- readRDS("tests/oilprice_fra_c28_s32_commodities_sectors.rds")

usethis::use_data(oilprice_fra_agg, overwrite = TRUE)
