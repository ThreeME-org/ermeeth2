## ThreeME data base
library(tidyverse)
library(arrow)
library(fst)
##
threeme_standard <- readRDS("tests/standard_shocks.rds")

scenarios <- threeme_standard$scenario |> unique()

list_threeme <-  purrr::set_names(scenarios, scenarios) |> map(~threeme_standard |> filter(scenario==.x) )
purrr::set_names(scenarios, scenarios) |> map(~threeme_standard |> filter(scenario==.x) |> saveRDS(str_c(.x,".rds")) )
# saveRDS(list_threeme,"tests/lists_threeme")


Sys.setenv("ARROW_R_DEV"="true")

write_parquet(threeme_standard, "threeme_full.parquet")

threeme_standard$year |> unique()

write_fst(threeme_standard, "threeme.fst")

plop <- read_fst("threeme.fst")

library(data.table)

data.table::fwrite(threeme_standard,"plop.csv")

library(qs)

qsave(threeme_standard, "myfile.qs")
plopqs<-qread("myfile.qs")

