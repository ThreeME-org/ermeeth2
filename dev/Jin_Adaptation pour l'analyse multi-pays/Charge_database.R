
#Definition of country and indicators to analysis
country_list     <- c("che","deu","dnk","esp","est","fin","gbr","grc","lux","aut")
country_names    <- c("Switzerland","Germany","Denmark","Spain","Estonia","Finland","Great Britain","Greece","Luxembourg","Austria")
indicateur_list  <- c("GDP","I","DISPINC_AT_VAL","EMS_CO2","CH","X","M","G","DS","Y","UNR")
sindicateur_list <- c("Y","VA","F_L","F_E","F_K","EMS_CI_CO2","EMS_Y_CO2","EMS_MAT_CO2")
scenario_name    <- "carbontax"

#Creation of database for agregation analysis
data_full <- country_list %>%
  map_df(~ readRDS(file = paste0("databases/carbontax_",.x,"_c28_s32_commodities_sectors.rds")) %>%
           as.data.frame() %>%
           filter( variable %in% indicateur_list |
                     grepl("CH_TOE$|CI_TOE_S00.*[0-6]",variable)) %>%
           mutate(country = .x) %>%
           set_names(c("variable","year","baseline",scenario_name,"commodity","sector","country"))
  )

#Creation of database for sectorial analysis
data_sector <- country_list %>%
  map_df(~ readRDS(file = paste0("databases/carbontax_",.x,"_c28_s32_commodities_sectors.rds")) %>%
           filter(grepl(paste(paste0("^",sindicateur_list,
                                     "_S[A-Z0-9]{3}$"),collapse = "|"),variable)) %>%
           mutate(country = .x) %>%
           set_names(c("variable","year","baseline",scenario_name,"commodity","sector","country"))
  )

#Creation of other basic data for analysis
data_FEC <- data_full %>%
  filter(grepl("CH_TOE$|CI_TOE_S00.*[0-6]",variable)) %>%
  aggregate(cbind(baseline,.[,4]) ~ year + country,., FUN = sum) %>%
  set_names(c("year","country","baseline",scenario_name)) %>%
  mutate(variable = "FEC",commodity = NA,sector = NA) %>%
  select(c(variable,year,baseline,scenario_name,commodity,sector,country))

data_full <- data_full %>% rbind(data_FEC)

data_EMS <- data_sector %>%
  filter(grepl("^EMS_.+_CO2_S[A-Z0-9]{3}$",variable)) %>%
  mutate(variable = str_remove(variable,"_(CI|MAT|Y)")) %>%
  aggregate(cbind(baseline,.[,4]) ~ year + country + variable + sector ,., FUN = sum) %>%
  set_names(c("year","country","variable","sector","baseline",scenario_name)) %>%
  mutate(commodity = NA) %>%
  select(c(variable,year,baseline,scenario_name,commodity,sector,country))

data_sector <- data_sector %>% rbind(data_EMS)

rm(data_FEC,data_EMS)
