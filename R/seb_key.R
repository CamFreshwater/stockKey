## Trim for Seb

library(tidyverse)


stock_key <- readRDS(
  here::here("data", "generated", "finalStockList_Nov2024.rds")
  )

seb_key <- read.csv(
  here::here(
    "data", "juvenile-marine-chinook-key-20240829-pardo.csv"
    )
)


dum <- stock_key %>% 
  mutate(
    juv_marine = case_when(
      Region1Name %in% c("NEVI", "NOMN", "Central_Coast") | 
        stock %in% c("DEENA_CR", "PALLANT_CR") ~ "Central BC",
      Region1Name %in% c("Mid_and_Upper_Columbia_R_sp", "Snake_R_sp/su") ~ 
        "Upper Col. Yearling",
      Region1Name %in% c("L_Columbia_R_fa", "Snake_R_fa", 
                         "Mid_Columbia_R_tule") ~"Col. Coastal",
      Region1Name %in% c("Willamette_R", "L_Columbia_R_sp", 
                         "U_Columbia_R_su/fa") ~ "Col. North",
      Region1Name %in% c("Fraser_Spring_4.2", "Fraser Spring 5.2",
                         "Fraser_Spring_5.2",
                         "Fraser_Summer_5.2") ~ "Fraser Yearling",
      Region1Name == "Fraser_Summer_4.1" ~ "Fraser Summer 4.1", 
      Region1Name == "N_California/S_Oregon_Coast" | 
        Region2Name == "California" ~ "Cali",
      Region2Name == "Oregon Coastal North Migrating" | pst_agg == "WACST" ~
        "WA/OR Coastal",
      Region1Name %in% c("Fraser_Fall", "SOMN") | pst_agg %in% c("PSD", "SOG") ~ 
        "Salish Coastal",
      pst_agg == "NBC_SEAK" ~ "NBC/SEAK",
      pst_agg %in% c("WCVI", "Russia", "Yukon") ~ pst_agg
    )
  ) 

# checking for NA regions that might have fallen through the cracks 
filter(dum, is.na(juv_marine))


saveRDS(
  dum, here::here("data", "generated", "seb_key.rds")
)

saveRDS(
  dum, here::here("data", "generated", paste0("seb_key_",ymd(Sys.Date()),".rds"))
)

write_csv(
  dum, here::here("data", "generated", paste0("seb_key_",ymd(Sys.Date()),".csv"))
)
