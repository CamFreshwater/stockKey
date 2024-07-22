## Trim for Seb

library(tidyverse)


stock_key <- readRDS(
  here::here("data", "generated", "finalStockList_Jul2024.rds")
  )


dum <- stock_key %>% 
  mutate(
    juv_marine = case_when(
      Region1Name %in% c("Mid_and_Upper_Columbia_R_sp", "Snake_R_sp/su") ~ 
        "Upper Col. Yearling",
      Region1Name %in% c("L_Columbia_R_fa", "Snake_R_fa", 
                         "Mid_Columbia_R_tule") ~"Col. Coastal",
      Region1Name %in% c("Willamette_R", "L_Columbia_R_sp", 
                         "U_Columbia_R_su/fa") ~ "Col. North",
      Region1Name %in% c("Fraser_Spring_4.2", "Fraser_Spring_5.2",
                         "Fraser_Summer_5.2") ~ "Fraser Yearling",
      Region1Name == "Fraser_Summer_4.1" ~ Region1Name, 
      Region1Name == "N_California/S_Oregon_Coast" | 
        Region2Name == "California" ~ "Cali",
      Region2Name == "Oregon Coastal North Migrating" | pst_agg == "WACST" ~
        "WA/OR Coastal",
      Region1Name %in% c("Fraser_Fall", "SOMN") | pst_agg %in% c("PSD", "SOG") ~ 
        "SoG Coastal",
      pst_agg %in% c("WCVI", "NBC_SEAK", "Russia", "Yukon") ~ pst_agg
    )
  ) %>% 
  select(stock, stock_group = Region1Name, juv_marine)

saveRDS(
  dum, here::here("data", "generated", "seb_key.rds")
)
