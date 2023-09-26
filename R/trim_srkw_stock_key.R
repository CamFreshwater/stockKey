## Export simplified key for SRKW researchers
# Sep 26, 2023

library(tidyverse)


key <- readRDS(
  here::here("data", "generated", "finalStockList_Jul2023.rds")
  ) %>% 
  mutate(
    aggregate  = case_when(
      grepl(".2", Region1Name) ~ "Fraser_Yearling", 
      grepl("Fraser", Region1Name) ~ Region1Name,
      Region1Name == "SOMN" ~ "SOG",
      TRUE ~ pst_agg
    )
  )  


fraser_out <- key %>% 
  filter(
    grepl("Fraser", Region1Name)
  ) %>% 
  mutate(
   aggregate = fct_relevel(
     aggregate, "Fraser_Yearling", "Fraser_Summer_4.1", "Fraser_Fall"
     )
  ) %>% 
  select(aggregate, management_unit = Region1Name, stock) %>% 
  arrange(aggregate, management_unit) 


all_out <- key %>% 
  select(aggregate, management_unit = Region1Name, stock) %>% 
  arrange(aggregate, management_unit) 


write.csv(fraser_out,
          here::here("data", "generated", "fraser_only_stock_key_srkw.csv"),
          row.names = FALSE)  
write.csv(all_out,
          here::here("data", "generated", "stock_key_srkw.csv"),
          row.names = FALSE)  
