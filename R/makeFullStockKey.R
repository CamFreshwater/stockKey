## Short script to build a more complete stock key
# April 21, 2020

library(tidyverse)

scStockKey <- read.csv(here::here("data", "southCoastStockKey.csv")) %>%
    mutate(stock = toupper(Stock)) %>%
    select(stock, Region1Name, Region2Name, Region3Name)

# list of observed stocks from high seas
stockKeyHS <- readRDS(here::here("data", "highSeasChinoookStockKey.RDS")) %>%
# stockKeyHS <- readRDS(here::here("data", "tempStockList.rds")) %>%
  # select(stock = STOCK_1, region, -HSS_REGION_1) %>%
  full_join(., scStockKey, by = "stock") %>% 
  mutate(Region1Name = as.character(Region1Name),
         Region2Name = as.character(Region2Name),
         Region3Name = as.character(Region3Name)) %>% 
  distinct()

# list of observed stocks from WCVI troll
stockKeyTroll <- readRDS(here::here("data", "wcviTrollStocks.rds")) 

# list of stocks from MGL SNP analyses
stockKeySNP <- read.csv(here::here("data", "snpStockKey.csv")) %>% 
  select(snp_repunit = repunit, snp_cu = CU_NAME, stock = collection)

stockKey1 <- stockKeyTroll %>% 
  full_join(., stockKeyHS, by = c("stock", "Region1Name")) %>% 
  # full_join(., stockKeySNP, by = "stock") %>% 
  distinct()

# Associate misspelled and unknown stocks with higher level regions
stockKeyOut <- stockKey1 %>% 
    select(stock, region, Region1Name
           #, snp_cu
           ) %>%
    mutate(
      #add unknown stocks
      stock = case_when(
        stock == "BIGQUL@LANG" ~ "BIG_Q",
        TRUE ~ stock
      )
      ,
      Region1Name = case_when(
        # grepl("Yukon River", snp_cu) ~ "Alaska_Yukon_R",
        grepl("CLEAR_C", stock) ~ "Central_Valley_fa",
        grepl("CHICKA", stock) ~ "SSE_Alaska",
        stock == "BUTTE_CR_SP" ~ "Central_Valley_sp",
        stock %in% c("BUTTE_CR_F", "AMERICAN_RIVER") ~ "Central_Valley_fa",
        grepl("BIG_CR", stock) ~ "Willamette_R",
        grepl("CLACK", stock) ~ "Willamette_R",
        stock == "BIG_BOULDER_CR" ~ "NSE_Alaska_Chilkat_R",
        stock %in% c("CLEARWATERRFA", "SALMON_R_F") ~ "Snake_R_fa",
        grepl("CEDA", stock) ~ "C_Puget_Sound",
        stock %in% c("BEAR_CR_SUFA") ~ "C_Puget_Sound",
        stock %in% c("APPLEGATE_CR", "CHETCO_R") ~ "N_California/S_Oregon_Coast",
        stock == "AMERICAN_SP" ~ "Central_Valley_sp",
        stock == "ALSEA_R" ~ "Mid_Oregon_Coast",
        grepl("PIT", stock) ~ "LWFR-Su",
        stock == "SOLDUC_F" ~ "Washington_Coast",
        stock == "WALKER" ~ "UPFR",
        stock == "HOMATHKO" ~ "SOMN",
        stock == "SALOOMPT" ~ "NOMN",
        grepl("SNOHOMISH", stock) ~ "N_Puget_Sound",
        grepl("LYO", stock) ~ "Snake_R_fa",
        grepl("ADAMS", stock) ~ "SOTH",
        grepl("SKAGIT", stock) ~ "N_Puget_Sound",
        grepl("UMPQUA", stock) ~ "Mid_Oregon_Coast",
        grepl("MAMQUAM", stock) ~ "SOMN",
        grepl("SQUAMISH", stock) ~ "SOMN",
        grepl("SOOS_CR", stock) ~ "S_Puget_Sound",
        grepl("GREEN", stock) ~ "S_Puget_Sound",
        grepl("WILLA", stock) ~ "Washington_Coast",
        grepl("SKYKOMISH", stock) ~ "C_Puget_Sound",
        stock == "BUTE" ~ "SOMN",
        grepl("SILMIL", stock) ~ "U_Columbia_R_su/fa",
        grepl("WARM", stock) ~ "U_Columbia_R_su/fa",
        grepl("HOH_RI", stock) ~ "Washington_Coast",
        grepl("CLE_ELM", stock) ~ "S_Puget_Sound",
        grepl("QUINA", stock) ~ "Washington_Coast",
        grepl("ABERN", stock) ~ "L_Columbia_R_fa",
        grepl("WOSS", stock) ~ "ECVI",
        grepl("CHEAK", stock) ~ "ECVI",
        grepl("QUATSE", stock) ~ "ECVI",
        grepl("HORSEY", stock) ~ "UPFR",
        grepl("HORSEF", stock) ~ "MUFR",
        grepl("OKANAG", stock) ~ "U_Columbia_R_su/fa",
        grepl("WENATCH", stock) ~ "U_Columbia_R_su/fa",
        grepl("COLE", stock) ~ "N_California/S_Oregon_Coast",
        grepl("KWIN", stock) ~ "Nass",
        grepl("WANN", stock) ~ "NOMN",
        grepl("HIRS", stock) ~ "NOMN",
        grepl("DEAN", stock) ~ "NOMN",
        grepl("CRAIG", stock) ~ "Stikine",
        grepl("DUDI", stock) ~ "Taku",
        grepl("ANDR", stock) ~ "SSE_Alaska_Stikine_R",
        grepl("SWEET", stock) ~ "Skeena Mid",
        grepl("MORI", stock) ~ "Skeena Bulkley",
        grepl("L_KAL", stock) ~ "Skeena Lower",
        grepl("DESC", stock) ~ "U_Columbia_R_su/fa",
        grepl("LITTLEC", stock) ~ "N_Puget_Sound",
        grepl("STILLAG", stock) ~ "N_Puget_Sound",
        grepl("TYNE", stock) ~ "N_Puget_Sound",
        grepl("SERP", stock) ~ "LWFR-F",
        grepl("ELK", stock) ~ "MUFR",
        grepl("CHILL", stock) ~ "LWFR-F",
        grepl("BIG_Q", stock) ~ "ECVI",
        grepl("EUCH", stock) ~ "N_Oregon_Coast",
        grepl("QUEE", stock) ~ "Washington_Coast",
        grepl("WHITE", stock) ~ "L_Columbia_R_sp",
        grepl("NOOK", stock) ~ "N_Puget_Sound",
        grepl("SLOQUET", stock) ~ "LWFR-Sp",
        grepl("RAFT", stock) ~ "NOTH",
        grepl("PUNT", stock) ~ "ECVI",
        grepl("SHAK", stock) ~ "Stikine",
        grepl("LEMI", stock) ~ "NOTH",
        grepl("SIUS", stock) ~ "Mid_Oregon_Coast",
        grepl("TOUL", stock) ~ "Central_Valley_fa",
        grepl("ENTI", stock) ~ "Mid_and_Upper_Columbia_R_sp",
        grepl("STAN", stock) ~ "Central_Valley_fa",
        grepl("TAKIA", stock) ~ "NOMN",
        grepl("PORTE", stock) ~ "SOMN",
        grepl("BULK", stock) ~ "Skeena Bulkley",
        grepl("ELWHA", stock) ~ "Washington_Coast",
        grepl("NANA", stock) ~ "ECVI",
        stock == "LITTLE" ~ "SOTH",
        grepl("TRASK", stock) ~ "N_Oregon_Coast",
        grepl("CHEHA", stock) ~ "Washington_Coast",
        grepl("EAGL", stock) ~ "SOTH",
        grepl("NEST", stock) ~ "N_Oregon_Coast",
        grepl("COWEE", stock) ~ "L_Columbia_R_fa",
        grepl("LOBST", stock) ~ "N_California/S_Oregon_Coast",
        grepl("PIST", stock) ~ "N_California/S_Oregon_Coast",
        grepl("HUNTER", stock) ~ "N_California/S_Oregon_Coast",
        grepl("THOMP", stock) ~ "SOTH",
        grepl("SANDY", stock) ~ "L_Columbia_R_fa",
        grepl("KENNET", stock) ~ "UPFR",
        grepl("WILLOW", stock) ~ "UPFR",
        grepl("SLIM", stock) ~ "UPFR",
        grepl("YUBA", stock) ~ "Central_Valley_fa",
        grepl("NEHA", stock) ~ "N_Oregon_Coast",
        grepl("WINCHUK", stock) ~ "N_California/S_Oregon_Coast",
        grepl("CLEEL", stock) ~ "Mid_Oregon_Coast",
        grepl("TRAPP", stock) ~ "Taku_R",
        stock == "EEL_F" ~ "California_Coast",
        grepl("SQUIN", stock) ~ "Skeena Upper",
        grepl("ATN", stock) ~ "NOMN",
        grepl("CHEWU", stock) ~ "Mid_and_Upper_Columbia_R_sp",
        grepl("CHIW", stock) ~ "Mid_and_Upper_Columbia_R_sp",
        grepl("NEVI", stock) ~ "UPFR",
        grepl("PTARM", stock) ~ "UPFR",
        grepl("NAHAT", stock) ~ "LWFR-F",
        grepl("TUY", stock) ~ "Stikine",
        region == "WCVI" ~ "WCVI",
        region == "NASS" ~ "Nass",
        region == "SKEENA MID" ~ "Skeena Mid",
        region == "SKEENA LOWER" ~ "Skeena Lower",
        region == "MUFR" ~ "MUFR",
        region == "Alaska" ~ "SSE_Alaska",
        grepl("COLUMBIA-SP", region) ~ "Mid_and_Upper_Columbia_R_sp",
        grepl("UPPER COLUMBIA-SU", region) ~ "U_Columbia_R_su/fa",
        grepl("TRINITY", region) ~ "Klamath_R",
        region == "SNAKE-SP/SU" ~ "Snake_R_sp/su",
        region == "MID COL-SP" ~ "Mid_and_Upper_Columbia_R_sp",
        region == "UPPER WILLAMETTE" ~ "L_Columbia_R_fa",
        region == "UP WILLAMETTE" ~ "L_Columbia_R_fa",
        region == "CENTRAL VALLEY-F" ~ "Central_Valley_fa",
        region == "CENT VAL-F" ~ "Central_Valley_fa",
        region == "CENTRAL VALLEY-SP" ~ "Central_Valley_sp",
        TRUE ~ as.character(Region1Name)
      )
    ) %>%
  select(-region) %>% 
  distinct() %>% 
  left_join(., 
            stockKey1 %>% 
              select(Region1Name, Region2Name, Region3Name) %>% 
              distinct(), 
            by = "Region1Name") %>% 
  distinct() %>% 
  # glimpse()
  # add higher level regional aggregates
  mutate(Region2Name =
           case_when(
             Region1Name == "Nass" ~ "North/Central BC",
             Region1Name == "LWTH" ~ "Fraser Early",
             Region1Name == "Juan_de_Fuca" ~ "Washington Coast/Juan de Fuca",
             Region1Name == "Hood_Canal" ~ "Hood Canal",
             Region1Name == "Central_Valley_fa" ~ "California",
             Region1Name == "Alsek" ~ "North/Central BC",
             Region1Name == "Alaska" ~ "Alaska South SE",
             Region1Name == "L_Columbia_R_sp" ~ "Spring Cowlitz",
             Region1Name == "Snake_R_sp/su" ~ "Snake Sp-Su",
             Region1Name == "U_Columbia_R_su/fa" ~ 
               "Up-Columbia S-F",
             Region1Name == "Mid_and_Upper_Columbia_R_sp" ~ 
               "Mid-Columbia Brights/Upriver Brights",
             Region1Name == "Mid_Columbia_R_tule" ~ 
               "Mid-Columbia Brights/Upriver Brights",
             stock %in% c("SKAGIT_SU", "SKYKOMISH_SU") ~ "Puget Sound Summer",
             stock == "NOOKSACK_SP@KE" ~ "Puget Sound Spring",
             Region1Name == "N_Puget_Sound" ~ "North Puget Sound Fall",
             Region1Name == "C_Puget_Sound" ~ "Central Puget Sound Fall",
             Region1Name == "S_Puget_Sound" ~ "South Puget Sound Fall",
             grepl("GOLD", stock) ~ "West Coast Hatchery",
             grepl("TOQUA", stock) ~ "West Coast Hatchery",
             Region1Name %in% c("CONUMA", "ROBERTSON", "NITINAT", "THORNTON") ~ 
               "West Coast Hatchery",
             Region1Name == "WCVI" ~ "West Coast Wild",
             grepl("QUINS", stock) ~ "Upper Strait of Georgia",
             grepl("NIMP", stock) ~ "North/Central BC",
             Region1Name == "ECVI" ~ 
               "Lower Strait of Georgia/Lower GS Hatchery",
             stock %in% c("CAPILANO") ~ "Fraser Late",
             stock %in% c("BUTE", "DEVEREUX", "HOMATHKO", "KLINAKLINI") ~ 
               "Upper Strait of Georgia",
             Region1Name == "SOMN" ~ 
               "Lower Strait of Georgia/Lower GS Hatchery",
             Region1Name %in% c("Mid_Oregon_Coast", 
                                "N_California/S_Oregon_Coast",
                                "N_Oregon_Coast") ~ 
               "Oregon Coastal North Migrating",
             Region1Name == "Washington_Coast" ~ 
               "Washington Coast/Juan de Fuca",
             Region1Name == "Snake_R_fa" ~ "Snake Fall",
             Region1Name == "L_Columbia_R_fa" ~ "Fall Cowlitz/Willamette",
             Region1Name %in% c("Central_Valley_sp",
                                "Klamath_R",
                                "California_Coast") ~ "California",
             Region1Name == "Taku_R" ~ "Alaska South SE",
             Region2Name == "Califormia" ~ "California",
             Region2Name == " Up-Columbia S-F" ~ "Up-Columbia S-F",
             TRUE ~ as.character(Region2Name)
           ),
         Region3Name = 
           case_when(
             grepl("Puget", Region1Name) ~ "Puget Sound",
             grepl("Oregon", Region1Name) ~ "Oregon/California",
             Region1Name == "Washington_Coast" ~ "Washington Coast",
             Region3Name == "Coastal Washington" ~ "Washington Coast",
             grepl("NIMP", stock) ~ "North/Central BC",
             Region1Name == "ECVI" ~ "SOG",
             stock %in% c("CAPILANO") ~ "Fraser River",
             grepl("CHEAK", stock) ~ "SOG",
             Region1Name == "SOMN" ~ "SOG",
             grepl("Snake", Region1Name) ~ "Snake",
             grepl("Columbia", Region1Name) ~ "Columbia",
             grepl("California", Region2Name) ~ "Oregon/California",
             Region2Name == "Alaska South SE" ~ "Alaska South SE",
             Region2Name == "North/Central BC" ~ "North/Central BC",
             TRUE ~ as.character(Region3Name)
           ),
         #even higher level aggreagtes for preliminary modeling
         Region4Name =
           case_when(
             Region3Name %in% c("Washington Coast", "Oregon/California") ~ 
               "CoastUS",
             Region3Name %in% c("SOG", "Puget Sound", "Fraser River") ~ 
               "SalSea",
             Region3Name %in% c("Columbia", "Snake") ~ "ColR",
             Region3Name == "North/Central BC" ~ "NBC",
             Region3Name == "Alaska South SE" ~ "SEAK",
             TRUE ~ Region3Name
           )) %>% 
  distinct() %>% 
  arrange(Region4Name, Region3Name, Region2Name, Region1Name, stock)

stockKeyOut %>% 
  group_by(stock) %>% 
  filter(n()>1) 

# check for gaps
stockKeyOut %>%
  select(stock, Region1Name, Region2Name) %>%
  filter(is.na(Region2Name)) %>%
  distinct()
stockKeyOut %>%
  select(stock, Region1Name:Region3Name) %>%
  filter(is.na(stock) | is.na(Region1Name) | is.na(Region2Name) |
           is.na(Region3Name))
stockKeyOut %>%
  select(stock, Region1Name, Region4Name) %>%
  filter(is.na(Region4Name)) %>%
  distinct()

## Incorporate CWT data
cwt <- readRDS(here::here("data", "cwt_stock_key.RDS")) %>% 
  mutate(stock = toupper(stock),
         gsi_stock = NA,
         Region1Name = NA) %>% 
  select(-release)

# first match automatically using amatch
for (i in 1:nrow(cwt)) {
  match <- stringdist::amatch(cwt$stock[i], stockKeyOut$stock, maxDist = 8)
  cwt$gsi_stock[i] <- stockKeyOut$stock[match]
  cwt$Region1Name[i] <- stockKeyOut$Region1Name[match]
}

cwt_out <- cwt %>% 
  mutate(
    Region1Name = case_when(
      grepl("COLE RIVERS", stock) ~ "Rogue_R",
      grepl("COLUMBIA R UPRIVER S", stock) ~ "L_Columbia_R_sp",
      stock == "COWLITZ R    26.0002" ~ "L_Columbia_R_fa",
      stock == "MINTER CR    15.0048" ~ "S_Puget_Sound",
      grepl("WELLS HATCH", stock) ~ "U_Columbia_R_su/fa",
      stock == "WHITE R      10.0031" ~ "C_Puget_Sound",
      state == "AK" ~ "Alaska",
      basin == "UPTR" ~ "SOTH",
      basin == "SKNA" ~ "Skeena Bulkley",
      basin %in% c("SWVI", "NWVI") ~ "WCVI",
      rmis_region %in% c("SAFA", "CECA") ~ "Central_Valley_fa",
      rmis_region == "KLTR" ~ "Klamath_R",
      basin == "CLEA" ~ "Snake_R_fa",
      stock == "LYONS FERRY HATCHERY" ~ "Snake_R_fa",
      rmis_region == "SNAK" ~ "Snake_R_sp/su",
      basin %in% c("UPSN", "SALM", "SIYA") ~ "Snake_R_sp/su",
      basin %in% c("DESC", "UMAT", "HOO", "KLIC", "CRGNG", "WAGN") ~ 
        "U_Columbia_R_su/fa",
      rmis_region == "UPCR" ~ "U_Columbia_R_su/fa",
      basin %in% c("WILL", "YOCL") ~ "Willamette_R",
      basin %in% c("SAND", "SAWA", "GREL") ~ "L_Columbia_R_fa",
      basin %in% c("TILN", "NEHA") ~ "N_Oregon_Coast",
      basin == "UMPQ" ~ "Mid_Oregon_Coast",
      basin == "SIXE" ~ "N_California/S_Oregon_Coast",
      basin == "ROGU" ~ "Rogue_R",
      basin %in% c("LEWI", "WIND", "COWL") ~ "L_Columbia_R_sp",
      basin == "GHLC" ~ "Washington_Coast",
      rmis_region %in% c("WILP", "NWC") ~ "Washington_Coast",
      rmis_region == "HOOD" ~ "Hood_Canal",
      rmis_region == "JUAN" ~ "Juan_de_Fuca",
      rmis_region == "MPS" ~ "C_Puget_Sound",
      rmis_region %in% c("NPS", "NOWA", "SKAG") ~ "N_Puget_Sound",
      rmis_region == "SPS" ~ "S_Puget_Sound",
      TRUE ~ Region1Name
      ),
    Region2Name = NA
    ) %>%
  left_join(., 
            stockKeyOut %>% 
              select(-stock, -Region2Name) %>% 
              distinct(), 
            by = c("Region1Name")) %>% 
  mutate(
    Region3Name = case_when(
      rmis_region %in% c("GST", "JNST") ~ "SOG",
      TRUE ~ Region3Name
    ),
    Region4Name = case_when(
      rmis_region == "GST" ~ "SalSea",
      TRUE ~ Region4Name
    ),
    id_type = "cwt"
  ) %>% 
  select(stock, Region1Name:id_type) %>%
  distinct()

# n_occur <- data.frame(table(cwt_out$stock))
# n_occur[n_occur$Freq > 1, ]

# combine cwt and gsi stock keys w/ identifiers
stock_key_out_cwt <- stockKeyOut %>% 
  mutate(id_type = "gsi") %>%
  select(stock, Region1Name:id_type) %>% 
  rbind(., cwt_out) %>% 
  #add pst specific aggregates based on Appendix E
  mutate(pst_agg = case_when(
    Region3Name == "Oregon/California" ~ "CA_ORCST",
    Region3Name == "Washington Coast" ~ "WACST",
    Region3Name == "Puget Sound" ~ "PSD",
    Region3Name %in% c("Alaska South SE", "North/Central BC") ~ "NBC_SEAK",
    Region1Name == "LWFR-F" ~ "FR-late",
    Region3Name == "Fraser River" ~ "FR-early",
    Region1Name %in% c("L_Columbia_R_fa") ~ "CR-tule",
    Region1Name %in% c("L_Columbia_R_sp", "Snake_R_sp/su",
                       "Mid_and_Upper_Columbia_R_sp", "Willamette_R") ~ 
      "CR-sp&su",
    Region1Name %in% c("U_Columbia_R_su/fa", "Snake_R_fa",
                       "Mid_Columbia_R_tule") ~ "CR-bright",
    TRUE ~ Region3Name
  ))

saveRDS(stock_key_out_cwt, here::here("data", "generated", "finalStockList_Apr2020.rds"))
write.csv(stock_key_out_cwt, here::here("data", "generated", "finalStockList_Apr2020.csv"),
          row.names = FALSE)

