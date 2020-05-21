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


stockKeyRec %>% 
  filter(sc_reg1 == "Fraser Summer 5.2") %>% 
  pull(stock)

# list of stocks from MGL SNP analyses (currently not integrated)
# stockKeySNP <- read.csv(here::here("data", "snpStockKey.csv")) %>% 
#   select(snp_repunit = repunit, snp_cu = CU_NAME, stock = collection)

stockKey1 <- stockKeyTroll %>% 
  full_join(., stockKeyHS, by = c("stock", "Region1Name")) %>% 
  # full_join(., stockKeySNP, by = "stock") %>% 
  distinct()


## Incorporate rec data
# list of observed stocks from south coast rec (ID region1name based on 
# different south coast regional roll up)
remove <- c("_RIVER", "_CREEK")
stockKeyRec <- readRDS(here::here("data", "rec_gsi_stocks.rds")) %>% 
  mutate(trim_stock = str_remove_all(stock, paste(remove, collapse = "|")),
         new_stock = NA,
         Region1Name = NA)

# add based on matches
for (i in 1:nrow(stockKeyRec)) {
  match <- stringdist::amatch(stockKeyRec$trim_stock[i], stockKey1$stock, 
                              maxDist = 6)
  stockKeyRec$new_stock[i] <- stockKey1$stock[match]
  stockKeyRec$Region1Name[i] <- stockKey1$Region1Name[match]
}

# preliminary coarse corrections to rec data
rec_out1 <- stockKeyRec %>% 
  mutate(
    Region1Name = case_when(
      grepl("SOTH", Region1Name) ~ "Fraser_Summer_4.1",
      grepl("Fraser", sc_reg1) ~ gsub(" ", "_", sc_reg1),
      sc_reg1 %in% c("NEVI", "ECVI Fall") ~ "ECVI",
      sc_reg1 %in% c("SWVI", "NWVI") ~ "WCVI",
      sc_reg1 == "North/Central BC" ~ "NOMN",
      sc_reg1 %in% c("Coastal Washington", "US Juan de Fuca") ~ 
        "Washington_Coast",
      TRUE ~ Region1Name
    )
  )

# combine rec, troll, and high seas keys
stockKey2 <- stockKey1 %>% 
  full_join(., rec_out1, by = c("stock", "Region1Name")) %>% 
  distinct()


# Associate misspelled and unknown stocks with higher level regions
stockKeyOut <- stockKey2 %>% 
    select(stock, sc_reg1, region, Region1Name
           #, snp_cu
           ) %>%
    mutate(
      #add unknown stocks
      stock = case_when(
        stock == "BIGQUL@LANG" ~ "BIG_Q",
        TRUE ~ stock
      ),
      Region1Name = case_when(
        grepl("SOTH", Region1Name) ~ "Fraser_Summer_4.1",
        grepl("LWFR-F", Region1Name) ~ "Fraser_Fall",
        grepl("LWFR-Sp", Region1Name) ~ "Fraser_Spring_5.2",
        grepl("LWTH", Region1Name) ~ "Fraser_Spring_4.2",
        grepl("MARIA", stock) ~ "Fraser_Summer_4.1",
        grepl("Fraser", sc_reg1) ~ sc_reg1,
        # grepl("Yukon River", snp_cu) ~ "Alaska_Yukon_R",
        grepl("ATNARK", stock) ~ "NOMN",
        stock %in% c("COWLITZ_HATCHERY_SPRING", "COWLITZ_H_SP") ~ 
            "L_Columbia_R_sp",
        stock %in% c("CHILKO", "L_CARIBOO", "QUESNEL", "STUART") ~
          "Fraser_Summer_5.2",
        Region1Name == "NOTH" ~ "Fraser_Summer_5.2",
        stock %in% c("BAEZAEKO", "BAKER_CR", "BRIDGE", "CHILAKO", "COTTONWOOD",
                     "ENDAKO", "NAZKO", "NECHAKO", "TASEKO", "U_CARIBOO", 
                     "WESTROAD") ~
          "Fraser_Spring_5.2",
        Region1Name == "UPFR" ~ "Fraser_Spring_5.2",
        grepl("CHILCOT", stock) ~ "Fraser_Spring_5.2",
        grepl("SLOQ", stock) ~ "Fraser_Summer_5.2",
        grepl("BIG_SILVER", stock) ~ "Fraser_Summer_5.2",
        grepl("ELKI", stock) ~ "Fraser_Summer_5.2",
        grepl("KUZK", stock) ~ "Fraser_Summer_5.2",
        grepl("PORTA", stock) ~ "Fraser_Fall",
        grepl("ROBERT", stock) ~ "WCVI",
        grepl("SAN JUAN", stock) ~ "WCVI",
        grepl("TENDER", stock) ~ "SOMN",
        grepl("CHEAK", stock) ~ "SOMN",
        grepl("BATTLE", stock) ~ "Central_Valley_fa",
        grepl("SACR", stock) ~ "Central_Valley_fa",
        grepl("FORKS_CR", stock) ~ "Washington_Coast",
        grepl("SHOVEL", stock) ~ "SOMN",
        grepl("CLEAR_C", stock) ~ "Central_Valley_fa",
        grepl("SPRING_CR", stock) ~ "Mid_Columbia_R_tule",
        grepl("CHICKA", stock) ~ "SSE_Alaska",
        stock %in% c("BUTTE_CR_SP", "BUTTE_CREEK", "FEATHER_RIVER_SPRING",
                     "SALMON_RIVER_CA_SPRING") ~ 
          "Central_Valley_sp",
        stock %in% c("BUTTE_CR_F", "AMERICAN_RIVER", "BUTTE_CREEK_FALL", 
                     "FEATHER_RIVER_FALL", "MERCED_RIVER") ~ "Central_Valley_fa",
        grepl("BIG_CR", stock) ~ "Willamette_R",
        grepl("CLACK", stock) ~ "Willamette_R",
        stock == "BIG_BOULDER_CR" ~ "NSE_Alaska_Chilkat_R",
        stock %in% c("CLEARWATERRFA", "SALMON_R_F") ~ "Snake_R_fa",
        grepl("CEDA", stock) ~ "C_Puget_Sound",
        stock %in% c("BEAR_CR_SUFA") ~ "C_Puget_Sound",
        stock %in% c("APPLEGATE_CR", "CHETCO_R") ~ "N_California/S_Oregon_Coast",
        stock == "AMERICAN_SP" ~ "Central_Valley_sp",
        stock == "ALSEA_R" ~ "Mid_Oregon_Coast",
        grepl("PIT", stock) ~ "Fraser_Summer_5.2",
        stock == "SOLDUC_F" ~ "Washington_Coast",
        stock == "WALKER" ~ "Fraser_Spring_5.2",
        stock %in% c("HOMATHKO", "ASHLULM") ~ "SOMN",
        grepl("NEECH", stock) ~ "NOMN",
        grepl("SNOHOMISH", stock) ~ "N_Puget_Sound",
        grepl("LYO", stock) ~ "Snake_R_fa",
        grepl("ADAMS", stock) ~ "Fraser_Summer_4.1",
        grepl("SKAGIT", stock) ~ "N_Puget_Sound",
        grepl("UMPQUA", stock) ~ "Mid_Oregon_Coast",
        grepl("MAMQUAM", stock) ~ "SOMN",
        grepl("SQUAMISH", stock) ~ "SOMN",
        grepl("SALOOM", stock) ~ "NOMN",
        grepl("SALLOOM", stock) ~ "NOMN",
        grepl("SOOS_CR", stock) ~ "S_Puget_Sound",
        grepl("GREEN", stock) ~ "S_Puget_Sound",
        grepl("WILLA", stock) ~ "Washington_Coast",
        grepl("SKYKOMISH", stock) ~ "C_Puget_Sound",
        stock == "BUTE" ~ "SOMN",
        grepl("SIMILKAMEEN", stock) ~ "U_Columbia_R_su/fa",
        grepl("OSOY", stock) ~ "U_Columbia_R_su/fa",
        grepl("SILMIL", stock) ~ "U_Columbia_R_su/fa",
        grepl("WARM", stock) ~ "U_Columbia_R_su/fa",
        grepl("HOH_RI", stock) ~ "Washington_Coast",
        grepl("CLE_ELM", stock) ~ "S_Puget_Sound",
        grepl("QUINA", stock) ~ "Washington_Coast",
        grepl("ABERN", stock) ~ "L_Columbia_R_fa",
        grepl("WHITE", stock) ~ "C_Puget_Sound",
        grepl("WOSS", stock) ~ "ECVI",
        grepl("CHEAK", stock) ~ "SOMN",
        grepl("QUATSE", stock) ~ "ECVI",
        grepl("HORSE", stock) ~ "Fraser_Spring_5.2",
        grepl("OKANAG", stock) ~ "U_Columbia_R_su/fa",
        grepl("WENATCH", stock) ~ "U_Columbia_R_su/fa",
        grepl("COLE", stock) ~ "N_California/S_Oregon_Coast",
        grepl("KWIN", stock) ~ "Nass",
        grepl("KASIK", stock) ~ "NOMN",
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
        grepl("SERP", stock) ~ "Fraser_Fall",
        grepl("ELK", stock) ~ "Mid_Oregon_Coast",
        grepl("CHILL", stock) ~ "Fraser_Fall",
        grepl("BIG_Q", stock) ~ "ECVI",
        grepl("EUCH", stock) ~ "N_Oregon_Coast",
        grepl("QUEE", stock) ~ "Washington_Coast",
        grepl("NOOK", stock) ~ "N_Puget_Sound",
        grepl("SLOQUET", stock) ~ "Fraser_Summer_5.2",
        grepl("RAFT", stock) ~ "Fraser_Summer_5.2",
        grepl("PUNT", stock) ~ "ECVI",
        grepl("SHAK", stock) ~ "Stikine",
        grepl("LEMI", stock) ~ "Fraser_Summer_5.2",
        grepl("SIUS", stock) ~ "Mid_Oregon_Coast",
        grepl("TOUL", stock) ~ "Central_Valley_fa",
        grepl("ENTI", stock) ~ "Mid_and_Upper_Columbia_R_sp",
        grepl("NACH", stock) ~ "Mid_and_Upper_Columbia_R_sp",
        grepl("STAN", stock) ~ "Central_Valley_fa",
        grepl("TAKIA", stock) ~ "NOMN",
        grepl("PORTE", stock) ~ "SOMN",
        grepl("BULK", stock) ~ "Skeena Bulkley",
        grepl("ELWHA", stock) ~ "Washington_Coast",
        grepl("NANA", stock) ~ "ECVI",
        stock == "LITTLE" ~ "Fraser_Summer_4.1",
        grepl("TRASK", stock) ~ "N_Oregon_Coast",
        grepl("CHEHA", stock) ~ "Washington_Coast",
        grepl("EAGL", stock) ~ "Fraser_Summer_4.1",
        grepl("NEST", stock) ~ "N_Oregon_Coast",
        grepl("COWEE", stock) ~ "L_Columbia_R_fa",
        grepl("LOBST", stock) ~ "N_California/S_Oregon_Coast",
        grepl("PIST", stock) ~ "N_California/S_Oregon_Coast",
        grepl("HUNTER", stock) ~ "N_California/S_Oregon_Coast",
        grepl("THOMP", stock) ~ "Fraser_Summer_4.1",
        grepl("SANDY", stock) ~ "L_Columbia_R_fa",
        grepl("KENNET", stock) ~ "Fraser_Spring_5.2",
        grepl("WILLOW", stock) ~ "Fraser_Spring_5.2",
        grepl("SLIM", stock) ~ "Fraser_Spring_5.2",
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
        grepl("NEVI", stock) ~ "Fraser_Spring_5.2",
        grepl("PTARM", stock) ~ "Fraser_Spring_5.2",
        grepl("NAHAT", stock) ~ "Fraser_Fall",
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
  select(-region, -sc_reg1) %>% 
  distinct() %>% 
  left_join(., 
            stockKey2 %>% 
              select(Region1Name, Region2Name, Region3Name) %>% 
              distinct(), 
            by = "Region1Name") %>% 
  distinct() %>%
  # add higher level regional aggregates
  mutate(Region1Name = gsub(" ", "_", Region1Name)) %>% 
  mutate(
    Region2Name =
      case_when(
        Region1Name %in% c("Nass", "Skeena_Lower", "Alsek") ~ "North/Central BC",
        grepl("Fraser_Spring", Region1Name) ~ "Fraser Early",
        grepl("Fraser_Summer", Region1Name) ~ "Fraser Early",
        grepl("Fraser_Fall", Region1Name) ~ "Fraser Late",
        Region1Name == "Juan_de_Fuca" ~ "Washington Coast/Juan de Fuca",
        Region1Name == "Hood_Canal" ~ "Hood Canal",
        Region1Name == "Central_Valley_fa" ~ "California",
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
        stock %in% c("CAPILANO") ~ "SOMN",
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
        Region1Name %in% c("L_Columbia_R_fa", "Willamette_R") ~
          "Fall Cowlitz/Willamette",
        Region1Name %in% c("Central_Valley_sp",
                           "Klamath_R",
                           "California_Coast") ~ "California",
        Region1Name == "Taku_R" ~ "Alaska South SE",
        Region2Name == "Califormia" ~ "California",
        Region2Name == " Up-Columbia S-F" ~ "Up-Columbia S-F",
        # sc_reg1 == "S Mainland Inlets" ~ "Upper Strait of Georgia",
        # sc_reg1 == "North/Central BC" ~ "North/Central BC",
        Region1Name == "SOMN" ~ "Upper Strait of Georgia",
        Region1Name == "NOMN" ~ "North/Central BC",
        TRUE ~ as.character(Region2Name)
      ),
    Region3Name = 
      case_when(
        grepl("Puget", Region1Name) ~ "Puget Sound",
        grepl("Oregon", Region1Name) ~ "Oregon/California",
        Region1Name == "Washington_Coast" ~ "Washington Coast",
        Region3Name == "Coastal Washington" ~ "Washington Coast",
        grepl("NIMP", stock) ~ "North/Central BC",
        Region1Name == "NOMN" ~ "North/Central BC",
        Region1Name == "ECVI" ~ "SOG",
        grepl("Fraser", Region1Name) ~ "Fraser River",
        stock %in% c("CAPILANO") ~ "Fraser River",
        grepl("CHEAK", stock) ~ "SOG",
        Region1Name == "SOMN" ~ "SOG",
        grepl("Snake", Region1Name) ~ "Snake",
        grepl("Columbia", Region1Name) ~ "Columbia",
        grepl("California", Region2Name) ~ "Oregon/California",
        Region2Name == "Alaska South SE" ~ "Alaska South SE",
        Region2Name == "North/Central BC" ~ "North/Central BC",
        Region1Name == "Willamette_R" ~ "Columbia",
        Region1Name == "WCVI" ~ "WCVI",
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
      )
    ) %>%
  # select(-sc_reg1) %>% 
  distinct() %>% 
  arrange(Region4Name, Region3Name, Region2Name, Region1Name, stock)

stockKeyOut %>%
  select(stock, Region1Name, Region3Name) %>%
  filter(is.na(Region3Name)) %>%
  distinct()


stockKeyOut %>% 
  group_by(stock) %>% 
  filter(n()>1)  %>% 
  arrange(stock)

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


# n_occur <- data.frame(table(cwt_out$stock))
# n_occur[n_occur$Freq > 1, ]


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
      grepl("WHITE", stock) ~ "C_Puget_Sound",
      state == "AK" ~ "Alaska",
      basin == "UPTR" ~ "Fraser_Summer_4.1",
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

