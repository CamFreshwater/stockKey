
library(tidyverse)

scStockKey <- read.csv(here::here("data", "southCoastStockKey.csv")) %>%
  mutate(stock = toupper(Stock)) %>%
  select(stock, Region1Name, Region2Name, Region3Name)


# list of observed stocks from high seas
stockKeyHS <- readRDS(here::here("data", "highSeasChinoookStockKey.RDS")) %>%
  full_join(., scStockKey, by = "stock") %>% 
  mutate(Region1Name = as.character(Region1Name),
         Region2Name = as.character(Region2Name),
         Region3Name = as.character(Region3Name)) %>% 
  distinct()


# list of observed stocks from WCVI troll
stockKeyTroll <- readRDS(here::here("data", "comm_gsi_stocks.rds")) 


stockKey1 <- stockKeyTroll %>% 
  full_join(., stockKeyHS, by = c("stock", "Region1Name")) %>% 
  distinct()



## PRELIMINARY CLEAN REC DATA --------------------------------------------------

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


# as above but with stocks from south coast
new_rec <- readRDS(here::here("data", "southcoast_updated_stocks.rds"))  %>% 
  rename(sc_reg1 = region) %>% 
  mutate(trim_stock = str_remove_all(stock, paste(remove, collapse = "|")),
         new_stock = NA,
         Region1Name = NA) 

for (i in 1:nrow(new_rec)) {
  match <- stringdist::amatch(new_rec$trim_stock[i], stockKey1$stock, 
                              maxDist = 2)
  new_rec$new_stock[i] <- stockKey1$stock[match]
  new_rec$Region1Name[i] <- stockKey1$Region1Name[match]
}


# preliminary coarse corrections to rec data
rec_out1 <- rbind(stockKeyRec, new_rec) %>% 
  mutate(
    Region1Name = case_when(
      sc_reg1 %in% c("NEVI", "ECVI Fall") ~ "ECVI",
      sc_reg1 %in% c("SWVI", "NWVI") ~ "WCVI",
      sc_reg1 == "North/Central BC" ~ "NOMN",
      sc_reg1 %in% c("Coastal Washington", "US Juan de Fuca") ~ 
        "Washington_Coast",
      TRUE ~ Region1Name
    ),
    region = NA
  ) %>% 
  select(stock, region, Region1Name) %>% 
  distinct() 

# as above but with stocks from hake offload, SRKW diet and juv salmon database
# extra ID'd later on and added manually
dum_stock <- data.frame(
  stock = c("WALKER_CREEK", "EEL_RIVER_FALL", 
            "ROB@GOLD", "SLIM", "R_CHEHALIS", "W_CHILLIWACK", "PORTAGE", 
            "R_CHILLIWACK", "EAGLE", "ELKIN", "NEVIN_CR", "RAFT", 
            "LITTLECAMPBELL", "STAMP", "GOLD(83-86)", "OKANAGAN_R", 
            "L_KALUM@AC", "SNAKE_S", "GREEN", "SKEENA@TERRACE",
            "ANDREW_CREEK",
            "BABINE_RIVER___SECTION_4",
            "BABINE_RIVER_SECTIONS_1_TO_3",
            "BAEZAEKO_RIVER",
            "CHILAKO_RIVER",
            "CHILCOTIN_RIVER",
            "DEAN_RIVER",
            "ELK_RIVER",
            "ELOCHOMAN",
            "JAMES_CREEK",
            "LEWIS_LAKE",
            "LITTLE_CAMPBELL_RIVER",
            "LITTLE_TRAPPER",
            "MACKENZIE",
            "MCCALL_RIVER",
            "NANAIMO_UNKNOWN",
            "NANGEESE_RIVER",
            "NEVIN_CREEK",
            "SKEENA_RIVER",
            "STAMP_RIVER",
            "TAHKENITCH_LAKE",
            "TATSAMENIE_LAKE_OUTLET",
            "TEIGEN_CREEK",
            "TRINITY_SPRING",
            "UPPER_SALMON_SNAKE_R",
            "VALLEY_CREEK",
            "WILLIPA_CREEK",
            "BANDON HATCHERY",
            "H-SAN JUAN R",        
            "HUMPTULIPS HATCHERY", 
            "METHOW R     48.0007",
            "PALMER HATCHERY",     
            "THORNTON CREEK H",
            "CLE_ELM_HATCH", "WALKER", "GRANITE", "MARSH_CR", "MINAM_CR", 
            "MCCALL", "CHEWUCH_SP", "MCCALL_HAT", "WENAHA", "RAPID_SP", 
            "SALMON_E_FORK", "SECECH_R", "BUTTE_SP", "UP_SALMON-SP",
            "UPPER_VALLEY", "WANNOCK", "DUDIDONTU_R", "MORICE", "COLONIAL_CAY",
            "SPRING_CREEK_H", "BIGQUL@LANG", "PUNTLED_SU", "LEMIEUX", 
            "VALLEY_CR", "TWISP_SP", "JOHN_DAY_MID", "CHILCOTIN", "BULKLEY", 
            "SACR_LF", "NANAIMO", "BIG", "JOHN_DAY_MAIN", "SACR_F", "RAPID SP",
            "ENTIAT", "CHIWAWA SP", "HANFORD REACH", "WENATCHEE SU", 
            "ABERNATHY F", "L. THOMPSON", "COLE RIVER", "MARSH CREEK",
            "CHEWUCH SP", "SALMON E.FORK", "MARSH CR", "MCCALL HAT",
            "TOQUART RIVER", "JOHN DAY MID", "NORTH SANTIAM", "CHILCOTIN_MIX",
            "AMERICAN_R", "KENNETH", "WILLOW", "TUCANNON_SP", "BULKLEY_SP", 
            "CLACKAMAS_NO", "CLEELMHATCH", "OSOYOOS_RESID", "EEL_F", 
            "ZYMOGOTITZ", "KASIKS", "TRASK_HAT_SP", "JOHN_DAY_NORTH", 
            "SQUINGULA", "NAHATLATCH_R" , "NEVIN", "GRIZZLY", "CLEARWA US",
            "KHUTZE RIVER", "AALTANHASH R" , "TENDERFOOT", "TAHKENITCH_L" , 
            "BABINE FENCE" , "WILLAPA", "BOOTH CR", "LEWIS", "COWLITZ",
            "ELOCHOMAN E", "BIGQUL@LANG"
            ),
  cu = NA
)
hake_stocks <- readRDS(here::here("data", "hake_missing_stocks.rds"))  %>% 
  rbind(., dum_stock) %>% 
  mutate(stock = toupper(stock))  %>% 
  mutate(trim_stock = str_remove_all(stock, paste(remove, collapse = "|")),
         new_stock = NA,
         Region1Name = NA) 

for (i in 1:nrow(hake_stocks)) {
  match <- stringdist::amatch(hake_stocks$trim_stock[i], stockKey1$stock, 
                              maxDist = 2)
  hake_stocks$new_stock[i] <- stockKey1$stock[match]
  hake_stocks$Region1Name[i] <- stockKey1$Region1Name[match]
}

# preliminary coarse corrections to rec data
hake_out <- hake_stocks %>% 
  mutate(
    region = NA,
    Region1Name = case_when(
      cu %in% c("EVI-fall", "EVIGStr-sum", "QP-fall") ~ "ECVI",
      grepl("LFR", cu) ~ "LWFR-F",
      cu == "SPS" ~ "S_Puget_Sound",
      grepl("UFR", cu) ~ "UPFR",
      cu == "LTh" ~ "UPFR",
      TRUE ~ Region1Name
    )
  ) %>% 
  select(stock, region, Region1Name) %>% 
  distinct()


# as above but with missing troll stocks 
new_troll <- read.csv(
  here::here("data", "missing_troll_stocks.csv")
) %>% 
  rename(troll_region = agg) %>% 
  mutate(trim_stock = str_remove_all(stock, paste(remove, collapse = "|")),
         new_stock = NA,
         Region1Name = NA) 

for (i in 1:nrow(new_troll)) {
  match <- stringdist::amatch(new_troll$trim_stock[i], stockKey1$stock, 
                              maxDist = 2)
  new_troll$new_stock[i] <- stockKey1$stock[match]
  new_troll$Region1Name[i] <- stockKey1$Region1Name[match]
}

new_troll_out <- new_troll %>% 
  mutate(
    region = NA,
    Region1Name = case_when(
      troll_region == "BB" ~ "SOMN",
      troll_region == "BCD" ~ "NOMN",
      troll_region == "COWA" ~ "Washington_Coast",
      troll_region == "Nahwitti" ~ "NEVI",
      troll_region == "Rivers" ~ "Central_Coast",
      troll_region == "SC+SFj" ~ "SOMN"
    )
  ) %>% 
  select(stock, region, Region1Name) %>% 
  distinct()


# as above but with SNP key 
# SNP stock key based on observed composition in WCVI tagging study
snp_key <- read.csv(
  here::here("data", "snpStockKey.csv")
) %>% 
  mutate(region = NA) %>% 
  select(stock = collection, region, Region1Name)


## COMBINE REC, TROLL, HIGH SEAS KEYS ------------------------------------------

key_rts <- stockKey1 %>% 
  # full_join(., rec_out1, by = c("stock", "Region1Name")) %>%
  distinct() %>% 
  filter(!is.na(Region2Name))

# Associate misspelled and unknown stocks with higher level regions
key1 <- key_rts %>% 
  select(stock, region, Region1Name) %>%
  rbind(., hake_out) %>% 
  rbind(., rec_out1) %>% 
  rbind(., new_troll_out) %>% 
  rbind(., snp_key) %>% 
  mutate(
    #add unknown stocks
    Region1Name = case_when(
      stock == "SOOKE HARBOUR SEAPEN" ~ "WCVI",
      stock == "SANDY COVE SEAPEN" ~ "Fraser_Fall",
      stock %in% c("BEAR", "BEAR_RIVER") ~ "Skeena Upper",
      stock %in% c("COWLITZ_HATCHERY_SPRING", "COWLITZ_H_SP") ~ 
        "L_Columbia_R_sp",
      grepl("COWLITZ", stock) ~ "L_Columbia_R_fa",
      Region1Name == "NASS" ~ "Nass",
      stock == "TEIGEN" ~ "Nass",
      grepl("KING_SALMON", stock) ~ "Alaska",
      grepl("SERP", stock) ~ "Fraser_Fall",
      grepl("ISHKHEENICKH", stock) ~ "Nass",
      stock == "SNOWBANK" ~ "Nass",
      grepl("CRANBERRY", stock) ~ "Nass",
      grepl("DAMDOCHAX", stock) ~ "Nass",
      grepl("PALLANT", stock) ~ "Haida_Gwaii",
      grepl("DEENA", stock) ~ "Haida_Gwaii",
      stock == "LEWIS_H_SP" ~ "L_Columbia_R_sp",
      stock == "MACKENZIE" ~ "Willamette_R",
      grepl("MOKELUMNE", stock) ~ "Central_Valley_fa",
      grepl("BONNEVILLE", stock) ~ "L_Columbia_R_fa",
      grepl("LEWIS", stock) ~ "L_Columbia_R_fa",
      stock %in% c("CLEARWATERRFA", "SALMON_R_F") ~ "Snake_R_fa",
      grepl("SPIUS", stock) ~ "Fraser_Spring_4.2",
      grepl("NECHAKO", stock) ~ "Fraser_Summer_5.2",
      grepl("CARIBOO", stock) ~ "Fraser_Summer_5.2",
      grepl("BAEZAEKO", stock) ~ "Fraser_Spring_5.2",
      grepl("CHILAKO", stock) ~ "Fraser_Spring_5.2",
      stock %in% c("ENDAKO", "NAZKO", "TASEKO", 
                   "WESTROAD", "HOLMES", "INDIANPOINT", "JAMES", "MCGREGOR",
                   "MORKILL") ~ "Fraser_Spring_5.2",
      grepl("BRENNER", stock) ~ "N_Puget_Sound",
      grepl("BONAPARTE", stock) ~ "Fraser_Spring_4.2",
      grepl("BLACKW", stock) ~ "Fraser_Spring_5.2",
      grepl("BAKER_", stock) ~ "Fraser_Spring_5.2",
      grepl("BRIDGE", stock) ~ "Fraser_Spring_5.2",
      grepl("GOAT", stock) ~ "Fraser_Spring_5.2",
      stock == "BLUE_CREEK_CA" ~ "Klamath_R",
      grepl("BLUE", stock) ~ "Fraser_Spring_5.2",
      grepl("WHITEHORSE", stock) ~ "N_Puget_Sound",
      grepl("HORSE", stock) ~ "Fraser_Spring_5.2",
      grepl("FINN", stock) ~ "Fraser_Spring_5.2",
      grepl("CHILCOT", stock) ~ "Fraser_Spring_5.2",
      grepl("BOWR", stock) ~ "Fraser_Spring_5.2",
      stock == "WALKER_CREEK" ~ "Fraser_Spring_5.2",
      stock == "WALKER" ~ "Fraser_Spring_5.2",
      grepl("HUPP", stock) ~ "S_Puget_Sound",
      grepl("ICY CR", stock) ~ "S_Puget_Sound",
      grepl("GLENWOOD", stock) ~ "N_Puget_Sound",
      grepl("GLENWOOD", stock) ~ "N_Puget_Sound",
      grepl("HURD", stock) ~ "N_Puget_Sound",
      grepl("KETA", stock) ~ "SSE_Alaska",
      grepl("COTTONW", stock) ~ "Fraser_Spring_5.2",
      grepl("KENNET", stock) ~ "Fraser_Spring_5.2",
      grepl("WILLOW", stock) ~ "Fraser_Spring_5.2",
      grepl("SLIM", stock) ~ "Fraser_Spring_5.2",
      grepl("DUTEAU", stock) ~ "Fraser_Spring_4.2",
      grepl("COLDW", stock) ~ "Fraser_Spring_4.2",
      grepl("BESSETTE", stock) ~ "Fraser_Spring_4.2",
      grepl("NICOLA", stock) ~ "Fraser_Spring_4.2",
      grepl("DEADM", stock) ~ "Fraser_Spring_4.2",
      grepl("MCKINLEY", stock) ~ "Fraser_Spring_5.2",
      grepl("HOLLIDAY", stock) ~ "Fraser_Spring_5.2",
      grepl("PITT_RIVER", stock) ~ "Fraser_Spring_5.2",
      grepl("GOAT", stock) ~ "Fraser_Spring_5.2",
      grepl("NEVI", stock) ~ "Fraser_Spring_5.2",
      grepl("PTARM", stock) ~ "Fraser_Spring_5.2",
      grepl("ENDAKO", stock) ~ "Fraser_Spring_5.2",
      grepl("SALMON_CR", stock) ~ "Fraser_Spring_5.2",
      grepl("BIRKENHEAD", stock) ~ "Fraser_Spring_5.2",
      grepl("SAMPSON", stock) ~ "Fraser_Spring_5.2",
      grepl("CHILK", stock) ~ "Fraser_Summer_5.2",
      grepl("TASEKO", stock) ~ "Fraser_Summer_5.2",
      grepl("GATES", stock) ~ "Fraser_Summer_5.2",
      grepl("SLOQ", stock) ~ "Fraser_Summer_5.2",
      grepl("STUART", stock) ~ "Fraser_Summer_5.2",
      grepl("CLEARWA", stock) ~ "Fraser_Summer_5.2",
      grepl("NECHAK", stock) ~ "Fraser_Summer_5.2",
      grepl("BARR", stock) ~ "Fraser_Summer_5.2",
      grepl("BIG_SILVER", stock) ~ "Fraser_Summer_5.2",
      grepl("AVOLA", stock) ~ "Fraser_Summer_5.2",
      grepl("FENNELL", stock) ~ "Fraser_Summer_5.2",
      grepl("N_THOM@", stock) ~ "Fraser_Summer_5.2",
      grepl("QUES", stock) ~ "Fraser_Summer_5.2",
      grepl("BIRCH_ISLAND", stock) ~ "Fraser_Summer_5.2",
      grepl("ELKI", stock) ~ "Fraser_Summer_5.2",
      grepl("KUZK", stock) ~ "Fraser_Summer_5.2",
      grepl("SHUSWAP", stock) ~ "Fraser_Summer_4.1",
      grepl("HARBOUR", stock) ~ "Fraser_Summer_4.1",
      grepl("MOMICH", stock) ~ "Fraser_Summer_4.1",
      grepl("SOUTH_TH", stock) ~ "Fraser_Summer_4.1",
      stock %in% c("CHILLIWACK_RIVER_SUMMER", "S-CHILLIWACK R") ~ 
        "Fraser_Summer_5.2",
      grepl("MARIA", stock) ~ "Fraser_Summer_4.1",
      grepl("LITTLE_R", stock) ~ "Fraser_Summer_4.1",
      grepl("NORRISH", stock) ~ "Fraser_Fall",
      grepl("SILVERDALE", stock) ~ "Fraser_Fall",
      grepl("POST", stock) ~ "Fraser_Fall",
      grepl("STAVE", stock) ~ "Fraser_Fall",
      grepl("ALOUETTE", stock) ~ "Fraser_Fall",
      grepl("BOOTH", stock) ~ "Fraser_Fall",
      grepl("WORTH", stock) ~ "Fraser_Fall",
      grepl("HICKS", stock) ~ "Fraser_Fall",
      grepl("CHILLIWACK", stock) ~ "Fraser_Fall",
      grepl("CHILQUA", stock) ~ "Fraser_Fall",
      grepl("HARRISON", stock) ~ "Fraser_Fall",
      grepl("PORTA", stock) ~ "Fraser_Summer_5.2",
      grepl("DUNGE", stock) ~ "Juan_de_Fuca",
      grepl("TLUPANA", stock) ~ "WCVI",
      grepl("KOOTOWIS", stock) ~ "WCVI",
      grepl("KENNEDY", stock) ~ "WCVI",
      grepl("KAOUK", stock) ~ "WCVI",
      grepl("ZEBALLOS", stock) ~ "WCVI",
      grepl("COLONIAL", stock) ~ "WCVI",
      grepl("BURMAN", stock) ~ "WCVI",
      grepl("RENFREW", stock) ~ "WCVI",
      grepl("CYPRE", stock) ~ "WCVI",
      grepl("SOOKE", stock) ~ "WCVI",
      grepl("MEGI", stock) ~ "WCVI",
      grepl("CONU", stock) ~ "WCVI",
      grepl("GOLD_R", stock) ~ "WCVI",
      grepl("GOLD R", stock) ~ "WCVI",
      stock %in% c("ROB@GOLD", "GOLD(83-86)") ~ "WCVI",
      grepl("S-MARBLE R", stock) ~ "WCVI",
      grepl("SARITA", stock) ~ "WCVI",
      grepl("NAHMINT", stock) ~ "WCVI",
      grepl("8702GOLD", stock) ~ "WCVI",
      grepl("TOQUART", stock) ~ "WCVI",
      grepl("ALBERNI", stock) ~ "WCVI",
      grepl("SUCWOA", stock) ~ "WCVI",
      grepl("BEDWELL", stock) ~ "WCVI",
      grepl("TAHSIS", stock) ~ "WCVI",
      grepl("ROBERT", stock) ~ "WCVI",
      grepl("THORN", stock) ~ "WCVI",
      grepl("OMEGA PACIFIC", stock) ~ "WCVI",
      grepl("TRANQ", stock) ~ "WCVI",
      grepl("SAN JUAN", stock) ~ "WCVI",
      grepl("SAN_JUAN", stock) ~ "WCVI",
      grepl("NITINAT", stock) ~ "WCVI",
      grepl("TENDER", stock) ~ "SOMN",
      grepl("PHILLIPS", stock) ~ "SOMN",
      grepl("CHEAK", stock) ~ "SOMN",
      grepl("DEVER", stock) ~ "SOMN",
      grepl("MAMQUAM", stock) ~ "SOMN",
      grepl("SQUAMISH", stock) ~ "SOMN",
      grepl("SHOVEL", stock) ~ "SOMN",
      grepl("ATNARK", stock) ~ "NOMN",
      grepl("SALOOM", stock) ~ "NOMN",
      grepl("NUSATS", stock) ~ "NOMN",
      grepl("SALLOOM", stock) ~ "NOMN",
      grepl("SALLOOM", stock) ~ "NOMN",
      stock == "TIGIL_RIVER" ~ "Russia",
      stock == "AVACHA" ~ "Russia",
      stock == "BOLSHAYA_RIVER" ~ "Russia",
      grepl("MARBLE_C", stock) ~ "Central_Valley_fa",
      grepl("SACR", stock) ~ "Central_Valley_fa",
      grepl("MAKAH", stock) ~ "Washington_Coast",
      grepl("NASELLE", stock) ~ "Washington_Coast",
      grepl("LONESOME", stock) ~ "Washington_Coast",
      grepl("HUMPTULIPS", stock) ~ "Washington_Coast",
      stock == "HOH_R" ~ "Washington_Coast",
      grepl("FORKS_CR", stock) ~ "Washington_Coast",
      grepl("FORKS CREEK", stock) ~ "Washington_Coast",
      grepl("CLEAR_C", stock) ~ "Central_Valley_fa",
      grepl("SPRING CR", stock) ~ "Mid_Columbia_R_tule",
      grepl("SPRING_CR", stock) ~ "Mid_Columbia_R_tule",
      grepl("CHICKA", stock) ~ "SSE_Alaska",
      grepl("MERCED", stock) ~ "Central_Valley_fa",
      grepl("ZYMOG", stock) ~ "Fraser_Fall",
      grepl("INCH CR", stock) ~ "Fraser_Fall",
      grepl("S-FIRST LK/GSVI", stock) ~ "ECVI",
      grepl("NATHAN_CR_VI", stock) ~ "ECVI",
      stock == "BIG" ~ "ECVI",
      grepl("NANAIMO", stock) ~ "ECVI",
      grepl("SORENSON", stock) ~ "ECVI",
      grepl("TRENT", stock) ~ "ECVI",
      grepl("GOLDSTREAM", stock) ~ "ECVI",
      grepl("WOSS", stock) ~ "ECVI",
      grepl("OYSTER", stock) ~ "ECVI",
      grepl("COLE RIVERS", stock) ~ "Rogue_R",
      grepl("COLUMBIA R UPRIVER S", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("ELOCHOMAN_R", stock) ~ "L_Columbia_R_fa",
      grepl("WASHOUGAL", stock) ~ "L_Columbia_R_fa",
      grepl("TANNER", stock) ~ "L_Columbia_R_fa",
      grepl("CARLTON ACCLIMATION", stock) ~ "U_Columbia_R_su/fa",
      grepl("DRYDEN POND", stock) ~ "U_Columbia_R_su/fa",
      grepl("S-BEDWELL", stock) ~ "WCVI",
      grepl("SAMISH", stock) ~ "N_Puget_Sound",
      grepl("SKYK", stock) ~ "N_Puget_Sound",
      grepl("SKOOKUM", stock) ~ "N_Puget_Sound",
      grepl("SKAGIT", stock) ~ "N_Puget_Sound",
      grepl("WELLS", stock) ~ "U_Columbia_R_su/fa",
      grepl("WENAT", stock) ~ "U_Columbia_R_su/fa",
      grepl("OKAN", stock) ~ "U_Columbia_R_su/fa",
      grepl("CHELA", stock) ~ "U_Columbia_R_su/fa",
      grepl("CLEAR CR", stock) ~ "S_Puget_Sound",
      stock == "IRRIGON HATCHERY" ~ "Snake_R_fa",
      stock %in% c("BUTTE_CREEK_FALL", "FEATHER_RIVER_FALL", 
                   "FEATHER R HATCHERY") ~ 
        "Central_Valley_fa",
      stock %in% c("SALMON_RIVER_JNST", "S-SALMON R/JNST") ~ "SOMN",
      stock %in% c("BUTTE_CR_SP", "BUTTE_CREEK", "FEATHER_RIVER_SPRING",
                   "FEATHER_SP") ~ 
        "Central_Valley_sp",
      stock == "SALMON_RIVER_CA_SPRING" ~ "California_Coast",
      grepl("SALMON_CAL", stock) ~ "California_Coast",
      grepl("SALMON_RIVER", stock) ~ "Fraser_Spring_5.2",
      grepl("FEATHER_F", stock) ~ "Central_Valley_fa",
      grepl("BUTTE_F", stock) ~ "Central_Valley_fa",
      grepl("WARM", stock) ~ "Central_Valley_sp",
      grepl("BING_CR", stock) ~ "L_Columbia_R_fa",
      grepl("BIG CR", stock) ~ "L_Columbia_R_fa",
      grepl("BIG_CR", stock) ~ "L_Columbia_R_fa",
      grepl("KLASKANINE", stock) ~ "L_Columbia_R_fa",
      grepl("TOUTLE", stock) ~ "L_Columbia_R_fa",
      grepl("COLUMBIA R -MID", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("JOHN DAY", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("JOHN_DAY", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("TUCANNON", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("KLICKITAT", stock) ~ "U_Columbia_R_su/fa",
      grepl("WILLAMETTE", stock) ~ "Willamette_R",
      grepl("SANTIAM", stock) ~ "Willamette_R",
      stock == "MCKENZIE HATCHERY" ~ "Willamette_R",
      grepl("CLACK", stock) ~ "Willamette_R",
      grepl("CHRISTINA", stock) ~ "Stikine",
      stock == "BIG_BOULDER_CR" ~ "NSE_Alaska_Chilkat_R",
      grepl("MARSH", stock) ~ "Snake_R_sp/su",
      grepl("OXBOW", stock) ~ "Snake_R_sp/su",
      stock == "SALMON_R_F" ~ "Snake_R_fa",
      grepl("GRANITE", stock) ~ "Snake_R_sp/su",
      stock %in% c("VALLEY_CREEK", "VALLEY_CR", "UPPER_VALLEY") ~ 
        "Snake_R_sp/su",
      stock %in% c("UPPER_SALMON_SR", "UPPER_SALMON_SNAKE_R", "UP_SALMON-SP", 
                   "SALMON E.FORK", "SALMON_E_FORK") ~ "Snake_R_sp/su",
      grepl("BUTTE", stock) ~ "Snake_R_sp/su",
      grepl("MCCALL", stock) ~ "Snake_R_sp/su",
      stock == "SNAKE_S" ~ "Snake_R_sp/su",
      grepl("FRENCHMAN-", stock) ~ "Snake_R_sp/su",
      grepl("CEDA", stock) ~ "C_Puget_Sound",
      stock %in% c("BEAR_CR_SUFA") ~ "C_Puget_Sound",
      stock %in% c("APPLEGATE_CR", "CHETCO_R") ~ "N_California/S_Oregon_Coast",
      stock == "AMERICAN_SP" ~ "Central_Valley_sp",
      grepl("NIMBUS", stock) ~ "Central_Valley_fa",
      grepl("AMERICAN", stock) ~ "Central_Valley_fa",
      stock == "ALSEA_R" ~ "Mid_Oregon_Coast",
      grepl("BANDON", stock) ~ "Mid_Oregon_Coast",
      grepl("SOLDUC", stock) ~ "Washington_Coast",
      stock %in% c("ASHLULM") ~ "SOMN",
      grepl("HOMATH", stock) ~ "SOMN",
      grepl("SNOQUAL", stock) ~ "N_Puget_Sound",
      grepl("SNOHOMISH", stock) ~ "N_Puget_Sound",
      grepl("LYO", stock) ~ "Snake_R_fa",
      grepl("WENAHA", stock) ~ "Snake_R_fa",
      grepl("NEZ_PERCET", stock) ~ "Snake_R_fa",
      grepl("GEORGE_ADAMS", stock) ~ "Hood_Canal",
      grepl("GEORGE ADAMS", stock) ~ "Hood_Canal",
      grepl("ADAMS", stock) ~ "Fraser_Summer_4.1",
      grepl("LUMMI", stock) ~ "N_Puget_Sound",
      grepl("JONES", stock) ~ "N_Puget_Sound",
      grepl("MARBLEMOUNT", stock) ~ "N_Puget_Sound",
      grepl("GRIZZLY", stock) ~ "N_Puget_Sound",
      grepl("SKAGIT", stock) ~ "N_Puget_Sound",
      grepl("U_SAUK", stock) ~ "N_Puget_Sound",
      grepl("MILLI", stock) ~ "Mid_Oregon_Coast",
      grepl("UMPQUA", stock) ~ "Mid_Oregon_Coast",
      stock %in% c("SALMON R HATCHERY", "SALMON R FISH CULTUR") ~
        "Mid_Oregon_Coast",
      grepl("BERNIE", stock) ~ "S_Puget_Sound",
      grepl("PALMER H", stock) ~ "S_Puget_Sound",
      grepl("CLARKS CRK", stock) ~ "S_Puget_Sound",
      grepl("BRENNER", stock) ~ "N_Puget_Sound",
      grepl("PUYALLUP", stock) ~ "S_Puget_Sound",
      grepl("GARRISON", stock) ~ "S_Puget_Sound",
      grepl("MINTER", stock) ~ "S_Puget_Sound",
      grepl("SOOS", stock) ~ "S_Puget_Sound",
      grepl("GREEN", stock) ~ "S_Puget_Sound",
      grepl("GISASA", stock) ~ "Alaska",
      grepl("NULATO", stock) ~ "Alaska",
      grepl("TUMWATER", stock) ~ "S_Puget_Sound",
      grepl("ISSAQ", stock) ~ "S_Puget_Sound",
      grepl("KENDALL CR", stock) ~ "S_Puget_Sound",
      grepl("VOIGHTS", stock) ~ "S_Puget_Sound",
      stock == "GORST CR REARING PND" ~ "S_Puget_Sound",
      grepl("GROVERS", stock) ~ "S_Puget_Sound",
      grepl("WILLAP", stock) ~ "Washington_Coast",
      grepl("WILLIP", stock) ~ "Washington_Coast",
      grepl("HOKO", stock) ~ "Washington_Coast",
      grepl("SKYKOMISH", stock) ~ "C_Puget_Sound",
      grepl("SKYKOMISH", stock) ~ "C_Puget_Sound",
      stock == "BUTE" ~ "SOMN",
      grepl("SHOVEL", stock) ~ "SOMN",
      grepl("CLE_ELM", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("CLE_ELUM", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("ENTI", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("TWISP", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("NACH", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("CHEWU", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("CHIW", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("COLUMBIA-SP", region) ~ "Mid_and_Upper_Columbia_R_sp",
      stock %in% c("METHOW_R_SP", "WENATCHEE_H_SP", "WENATCHEE_R_SP") ~ 
        "Mid_and_Upper_Columbia_R_sp",
      region == "MID COL-SP" ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("OKAN", stock) ~ "U_Columbia_R_su/fa",
      grepl("METHOW", stock) ~ "U_Columbia_R_su/fa",
      grepl("WENATCH", stock) ~ "U_Columbia_R_su/fa",
      grepl("SILMIL", stock) ~ "U_Columbia_R_su/fa",
      grepl("SIMIL", stock) ~ "U_Columbia_R_su/fa",
      grepl("WELLS_H", stock) ~ "U_Columbia_R_su/fa",
      grepl("JOSEPH", stock) ~ "U_Columbia_R_su/fa",
      grepl("OSOY", stock) ~ "U_Columbia_R_su/fa",
      grepl("DESC", stock) ~ "U_Columbia_R_su/fa",
      grepl("HANFORD", stock) ~ "U_Columbia_R_su/fa",
      grepl("UMAT", stock) ~ "U_Columbia_R_su/fa",
      grepl("MARION", stock) ~ "U_Columbia_R_su/fa",
      grepl("PRIEST", stock) ~ "U_Columbia_R_su/fa",
      stock == "L_WH_SAL_H_SF" ~ "U_Columbia_R_su/fa",
      grepl("UPPER COLUMBIA-SU", region) ~ "U_Columbia_R_su/fa",
      grepl("HOH_RI", stock) ~ "Washington_Coast",
      grepl("S_PRAIRIE", stock) ~ "S_Puget_Sound",
      grepl("CLE_ELM", stock) ~ "S_Puget_Sound",
      grepl("QUINA", stock) ~ "Washington_Coast",
      grepl("ABERN", stock) ~ "L_Columbia_R_fa",
      grepl("WHITE", stock) ~ "C_Puget_Sound",
      stock == "HOODSPORT HATCHERY" ~ "Hood_Canal",
      grepl("CHEAK", stock) ~ "SOMN",
      grepl("SOUTHGATE", stock) ~ "SOMN",
      grepl("CAPIL", stock) ~ "SOMN",
      grepl("PORTE", stock) ~ "SOMN",
      grepl("KLINAK", stock) ~ "SOMN",
      grepl("CAMP", stock) ~ "NEVI",
      grepl("NIMP", stock) ~ "NEVI",
      grepl("KEOGH", stock) ~ "NEVI",
      grepl("NANA", stock) ~ "ECVI",
      grepl("DISCOVERY PASSAGE SEAPENS", stock) ~ "ECVI",
      grepl("COWICH", stock) ~ "ECVI",
      grepl("QUATSE", stock) ~ "NEVI",
      grepl("WOSS", stock) ~ "NEVI",
      grepl("BIGQUL@LANG", stock) ~ "ECVI",
      grepl("QUAL", stock) ~ "ECVI",
      grepl("COLE", stock) ~ "N_California/S_Oregon_Coast",
      grepl("TSEAX", stock) ~ "Nass",
      grepl("SEASKINN", stock) ~ "Nass",
      grepl("KINCOLITH", stock) ~ "Nass",
      grepl("KITEEN", stock) ~ "Nass",
      grepl("KWIN", stock) ~ "Nass",
      grepl("MEZIADIN", stock) ~ "Nass",
      grepl("ISHKEENI", stock) ~ "Nass",
      grepl("SNOWBANK", stock) ~ "Nass",
      grepl("OWEEGEE", stock) ~ "Nass",
      grepl("KATEEN", stock) ~ "Nass",
      grepl("TANKEEAH", stock) ~ "NOMN",
      grepl("NEECH", stock) ~ "NOMN",
      grepl("QUAAL", stock) ~ "NOMN",
      grepl("KITLOPE", stock) ~ "NOMN",
      grepl("KILBE", stock) ~ "NOMN",
      grepl("KASIK", stock) ~ "NOMN",
      grepl("KILDALA", stock) ~ "NOMN",
      grepl("NEKITE", stock) ~ "Central_Coast",
      grepl("ASHLU_", stock) ~ "Central_Coast",
      grepl("WANN", stock) ~ "Central_Coast",
      grepl("CHUCKW", stock) ~ "Central_Coast",
      grepl("KITI", stock) ~ "NOMN",
      grepl("HIRS", stock) ~ "NOMN",
      grepl("DEAN", stock) ~ "Central_Coast",
      grepl("VERRETT", stock) ~ "Stikine",
      grepl("TAHLTAN", stock) ~ "Stikine",
      grepl("CRAIG", stock) ~ "Stikine",
      grepl("NAKINA", stock) ~ "Taku",
      grepl("DUDI", stock) ~ "Taku",
      Region1Name == "Take_R" ~ "Taku",
      grepl("ANDR", stock) ~ "Stikine",
      stock == "SKEENA_RIVER" ~ "Skeena Mid",
      grepl("ZYMOGOTITZ_R", stock) ~ "Skeena Mid",
      grepl("KITSUMKALUM", stock) ~ "Skeena Mid",
      grepl("KITWANGA", stock) ~ "Skeena Mid",
      grepl("SKEENA@TERRACE", stock) ~ "Skeena Mid",
      grepl("BABINE", stock) ~ "Skeena Mid",
      grepl("OTSI_CR", stock) ~ "Skeena Upper",
      grepl("SQUIN", stock) ~ "Skeena Upper",
      grepl("KLUATAN", stock) ~ "Skeena Upper",
      grepl("KITSEGUECLA", stock) ~ "Skeena Upper",
      grepl("SLAMGEESH", stock) ~ "Skeena Upper",
      grepl("KULDO", stock) ~ "Skeena Upper",
      grepl("KLUAYAZ", stock) ~ "Skeena Upper",
      grepl("SQUINGULA", stock) ~ "Skeena Upper",
      grepl("THOMAS_C", stock) ~ "Skeena Mid",
      grepl("SHEGUNIA", stock) ~ "Skeena Mid",
      grepl("KISPI", stock) ~ "Skeena Mid",
      grepl("SWEET", stock) ~ "Skeena Mid",
      grepl("NANGE", stock) ~ "Skeena Mid",
      grepl("BULK", stock) ~ "Skeena Bulkley",
      grepl("MORI", stock) ~ "Skeena Bulkley",
      grepl("FIDDLER", stock) ~ "Skeena Lower",
      grepl("L_KAL", stock) ~ "Skeena Lower",
      grepl("GITNAD", stock) ~ "Skeena Lower",
      grepl("KHYEX_R", stock) ~ "Skeena Lower",
      grepl("EXCHAM", stock) ~ "Skeena Lower",
      grepl("ZYMAGOT", stock) ~ "Skeena Lower",
      grepl("EXSTEW", stock) ~ "Skeena Lower",
      grepl("WALLACE", stock) ~ "C_Puget_Sound",
      grepl("LITTLEC", stock) ~ "N_Puget_Sound",
      grepl("STILLAG", stock) ~ "N_Puget_Sound",
      grepl("TYNE", stock) ~ "N_Puget_Sound",
      grepl("NICOMEKL", stock) ~ "Fraser_Fall",
      grepl("COOS", stock) ~ "Mid_Oregon_Coast",
      grepl("NOBLE CR", stock) ~ "Mid_Oregon_Coast",
      grepl("ELK", stock) ~ "Mid_Oregon_Coast",
      grepl("CHILL", stock) ~ "Fraser_Fall",
      grepl("BIG_Q", stock) ~ "ECVI",
      grepl("MINAM", stock) ~ "N_Oregon_Coast",
      grepl("WILSON_R", stock) ~ "N_Oregon_Coast",
      grepl("EUCH", stock) ~ "N_Oregon_Coast",
      grepl("SOL_DUC", stock) ~ "Washington_Coast",
      grepl("KILCHIS", stock) ~ "Washington_Coast",
      grepl("QUEE", stock) ~ "Washington_Coast",
      grepl("FORTSON", stock) ~ "N_Puget_Sound",
      grepl("NOOK", stock) ~ "N_Puget_Sound",
      grepl("SLOQUET", stock) ~ "Fraser_Summer_5.2",
      grepl("RAFT", stock) ~ "Fraser_Summer_5.2",
      grepl("PUNT", stock) ~ "ECVI",
      grepl("FANNY", stock) ~ "ECVI",
      grepl("QUIN", stock) ~ "NEVI",
      grepl("ROSEWALL", stock) ~ "ECVI",
      grepl("SHAK", stock) ~ "Stikine",
      grepl("LEMI", stock) ~ "Fraser_Summer_5.2",
      grepl("SIUS", stock) ~ "Mid_Oregon_Coast",
      grepl("TOUL", stock) ~ "Central_Valley_fa",
      grepl("TUOL", stock) ~ "Central_Valley_fa",
      grepl("STAN", stock) ~ "Central_Valley_fa",
      grepl("BATTLE", stock) ~ "Central_Valley_fa",
      stock == "MOK R FISH INS" ~ "Central_Valley_fa",
      grepl("TAKIA", stock) ~ "NOMN",
      grepl("ELWHA", stock) ~ "Juan_de_Fuca",
      stock == "LITTLE" ~ "Fraser_Summer_4.1",
      grepl("TRASK", stock) ~ "N_Oregon_Coast",
      stock %in% c("CHEHALIS_RIVER_SUMMER", 
                   "S-CHEHALIS R") ~ "Fraser_Summer_5.2",
      grepl("CHEHA", stock) ~ "Fraser_Fall",
      grepl("UPPER_PITT", stock) ~ "Fraser_Spring_5.2",
      grepl("EAGL", stock) ~ "Fraser_Spring_5.2",
      grepl("SEYMOUR", stock) ~ "Fraser_Spring_5.2",
      grepl("NEST", stock) ~ "N_Oregon_Coast",
      grepl("TAHKENITCH", stock) ~ "Mid_Oregon_Coast",
      grepl("MORGAN CR", stock) ~ "Mid_Oregon_Coast",
      stock == "RINGOLD SPRINGS HATCHERY" ~ "U_Columbia_R_su/fa",
      grepl("COWEE", stock) ~ "L_Columbia_R_fa",
      grepl("COQUILLE", stock) ~ "N_California/S_Oregon_Coast",
      grepl("LOBST", stock) ~ "N_California/S_Oregon_Coast",
      grepl("PIST", stock) ~ "N_California/S_Oregon_Coast",
      grepl("HUNTER", stock) ~ "N_California/S_Oregon_Coast",
      grepl("NORTH_THOMP", stock) ~ "Fraser_Summer_5.2",
      grepl("THOMP", stock) ~ "Fraser_Summer_4.1",
      grepl("SANDY", stock) ~ "L_Columbia_R_fa",
      grepl("FALLERT CR", stock) ~ "L_Columbia_R_fa",
      grepl("KALAMA", stock) ~ "L_Columbia_R_fa",
      stock == "NPT HATCHERY" ~ "U_Columbia_R_su/fa",
      grepl("YUBA", stock) ~ "Central_Valley_fa",
      grepl("NEHA", stock) ~ "N_Oregon_Coast",
      grepl("WINCHUK", stock) ~ "N_California/S_Oregon_Coast",
      grepl("CLEEL", stock) ~ "Mid_Oregon_Coast",
      grepl("TRAPP", stock) ~ "Taku",
      stock == "EEL_F" ~ "California_Coast",
      stock == "EEL_RIVER_FALL" ~ "California_Coast",
      stock == "RIVERS INLET SEAPEN" ~ "Central_Coast",
      grepl("KHUTZE", stock) ~ "NOMN",
      grepl("DRAKE", stock) ~ "NOMN",
      grepl("HUGH", stock) ~ "NOMN",
      grepl("EVELYN", stock) ~ "NOMN",
      grepl("DUNN", stock) ~ "NOMN",
      grepl("AALTANHASH", stock) ~ "NOMN",
      grepl("KILTUISH", stock) ~ "NOMN",
      grepl("ATN", stock) ~ "NOMN",
      grepl("NAHAT", stock) ~ "Fraser_Fall",
      grepl("TATSAMENIE", stock) ~ "Taku",
      grepl("TUY", stock) ~ "Stikine",
      grepl("DEWATTO", stock) ~ "Hood_Canal",
      grepl("QUILCENE", stock) ~ "Hood_Canal",
      grepl("HAMMA_", stock) ~ "Hood_Canal",
      grepl("SOTH", Region1Name) ~ "Fraser_Summer_4.1",
      grepl("LWFR-F", Region1Name) ~ "Fraser_Fall",
      grepl("LWFR-Sp", Region1Name) ~ "Fraser_Spring_5.2",
      grepl("LWTH", Region1Name) ~ "Fraser_Spring_4.2",
      Region1Name == "NOTH" ~ "Fraser_Summer_5.2",
      Region1Name == "UPFR" ~ "Fraser_Spring_5.2",
      grepl("STAMP", stock) ~ "WCVI",
      grepl("TOFINO", stock) ~ "WCVI",
      grepl("GORDON", stock) ~ "WCVI",
      grepl("LEINER", stock) ~ "WCVI",
      region == "WCVI" ~ "WCVI",
      region == "NASS" ~ "Nass",
      region == "SKEENA MID" ~ "Skeena Mid",
      region == "SKEENA LOWER" ~ "Skeena Lower",
      region == "Alaska" ~ "Alaska",
      grepl("Alaska", stock) ~ "Alaska",
      grepl("TRINITY", region) ~ "Klamath_R",
      grepl("TRINITY", stock) ~ "Klamath_R",
      grepl("SECECH", stock) ~ "Snake_R_sp/su",
      grepl("IMNAHA", stock) ~ "Snake_R_sp/su",
      grepl("RAPID", stock) ~ "Snake_R_sp/su",
      region == "SNAKE-SP/SU" ~ "Snake_R_sp/su",
      region == "UPPER WILLAMETTE" ~ "Willamette_R",
      region == "UP WILLAMETTE" ~ "Willamette_R",
      region == "CENTRAL VALLEY-F" ~ "Central_Valley_fa",
      region == "CENT VAL-F" ~ "Central_Valley_fa",
      region == "CENTRAL VALLEY-SP" ~ "Central_Valley_sp",
      TRUE ~ as.character(Region1Name)
    )
  ) %>%
  select(-region) %>% 
  distinct()

key1 %>% 
  filter(is.na(Region1Name))%>% 
  left_join(., new_rec %>% select(stock, sc_reg1), by = "stock")


key1 %>% 
  group_by(stock) %>% 
  filter(n()>1)  %>% 
  arrange(stock) %>% 
  select(stock, Region1Name)


# ADD CWT DATA -----------------------------------------------------------------

cwt <- readRDS(here::here("data", "cwt_stock_key_out.RDS")) %>% 
  mutate(stock = toupper(stock),
         gsi_stock = NA,
         Region1Name = NA) %>% 
  select(-release)

# first match automatically using amatch
for (i in 1:nrow(cwt)) {
  match <- stringdist::amatch(cwt$stock[i], key1$stock, maxDist = 8)
  cwt$gsi_stock[i] <- key1$stock[match]
  cwt$Region1Name[i] <- key1$Region1Name[match]
}

cwt_out <- cwt %>% 
  mutate(
    Region1Name = case_when(
      grepl("INCH CR", stock) ~ "Fraser_Fall",
      grepl("MOKEL", stock) ~ "Central_Valley_fa",
      grepl("WOSS", stock) ~ "NEVI",
      grepl("COLE RIVERS", stock) ~ "Rogue_R",
      grepl("COLUMBIA R UPRIVER S", stock) ~ "Mid_and_Upper_Columbia_R_sp",
      grepl("WASHOUGAL", stock) ~ "L_Columbia_R_fa",
      grepl("BONNE", stock) ~ "L_Columbia_R_fa",
      grepl("TANNER", stock) ~ "L_Columbia_R_fa",
      grepl("COWLITZ", stock) ~ "L_Columbia_R_fa",
      stock == "BIG CR HATCHERY" ~ "L_Columbia_R_fa",
      stock == "SANDY HATCHERY (SANDY R)" ~ "L_Columbia_R_fa",
      stock == "NISQUALLY R  11.0008" ~ "S_Puget_Sound",
      stock == "MINTER CR    15.0048" ~ "S_Puget_Sound",
      grepl("S-BEDWELL", stock) ~ "WCVI",
      grepl("SKYK", stock) ~ "N_Puget_Sound",
      grepl("SKAGIT", stock) ~ "N_Puget_Sound",
      grepl("WHITE", stock) ~ "C_Puget_Sound",
      state == "AK" ~ "Alaska",
      basin == "UPTR" ~ "Fraser_Summer_4.1",
      basin == "SKNA" ~ "Skeena Bulkley",
      basin %in% c("SWVI", "NWVI") ~ "WCVI",
      rmis_region %in% c("SAFA", "CECA") ~ "Central_Valley_fa",
      rmis_region == "KLTR" ~ "Klamath_R",
      basin == "CLEA" ~ "Snake_R_fa",
      stock == "LYONS FERRY HATCHERY" ~ "Snake_R_fa",
      basin %in% c("UPSN", "SALM", "SIYA") ~ "Snake_R_sp/su",
      basin %in% c("WECH", "MEOK") ~ "U_Columbia_R_su/fa",
      grepl("WELLS", stock) ~ "U_Columbia_R_su/fa",
      grepl("SHOVEL", stock) ~ "SOMN",
      grepl("WENAT", stock) ~ "U_Columbia_R_su/fa",
      grepl("OKAN", stock) ~ "U_Columbia_R_su/fa",
      grepl("CHELA", stock) ~ "U_Columbia_R_su/fa",
      basin %in% c("DESC", "UMAT", "HOO", "KLIC", "CRGNG", "WAGN") ~ 
        "U_Columbia_R_su/fa",
      grepl("YAKI", stock) ~ "U_Columbia_R_su/fa",
      rmis_region == "NOCA" ~ "N_California/S_Oregon_Coast",
      basin %in% c("WILL", "YOCL") ~ "Willamette_R",
      basin %in% c("SAND", "SAWA", "GREL") ~ "L_Columbia_R_fa",
      basin %in% c("TILN", "NEHA") ~ "N_Oregon_Coast",
      grepl("CLEAR CR", stock) ~ "S_Puget_Sound",
      basin == "UMPQ" ~ "Mid_Oregon_Coast",
      basin == "SIXE" ~ "N_California/S_Oregon_Coast",
      basin == "ROGU" ~ "Rogue_R",
      basin %in% c("LEWI", "WIND", "COWL") ~ "L_Columbia_R_sp",
      basin == "GHLC" ~ "Washington_Coast",
      rmis_region == "SNAK" ~ "Snake_R_sp/su",
      rmis_region %in% c("WILP", "NWC") ~ "Washington_Coast",
      rmis_region == "HOOD" ~ "Hood_Canal",
      rmis_region == "JUAN" ~ "Juan_de_Fuca",
      rmis_region == "MPS" ~ "C_Puget_Sound",
      rmis_region %in% c("NPS", "NOWA", "SKAG") ~ "N_Puget_Sound",
      rmis_region == "SPS" ~ "S_Puget_Sound",
      TRUE ~ Region1Name
    ),
    # snake run timing distinguished at basin level, adjust stock name 
    # accordingly
    stock = case_when(
      basin == "CLEA" ~ "SNAKE R FALL",
      TRUE ~ stock
    )
  ) %>% 
  distinct()

# join initial keys together
key2 <- rbind(key1, 
              cwt_out %>%
                select(stock, Region1Name)) %>% 
  mutate(Region1Name = gsub(" ", "_", Region1Name))


# ADD REGIONAL ROLL UPS --------------------------------------------------------

key_out <- key2 %>% 
  left_join(., 
            key_rts %>% 
              select(Region1Name, Region2Name, Region3Name) %>% 
              distinct(), 
            by = "Region1Name",
            relationship = "many-to-many") %>% 
  distinct() %>%
  mutate(
    Region2Name =
      case_when(
        Region1Name %in% c("Nass", "Skeena_Lower", "Skeena_Bulkley", 
                           "Skeena_Mid", "Skeena_Upper", "Skeena_Babine",  
                           "Alsek", "Stikine", "Central_Coast") ~ 
          "North/Central BC",
        grepl("Fraser_Spring", Region1Name) ~ "Fraser Early",
        grepl("Fraser_Summer", Region1Name) ~ "Fraser Early",
        grepl("Fraser_Fall", Region1Name) ~ "Fraser Late",
        Region1Name == "Juan_de_Fuca" ~ "Washington Coast/Juan de Fuca",
        Region1Name == "Hood_Canal" ~ "Hood Canal",
        Region1Name == "Central_Valley_fa" ~ "California",
        Region1Name == "Alaska" ~ "Alaska South SE",
        Region1Name == "L_Columbia_R_sp" ~ "Spring Cowlitz",
        Region1Name == "Snake_R_sp/su" ~ "Snake Sp-Su",
        Region1Name %in% c("U_Columbia_R_su/fa", "U_Columbia_R_su/fa") ~
          "Up-Columbia S-F",
        Region1Name == "Mid_and_Upper_Columbia_R_sp" ~
          "Up-Columbia_sp",
        Region1Name == "Mid_Columbia_R_tule" ~ 
          "Mid-Columbia Brights/Upriver Brights",
        grepl("Keogh", stock) ~ "North/Central BC",
        grepl("OMEGA", stock) ~ "West Coast Hatchery",
        stock %in% c("SKAGIT_SU", "SKYKOMISH_SU") ~ "Puget Sound Summer",
        stock == "NOOKSACK_SP@KE" ~ "Puget Sound Spring",
        Region1Name == "N_Puget_Sound" ~ "North Puget Sound Fall",
        Region1Name == "C_Puget_Sound" ~ "Central Puget Sound Fall",
        Region1Name == "S_Puget_Sound" ~ "South Puget Sound Fall",
        grepl("GOLD", stock) ~ "West Coast Hatchery",
        grepl("TOQUA", stock) ~ "West Coast Hatchery",
        stock %in% c("CONUMA", "ROBERTSON", "NITINAT", "THORNTON") ~
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
        Region1Name == "Taku" ~ "Alaska South SE",
        Region2Name == "Califormia" ~ "California",
        Region2Name == " Up-Columbia S-F" ~ "Up-Columbia S-F",
        Region1Name == "SOMN" ~ "Upper Strait of Georgia",
        Region1Name == "Haida_Gwaii" ~ "North/Central BC",
        Region1Name == "NEVI" ~ "North/Central BC",
        Region1Name == "NOMN" ~ "North/Central BC",
        Region1Name == "Yukon" ~ "Yukon",
        Region1Name == "Russia" ~ "Russia",
        TRUE ~ as.character(Region2Name)
      ),
    Region3Name = 
      case_when(
        grepl("Puget", Region1Name) ~ "Puget Sound",
        grepl("Oregon", Region1Name) ~ "Oregon/California",
        Region1Name == "Washington_Coast" ~ "Washington Coast",
        Region3Name == "Coastal Washington" ~ "Washington Coast",
        grepl("NIMP", stock) ~ "North/Central BC",
        Region1Name %in% c("NOMN", "NEVI") ~ "North/Central BC",
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
        Region1Name == "Russia" ~ "Russia",
        Region1Name == "Yukon" ~ "Yukon",
        TRUE ~ as.character(Region3Name)
      ),
    Region4Name =
      case_when(
        Region3Name %in% c("Washington Coast", "Oregon/California") ~ 
          "CoastUS",
        Region3Name %in% c("SOG", "Puget Sound", "Fraser River") ~ 
          "SalSea",
        Region3Name %in% c("Columbia", "Snake") ~ "ColR",
        Region3Name == "North/Central BC" ~ "NBC",
        Region3Name == "Alaska South SE" ~ "SEAK",
        Region1Name == "Russia" ~ "Russia",
        TRUE ~ Region3Name
      )
  ) %>% 
  distinct() %>% 
  arrange(Region4Name, Region3Name, Region2Name, Region1Name, stock) %>% 
  #add PST aggregate names
  mutate(pst_agg = case_when(
    Region3Name == "Oregon/California" ~ "CA_ORCST",
    Region3Name == "Washington Coast" ~ "WACST",
    Region3Name == "Puget Sound" ~ "PSD",
    Region3Name %in% c("Alaska South SE", "North/Central BC") ~ "NBC_SEAK",
    Region1Name == "Fraser_Fall" ~ "FR-late",
    Region3Name == "Fraser River" ~ "FR-early",
    Region1Name %in% c("L_Columbia_R_fa",
                       "Mid_Columbia_R_tule") ~ "CR-lower_fa",
    Region1Name == "L_Columbia_R_sp" ~ "CR-lower_sp",
    Region1Name %in% c("Snake_R_sp/su", "Mid_and_Upper_Columbia_R_sp", 
                       "Willamette_R") ~ "CR-upper_sp",
    Region1Name %in% c("U_Columbia_R_su/fa", "U_Columbia_R_su/fa", 
                       "Snake_R_fa") ~ "CR-upper_su/fa",
    TRUE ~ Region3Name
  ))

# checks
key_out %>%
  select(stock, Region1Name, Region3Name, pst_agg) %>%
  filter(is.na(pst_agg)) %>%
  distinct()

key_out %>% 
  group_by(stock) %>% 
  filter(n()>1)  %>% 
  arrange(stock) %>% 
  select(stock, Region1Name)

# save
saveRDS(key_out, here::here("data", "generated", "finalStockList_Nov2024.rds"))
write.csv(key_out, here::here("data", "generated", "finalStockList_Nov2024.csv"),
          row.names = FALSE)

