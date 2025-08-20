
# 1. Create Master date Data.Frame        
# From year 2000 till today 
begin <- "2000-01-15" %>% as.Date()
end <- Sys.Date()     #Sys.Date() - current system date
report_time_analysis <- paste0(begin, ":", end)

Date <- seq(from = begin, to = as.Date(end), by = "month")
master_df <- data.frame(date = Date) %>% 
  dplyr::mutate(., year = {year(date)}, month = {month(date)}) %>% 
  dplyr::select(., year, month)




# 2.1. Feed stuff prices         
# CORN, SORGHUM, WHEAT ----> LMIC: corn, sorghum and wheat prices $/cwt

## Weekly Cash Grain Prices
corn_pricessing_values <- readxl::read_xlsx("data/Corn_processing_Suppl_prices.xlsx", sheet = 1) %>% .[,-1] %>% 
  as.data.frame()

corn_to_st_flaked <- corn_pricessing_values[1,] %>% as.numeric()
sorg_to_st_flaked <- corn_pricessing_values[2,] %>% as.numeric()  
wht_to_st_rolled <- corn_pricessing_values[3,] %>% as.numeric() 

# corn and wheat prices are given in $/bu and converted into $/cwt; sorghum prices are given in $/cwt
wfile <- "https://lmic.info/wp-content//lmic-files/lmic-data/tac/spreadsheets/feedstuffs/Grainpr.xlsx" #file to download
xfile <- "./data/Grain_corn_sorg_wht_prices.xlsx" #what we want to call it once it has been downloaded
download.file(wfile, destfile = xfile, method = "libcurl", mode = "wb") #download the file

grain_lmic_prices_cr_srg_wht <- readxl::read_xlsx ("./data/Grain_corn_sorg_wht_prices.xlsx", sheet = 6, col_names = TRUE, skip = 8) %>% 
 dplyr::select(1, 2, 17, 19, 40) %>% 
  `colnames<-`(c("date", "monthly_average_wheat", "monthly_average_corn_1", 'monthly_average_corn_2', 'monthly_average_sorghum')) %>%
  dplyr::mutate(., year = year(date), month = month(date), 
                monthly_average_wheat = {monthly_average_wheat/0.60 %>% as.numeric() %>% round(., 2)},
                monthly_average_corn = {((monthly_average_corn_1+monthly_average_corn_2)/2)/0.56 %>% as.numeric() %>% round(., 2)},
                monthly_average_sorghum = {monthly_average_sorghum %>% as.numeric() %>% round(., 2)},
                monthly_corn_steam_flk = monthly_average_corn*corn_to_st_flaked,
                monthly_sorg_steam_flk = monthly_average_sorghum*sorg_to_st_flaked,
                monthly_wht_steam_rolled = monthly_average_wheat*wht_to_st_rolled) %>% 
  dplyr::select(., year, month, monthly_corn_steam_flk, monthly_sorg_steam_flk, monthly_wht_steam_rolled)


# CORN GLUTEN FEED WET (SWEET BRANS) ------> LMIC: Corn Gluten Feed Wet (50-60% Moisture) prices
## Weekly Dried Distillers Grains prices for various markets

# prices are given in $/ton and converted into $/cwt
wfile <- "https://lmic.info/wp-content//lmic-files/lmic-data/tac/spreadsheets/feedstuffs/WKDDGPrices.xlsx" #file to download
xfile <- "./data/Grain_sweet_bran.xlsx"
download.file(wfile, destfile = xfile, method = "libcurl", mode = "wb") # download the file

grain_lmic_prices_sweet_br <- readxl::read_xlsx ("./data/Grain_sweet_bran.xlsx", sheet=4, col_names = TRUE, skip = 5) %>% 
  dplyr::select(1, 18) %>%
  `colnames<-`(c("date", 'monthly_average_sweet_br')) %>%
  dplyr::mutate(., year = year(date), month = month(date), 
                monthly_average_sweet_br = {monthly_average_sweet_br/20 + 13.125 %>% as.numeric() %>% round(., 2)}) %>% 
  dplyr::select(., year, month, monthly_average_sweet_br)

### JOINING DATA
GRAINS <- left_join(master_df, grain_lmic_prices_cr_srg_wht, by = c("year", "month")) %>% relocate(., year, month) %>% 
  left_join(., grain_lmic_prices_sweet_br, by = c("year", "month"))
  




# 2.2. Protein Sources prices        
# UREA ----> WorldBank: Monthly prices (pink sheet)

# given in $/mt; converted into $/cwt
wfile <- "https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx" #file to download
xfile <- "./data/protein_urea_world_bank.xlsx"
download.file(wfile, destfile = xfile, method = "libcurl", mode = "wb") # download the file

protein_urea_world_bank <- readxl::read_excel("./data/protein_urea_world_bank.xlsx", sheet = "Monthly Prices") %>% 
  dplyr::select(1, 61) %>%
  `colnames<-`(c("date", 'monthly_average_urea')) %>%
  dplyr::mutate(., year = substr(date, 1,4), month = substr(date, 6,7),
                year = as.numeric(as.character(year)), month = as.numeric(as.character(month)),
                monthly_average_urea = {monthly_average_urea %>% as.numeric() %>% round(., 2)}/(22.0462)) %>% 
  dplyr::select(., year, month, monthly_average_urea)
 

# COTTONSEED MEAL 
# prices are given in $/ton and converted to $/cwt

# TILL 2022
protein_cottn_meal_till_2022 <- read_excel("./data/Protein_cottons_meal_before_2022.xlsx", sheet = 1, skip = 1) %>% 
  dplyr::select(1,2,7,8,9, 11) %>% 
  `colnames<-`(c("date", "location", "transmode", "low", "high", "delivery_period")) %>% 
  dplyr::mutate(year = year(date), month = month(date)) %>% 
  dplyr::filter(., location == "St. Louis, MO", delivery_period == "Cash") %>% 
  group_by(year, month) %>% 
  summarize(low = mean(as.numeric(as.character(low)), na.rm = TRUE), high = mean(as.numeric(as.character(high)), na.rm = TRUE)) %>% 
  dplyr::mutate(., monthly_average_cottn_meal = ((as.numeric(as.character(low))+ as.numeric(as.character(high)))/40)) %>% 
  dplyr::select(., year, month, monthly_average_cottn_meal)


# AFTER 2022
## AMS API excel -----> prices are given in $/ton and converted into $/cwt

protein_cottn_meal_after_2022_St_L <- read_excel("./data/Protein_cottons_meal_after_2022.xlsx", sheet = 1, skip = 1) %>% 
  dplyr::select(13,8,15, 40,42, 43) %>% 
  `colnames<-`(c("date", "commodity", "location", "avg_price", "FOB_delivered", "transmode")) %>% 
  dplyr::filter(., commodity == "Cottonseed Meal", location == "St Louis, MO") %>% 
  dplyr::mutate(date = as.Date(as.character(date),format = "%m/%d/%Y")) %>% 
  dplyr::mutate(., year = year(date), month = month(date),
                monthly_average_cottn_meal_St_L = avg_price/20) %>% 
  group_by(year, month) %>% 
  summarize(., monthly_average_cottn_meal_St_L = mean(monthly_average_cottn_meal_St_L, na.rm = TRUE))  %>% 
  dplyr::select(., year, month, monthly_average_cottn_meal_St_L)

protein_cottn_meal_after_2022_KC <- read_excel("./data/Protein_cottons_meal_after_2022.xlsx", sheet = 1, skip = 1) %>% 
  dplyr::select(13,8,15, 40,42, 43) %>% 
  `colnames<-`(c("date", "commodity", "location", "avg_price", "FOB_delivered", "transmode")) %>% 
  dplyr::filter(., commodity == "Cottonseed Meal", location == "KC Region") %>% 
  dplyr::mutate(date = as.Date(as.character(date),format = "%m/%d/%Y")) %>% 
  dplyr::mutate(., year = year(date), month = month(date),
                monthly_average_cottn_meal_K_C = avg_price/20) %>% 
  group_by(year, month) %>% 
  summarize(., monthly_average_cottn_meal_K_C = mean(monthly_average_cottn_meal_K_C, na.rm = TRUE))  %>% 
  dplyr::select(., year, month, monthly_average_cottn_meal_K_C)

protein_cottn_meal_after_2022 <- left_join(master_df, protein_cottn_meal_after_2022_St_L, by = c("year", "month")) %>% 
  left_join(., protein_cottn_meal_after_2022_KC,  by = c("year", "month")) %>% 
  dplyr::mutate(., monthly_average_cottn_meal = 
           ifelse(is.na(monthly_average_cottn_meal_St_L), monthly_average_cottn_meal_K_C, monthly_average_cottn_meal_St_L)) %>% 
  dplyr::select(., year, month, monthly_average_cottn_meal)

# combined monthly
protein_cottn_meal <- rbind(protein_cottn_meal_till_2022, protein_cottn_meal_after_2022) %>% 
  group_by(year, month) %>% 
  summarize(., monthly_average_cottn_meal = mean(monthly_average_cottn_meal, na.rm = TRUE))


# DDSG ----> LMIC, prices are given in $/ton and converted to $/cwt
protein_DDGS <- readxl::read_xlsx ("./data/Grain_sweet_bran.xlsx", sheet=4, col_names = TRUE, skip = 5) %>%
  dplyr::select(1, 2, 3, 4, 9) %>% 
  rename(., date = 1) %>% 
  dplyr::mutate(year = year(date), month = month(date), 
                monthly_average_DDGS = rowMeans(.[,-1], na.rm = TRUE),
                monthly_average_DDGS = monthly_average_DDGS/20 + 13.125) %>% 
  dplyr::select(., year, month, monthly_average_DDGS) 
  

### JOINING DATA
PROTEIN <- left_join(master_df, protein_urea_world_bank, by = c("year", "month")) %>% 
  left_join(., protein_cottn_meal,  by = c("year", "month")) %>% 
  left_join(., protein_DDGS, by = c("year", "month"))

PROTEIN$monthly_average_cottn_meal[-(1:tail(which(!is.na(PROTEIN$monthly_average_cottn_meal)),1))] <- 
  tail(PROTEIN$monthly_average_cottn_meal[!is.na(PROTEIN$monthly_average_cottn_meal)],1)

# Create a time series object
ts_obj <- ts(PROTEIN$monthly_average_cottn_meal, 
             start = c(year(begin), month(begin)),
             frequency = 12)

# Interpolate missing values
interpolated_values <- zoo::na.approx(ts_obj) %>% unlist()
PROTEIN$monthly_average_cottn_meal <- interpolated_values





# 2.3. Retrieving Roughage Sources prices        
# ALFALFA price are given in $/ton and converted to $/cwt
## Downloaded from USDA-NASS Database
roughage_nass_alfalfa <- usdarnass::nass_data(source_desc = "SURVEY", sector_desc = "CROPS", group_desc = "FIELD CROPS",
                                 commodity_desc = "HAY",statisticcat_desc = "PRICE RECEIVED",short_desc = "HAY, ALFALFA - PRICE RECEIVED, MEASURED IN $ / TON", 
                                 state_name = "TEXAS", year = "1990=<", freq_desc = "monthly") %>% 
  dplyr::select(.,  year = year, month = end_code, monthly_average_alfalfa = Value) %>% 
  dplyr::mutate(., year = {year %>% as.numeric()}, month = {month %>% as.numeric()},
                monthly_average_alfalfa = as.numeric(as.character(monthly_average_alfalfa))/20) %>% 
  dplyr::select(., year, month, monthly_average_alfalfa) 


# CORN SILAGE are given in $/bu and converted into $/cwt
roughage_corn_silage <- readxl::read_xlsx ("./data/Grain_corn_sorg_wht_prices.xlsx", sheet=6, col_names = TRUE, skip = 8) %>% 
  dplyr::select(1, 19) %>% 
  `colnames<-`(c("date", 'monthly_average_corn')) %>%
  dplyr::mutate(., year = year(date), month = month(date),
                monthly_corn_silage = {((((monthly_average_corn)*10))/20) %>% as.numeric() %>% round(., 2)}) %>% 
  dplyr::select(., year, month, monthly_corn_silage)


# CORNSTALKS ----> LMIC, prices are given in $/ton and converted to $/cwt
wfile <- "https://lmic.info/wp-content//lmic-files/lmic-data/tac/spreadsheets/feedstuffs/FEEDPR.xlsx" #file to download
xfile <- "./data/Roughage_cornstlks.xlsx"
download.file(wfile, destfile = xfile, method = "libcurl", mode = "wb") # download the file

protein_corn_stlks <- readxl::read_xlsx ("./data/Roughage_cornstlks.xlsx", sheet=2, col_names = TRUE, skip = 4) %>%
  dplyr::select(1, 6)  %>% 
  `colnames<-`(c("date", "monthly_average_other_hay")) %>% 
  dplyr::mutate(year = year(date), month = month(date),
                monthly_average_other_hay = monthly_average_other_hay/20) %>% 
  dplyr::select(., year, month, monthly_average_other_hay)
                
### JOINING DATA
ROUGHAGE <- left_join(master_df, roughage_nass_alfalfa, by = c("year", "month")) %>% 
  left_join(., roughage_corn_silage, by = c("year", "month")) %>% 
  left_join(., protein_corn_stlks, by = c("year", "month")) 





# 2.4. Retrieving Supplemental Fat prices        
## Tallow ----> LMIC
wfile <- "https://lmic.info/wp-content//lmic-files/lmic-data/tac/spreadsheets/meat/WklyTallowsProteins.xlsx" #file to download
xfile <- "./data/Supplm_fat_tallow_lmic.xlsx"
download.file(wfile,
              destfile = xfile,
              method = "libcurl", mode = "wb") # download the file

suppl_fat_tallow <- readxl::read_xlsx ("./data/Supplm_fat_tallow_lmic.xlsx", sheet=2, col_names = TRUE, skip = 6) %>%
  dplyr::select(1, 12)  %>% 
  `colnames<-`(c("date", "monthly_average_tallow")) %>% 
  dplyr::mutate(year = year(date), month = month(date)) %>% 
  dplyr::select(., year, month, monthly_average_tallow) %>% 
  group_by(., year, month)  %>% 
  summarize(., monthly_average_tallow = mean(monthly_average_tallow, na.rm = TRUE))


# YELLOW GREASE, prices are given in cents per pound and converted into to $/cwt
suppl_fat_yellow_gr_till_2022 <- read_excel("./data/Supplm_fat_yellow_before_2022.xlsx", sheet = 1, skip = 1) %>% 
  dplyr::select(1, 2, 6, 8, 9) %>%
  `colnames<-`(c("date", "location", "unit", "bid1", "bid2")) %>% 
  dplyr::filter(., location == "Kansas City, MO") %>% 
  dplyr::mutate(year = year(date), month = month(date), monthly_average_tallow = (bid1+bid2)/2) %>% 
  group_by(., year, month) %>% 
  summarize(., monthly_average_yellow_gr = mean(monthly_average_tallow, na.rm = TRUE)) %>% 
  dplyr::mutate(., monthly_average_yellow_gr = ifelse(monthly_average_yellow_gr < 1, monthly_average_yellow_gr*100, monthly_average_yellow_gr)) %>% 
  dplyr::select(., year, month, monthly_average_yellow_gr)

## after February 2022
suppl_fat_yellow_gr_after_2022 <- read_excel("./data/Supplm_fat_yellow_after_2022.xlsx", sheet = 1, skip = 1) %>% 
  dplyr::select(13, 8, 15, 33, 40) %>% 
  `colnames<-`(c("date", "commodity_type", "region", "unit", "avg_price")) %>% 
  dplyr::filter(., commodity_type == "Yellow Grease") %>% 
  dplyr::mutate(., year = substr(date, 7,10), month = substr(date, 1,2),
                year = as.numeric(as.character(year)), month = as.numeric(as.character(month))) %>% 
  group_by(., year, month) %>% 
  summarize(., monthly_average_yellow_gr = mean(avg_price, na.rm = TRUE)) %>% 
  dplyr::select(., year, month, monthly_average_yellow_gr)

suppl_fat_yellow_gr <- rbind(suppl_fat_yellow_gr_till_2022, suppl_fat_yellow_gr_after_2022) %>% 
  group_by(year, month) %>% 
  summarize(., monthly_average_yellow_gr = mean(monthly_average_yellow_gr))

### JOINING DATA
SUPPL_FAT <-  left_join(master_df, suppl_fat_tallow, by = c("year", "month")) %>% 
  left_join(., suppl_fat_yellow_gr, by = c("year", "month")) 

SUPPL_FAT$monthly_average_yellow_gr[-(1:tail(which(!is.na(SUPPL_FAT$monthly_average_yellow_gr)),1))] <- 
  tail(SUPPL_FAT$monthly_average_yellow_gr[!is.na(SUPPL_FAT$monthly_average_yellow_gr)],1)

SUPPL_FAT$monthly_average_tallow[-(1:tail(which(!is.na(SUPPL_FAT$monthly_average_tallow)),1))] <- 
  tail(SUPPL_FAT$monthly_average_tallow[!is.na(SUPPL_FAT$monthly_average_tallow)],1)

## interpolating missing prices
SUPPL_FAT$monthly_average_yellow_gr <- ts(SUPPL_FAT$monthly_average_yellow_gr, start = c(year(begin),month(begin)), 
                                          end = c(tail(SUPPL_FAT$year, 1),tail(SUPPL_FAT$month, 1)), frequency = 12) %>% zoo::na.approx() %>% unlist()

SUPPL_FAT$monthly_average_tallow <- ts(SUPPL_FAT$monthly_average_tallow, start = c(year(begin),month(begin)), 
                                       end = c(tail(SUPPL_FAT$year, 1),tail(SUPPL_FAT$month, 1)), frequency = 12) %>% zoo::na.approx() %>% unlist()





# 3. Kansas feedlot data        
wfile <- "https://lmic.info/wp-content//lmic-files/lmic-data/tac/spreadsheets/cattle/KSUFeedlot.xlsx" #file to download
xfile <- "./data/KSU_FEEDLOTS_data.xlsx" #what we want to call it once it has been downloaded
download.file(wfile, destfile = xfile, method = "libcurl", mode = "wb") #download the file

KSU_feedlots_data <- readxl::read_xlsx ("./data/KSU_FEEDLOTS_data.xlsx", sheet=2, col_names = TRUE, skip = 4) %>% 
  dplyr::select(1, 3, 4, 5, 6, 7, 8, 9, 10) %>% 
  `colnames<-`(c("marketed_date", "inweight_KSU", "outweight_KSU", "DOF_KSU", "ADG_KSU", "calc_INWT_KSU", "Feed_Gain_DMbasis_KSU", 
                 "Death_loss_KSU", "Averg_COG_cwt_KSU")) %>%
  dplyr::mutate(., marketed_date = marketed_date %m-% months(1), year = year(marketed_date), month = month(marketed_date), 
                purchased_date = marketed_date %m-% months(4),
                marketed_date = make_date(year, month, 15),
                DMI_intake_total_KSU = (outweight_KSU-inweight_KSU)*Feed_Gain_DMbasis_KSU,
                DMI_KSU = ADG_KSU * Feed_Gain_DMbasis_KSU) %>% 
  dplyr::select(., purchased_date, marketed_date, year, month, inweight_KSU, outweight_KSU, DOF_KSU, ADG_KSU, 
                calc_INWT_KSU, Feed_Gain_DMbasis_KSU, Death_loss_KSU, Averg_COG_cwt_KSU, DMI_intake_total_KSU, DMI_KSU)


# DMI/INWEIGHT ----> from lbs to kg
KSU_feedlots_data_DMI <- KSU_feedlots_data %>% dplyr::filter(marketed_date >= "2009-06-15") 
  
# DMI
KSU_feedlots_data_DMI_monthly <- KSU_feedlots_data_DMI %>% 
  group_by(month) %>% 
  summarize(., monthly_DMI = mean(DMI_KSU, na.rm = TRUE)) %>%
  dplyr::mutate(.,
  mean = mean(KSU_feedlots_data_DMI$DMI_KSU, na.rm = TRUE),
  diff = mean - monthly_DMI, percent = 1-(diff)/mean,
  check = mean * percent, DMI_simulator = 20.5,
  DMI_simulator_monthly = DMI_simulator*percent) %>%
  dplyr::select(., month, DMI_simulator_monthly)





# 4. Joining all prices and monthly DMI values together 

suppl_values <- readxl::read_xlsx("data/Corn_processing_Suppl_prices.xlsx", sheet = 2) %>% .[,-1] %>% 
  as.data.frame()

monthly_average_limestone <- suppl_values[1,1] %>% as.numeric()  #($/cwt)
monthly_average_suppl <- suppl_values[2,1] %>% as.numeric()   #($/ton)

all_prices <- left_join(master_df, GRAINS, by = c('year', 'month')) %>% 
  left_join(., PROTEIN, by = c('year', 'month')) %>% 
  left_join(., ROUGHAGE, by = c('year', 'month')) %>% 
  left_join(., SUPPL_FAT, by = c('year', 'month')) %>% 
  dplyr::mutate(., monthly_average_limestone = monthly_average_limestone, monthly_average_suppl = monthly_average_suppl) %>% 
  left_join(., KSU_feedlots_data_DMI_monthly, by = c("month")) %>% 
  dplyr::mutate(., DMI_simulator_monthly = DMI_simulator_monthly * 0.454)

directory_path <- "data"
saveRDS(all_prices, file = file.path(directory_path, "Prices.rds"))
