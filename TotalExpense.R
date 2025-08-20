
# 1. Feeder cattle: 750-800 lb. Ok City, $/ cwt
wfile <- "https://lmic.info/wp-content//lmic-files/lmic-data/tac/spreadsheets/cattle/AuctionsOklahomaCity.xlsx" #file to download
xfile <- "./data/Feeder_cattle_AuctionsOklahoma.xlsx" #what we want to call it once it has been downloaded
download.file(wfile,
              destfile = xfile,
              method = "libcurl", mode = "wb") #download the file

feeder_cattle_price <- readxl::read_xlsx ("./data/Feeder_cattle_AuctionsOklahoma.xlsx", sheet=6, col_names = TRUE, skip = 10) %>% 
  dplyr::select(1, 16) %>% 
  `colnames<-`(c("date", "feeder_cattle_price")) %>%
  dplyr::mutate(., year = year(date), month = month(date), feeder_cattle_price = {feeder_cattle_price %>% as.numeric() %>% round(., 2)}) %>% 
  dplyr::select(., year, month, feeder_cattle_price)



# 2. Interest rate, in %
wfile <- "https://www.dallasfed.org/-/media/Documents/research/surveys/AgSurvey/data/agrates.xlsx" #file to download
xfile <- "./data/Interest_rate_DallasFed.xlsx" #what we want to call it once it has been downloaded
download.file(wfile,
              destfile = xfile,
              method = "libcurl", mode = "wb") #download the file

interest_dallas_fed <- readxl::read_xlsx("./data/Interest_rate_DallasFed.xlsx", sheet = 1, skip = 3) %>%
  tidyr::separate(., Date, c("year", "quarter")) %>%
  dplyr::mutate(., month = {case_when(quarter == "Q1" ~ 1,
                                      quarter == "Q2" ~ 4,
                                      quarter == "Q3" ~ 7,
                                      quarter == "Q4" ~ 10,
                                      TRUE ~ NA_real_)},
                year = {year %>% as.numeric()}) %>%
  dplyr::rename(., feeder_interest_rate = `Feeder cattle`) %>%
  dplyr::select(., year, month, feeder_interest_rate) %>%
  left_join(master_df, ., by = c("year", "month")) %>% 
  dplyr::filter(., date >= "2000-01-01") %>%
  tidyr::fill(data = ., feeder_interest_rate) %>% 
  dplyr::select(., year, month, feeder_interest_rate) %>% 
  dplyr::mutate(., feeder_interest_rate = as.numeric(as.character(feeder_interest_rate))) %>%  na.omit()



# 3. Steer price
## marketed month (for calculations)
steer_price <- usdampr::mpr_request(slugIDs = "2686", report_time = report_time_analysis)$Detail %>% 
  tidyr::separate(., selling_basis_description, c("selling_type", "selling_transportation")) %>%
  dplyr::filter(., class_description == "STEER", grade_description == "Total all grades", selling_type == "LIVE", 
                selling_transportation == "FOB", report_date >= "2005-07-01") %>%
  dplyr::mutate(., date = {report_date %m-% months(5)}, year = {year(date)}, month = {month(date)}) %>%
  dplyr::select(., date, year, month, report_title, class_description, grade_description, selling_type, selling_transportation, head_count, weight_range_avg, weighted_avg_price) %>%
  dplyr::arrange(., year, month) %>% dplyr::mutate(., steer_price = weighted_avg_price) %>% 
  dplyr::select(., year, month, steer_price)

steer_price$steer_price <- ts(steer_price$steer_price, start = c(2005,1), frequency = 12) %>% zoo::na.approx() %>% 
  unlist() 

## actual month data
steer_price_actual <- usdampr::mpr_request(slugIDs = "2686", report_time = report_time_analysis)$Detail %>% 
  tidyr::separate(., selling_basis_description, c("selling_type", "selling_transportation")) %>%
  dplyr::filter(., class_description == "STEER", grade_description == "Total all grades", selling_type == "LIVE", selling_transportation == "FOB") %>%
  dplyr::mutate(., date = {report_date %m-% months(1)}, year = {year(date)}, month = {month(date)}) %>%
  dplyr::select(., date, year, month, report_title, class_description, grade_description, selling_type, selling_transportation, head_count, weight_range_avg, weighted_avg_price) %>%
  dplyr::arrange(., year, month) %>% dplyr::mutate(., steer_price_actual = weighted_avg_price) %>% 
  dplyr::select(., year, month, steer_price_actual)

# Total feed, handling, and management charge; 750 feeder steer price
# Interest on feeder and 1/2 feed; Death loss (1% of purchase)



# 4. Total expense calculation
directory_path <- "data"
animal_performance_ADG_DOF <- readRDS(file = file.path(directory_path, "Diet_Performance.rds"))

Total_expenses <- left_join(animal_performance_ADG_DOF, feeder_cattle_price, by = c("year", "month")) %>% 
  left_join(., interest_dallas_fed, by = c("year", "month")) %>% left_join(., steer_price, by = c("year", "month")) %>% 
  left_join(., steer_price_actual, by = c("year", "month")) %>% 
  dplyr::mutate(., Gain = fitted_outwt - inwt,
         
         date = as.Date(zoo::as.yearmon(paste(year, month), "%Y %m")),
         date_purchased = date,
         date_marketed = date_purchased %m+% months(4), year_marketed = year(date_marketed), month_marketed = month(date_marketed),
         year_purch = year(date_purchased), month_purch = month(date_purchased),
         
         # $/cwt of as-fed feed
         AS_FED_corn = (DM_Q_corn*100/(TABLEAU["DM", "corn_sf"]))/(lb_to_kg*100), AS_FED_sorg = (DM_Q_sorg*100/(TABLEAU["DM","sorghum_sf"]))/(lb_to_kg*100),
         AS_FED_wheat = (DM_Q_wheat*100/TABLEAU["DM", "wheat_sf"])/(lb_to_kg*100), AS_FED_sweet_br = (DM_Q_sweet_br*100/TABLEAU["DM","sweet_bran"])/(lb_to_kg*100),
         
         AS_FED_urea = (DM_Q_urea*100/TABLEAU["DM", "urea"])/(lb_to_kg*100), AS_FED_cottns_m = (DM_Q_cottn_m*100/TABLEAU["DM", "cottons_m"])/(lb_to_kg*100),
         AS_FED_DDGS = (DM_Q_DDGS*100/TABLEAU["DM","DDGS"])/(lb_to_kg*100),
         
         AS_FED_alf = (DM_Q_alfalfa*100/TABLEAU["DM","alfalfa"])/(lb_to_kg*100), AS_FED_corn_silage = (DM_Q_corn_silage*100/TABLEAU["DM", "silage_corn"])/(lb_to_kg*100),
         AS_FED_cornstalks = (DM_Q_cornstalks*100/TABLEAU["DM", "cornstalks"])/(lb_to_kg*100),
         
         AS_FED_tallow = (DM_Q_tallow*100/TABLEAU["DM", "tallow"])/(lb_to_kg*100), AS_FED_yellow_gr = (DM_Q_yellow_gr*100/TABLEAU["DM", "yellow_grease"])/(lb_to_kg*100),
         AS_FED_limest = (DM_Q_limestone*100/TABLEAU["DM", "limestone"])/(lb_to_kg*100), AS_FED_supplmt = (DM_Q_supplmt*100/TABLEAU["DM", "supplmt"])/(lb_to_kg*100),
         yardage = 0.50,
         
         Total_feed_DM_daily_kg = DM_Q_corn + DM_Q_sorg + DM_Q_wheat + DM_Q_sweet_br + DM_Q_urea + DM_Q_cottn_m + DM_Q_DDGS + 
           DM_Q_alfalfa + DM_Q_corn_silage + DM_Q_cornstalks + DM_Q_tallow + DM_Q_yellow_gr + DM_Q_limestone + DM_Q_supplmt,
         
         Total_feed_DM_total_lb = (DM_Q_corn + DM_Q_sorg + DM_Q_wheat + DM_Q_sweet_br + DM_Q_urea + DM_Q_cottn_m + DM_Q_DDGS + 
                                     DM_Q_alfalfa + DM_Q_corn_silage + DM_Q_cornstalks + DM_Q_tallow + DM_Q_yellow_gr + DM_Q_limestone + DM_Q_supplmt)/0.454,
         
         Total_feed_AS_FED_daily = AS_FED_corn + AS_FED_sorg + AS_FED_wheat + AS_FED_sweet_br + AS_FED_urea + AS_FED_cottns_m + AS_FED_DDGS + 
           AS_FED_alf + AS_FED_corn_silage + AS_FED_cornstalks + AS_FED_tallow + AS_FED_yellow_gr + AS_FED_limest + AS_FED_supplmt,
         
         Total_feed_AS_FED_total = Total_feed_AS_FED_daily * DOF,
         
         Total_feed_AS_FED_total_2 = ((Total_feed_DM_daily_kg * DOF)*100/DM_inlclusion)/(lb_to_kg*100),
         
         Feed_to_gain = Total_feed_DM_total_lb/ADG,
         
         # Total expense
         Feeder_cost = (feeder_cattle_price*7.5),
         Feed_cost = (monthly_corn_steam_flk*AS_FED_corn + monthly_sorg_steam_flk*AS_FED_sorg +
                        monthly_wht_steam_rolled*AS_FED_wheat + monthly_average_sweet_br*AS_FED_sweet_br +
                        
                        monthly_average_urea*AS_FED_urea + monthly_average_cottn_meal*AS_FED_cottns_m + monthly_average_DDGS*AS_FED_DDGS +
                        
                        monthly_average_alfalfa*AS_FED_alf + monthly_corn_silage*AS_FED_corn_silage + monthly_average_other_hay*AS_FED_cornstalks +
                        
                        monthly_average_tallow*AS_FED_tallow + monthly_average_yellow_gr*AS_FED_yellow_gr + 
                        
                        monthly_average_limestone*AS_FED_limest + monthly_average_suppl*AS_FED_supplmt) * DOF,
         
         Yardage_cost = yardage * DOF,
         
         Interest_on_feeder = (Feeder_cost * feeder_interest_rate/100)*DOF/365,
         Interest_on_feed = ((Feed_cost+Yardage_cost) * feeder_interest_rate * 0.5/100)*DOF/365,
         
         Death_loss = Feeder_cost * 0.013,
         Vet_cost = 20,
         Total_Expenses = Feeder_cost + Feed_cost + Yardage_cost + Interest_on_feeder + Interest_on_feed + Death_loss + Vet_cost,
         
         AFC = (DMI_simulator_monthly/0.454)/(ADG),
         
         # selling price required to cover total expense $/cwt
         Selling_price_covering_tot_expense = Total_Expenses/((Gain + inwt)*0.01),
         Net_margin_cwt = (steer_price - Selling_price_covering_tot_expense),
         Net_margin_head = (steer_price * 0.01 * fitted_outwt - Total_Expenses),
         
         
         # Cost of 1 cwt gain
         Feed_COG = Feed_cost/(Gain * 0.01),
         Total_COG = (Total_Expenses - Feeder_cost)/((Gain) * 0.01)) %>% 
  relocate(., date_purchased, year_purch, month_purch, date_marketed, year_marketed, month_marketed) %>% 
  dplyr::select(., -year, -month)

directory_path <- "data"
saveRDS(Total_expenses, file = file.path(directory_path, "TotalExpense.rds"))

