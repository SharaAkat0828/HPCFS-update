
# Create Master date Data.Frame
begin <- "2000-01-15" %>% as.Date()
end <- Sys.Date()  #Sys.Date() - current system date
report_time_analysis <- paste0(begin, ":", end)

Date <- seq(from = begin, to = as.Date(end), by = "month")
master_df <- data.frame(date = Date) %>% 
  dplyr::mutate(., year = {year(date)}, month = {month(date)}) %>% 
  dplyr::select(., year, month)


# OUTWT calculation 
# Corn prices     
corn_price <- readxl::read_xlsx ("./data/Grain_corn_sorg_wht_prices.xlsx", sheet=6, col_names = TRUE, skip = 8) %>% 
  dplyr::select(1, 2, 19, 40) %>% 
  `colnames<-`(c("date", "monthly_average_wheat", 'monthly_average_corn', 'monthly_average_sorghum')) %>%
  dplyr::mutate(., year = year(date), month = month(date), 
                corn_price = {monthly_average_corn/0.56 %>% as.numeric() %>% round(., 2)}) %>% 
  dplyr::select(., year, month, corn_price)

# Commercial cattle slaughtered (head)    
slaughter_head <- usdarnass::nass_data(source_desc = "SURVEY", sector_desc = "ANIMALS & PRODUCTS", group_desc = "LIVESTOCK",
                                       commodity_desc = "CATTLE",
                                       short_desc = "CATTLE, STEERS, SLAUGHTER, COMMERCIAL, FI - SLAUGHTERED, MEASURED IN HEAD", 
                                       agg_level_desc = "NATIONAL", year = "1990=<", freq_desc = "monthly") %>% 
  dplyr::select(.,  year = year, month = end_code, slaughter_head = Value) %>% 
  dplyr::mutate(., year = {year %>% as.numeric()}, month = {month %>% as.numeric()}, 
                slaughter_head = {as.numeric(gsub(",","",slaughter_head))})

# Beef production (lb)    
beef_prod_lb <- usdarnass::nass_data(source_desc = "SURVEY", sector_desc = "ANIMALS & PRODUCTS", group_desc = "LIVESTOCK",
                                     commodity_desc = "BEEF",
                                     short_desc = "BEEF, SLAUGHTER, COMMERCIAL, FI - PRODUCTION, MEASURED IN LB", 
                                     agg_level_desc = "NATIONAL", year = "1990=<", freq_desc = "monthly") %>% 
  dplyr::select(.,  year = year, month = end_code, beef_prod_lb = Value) %>% 
  dplyr::mutate(., year = {year %>% as.numeric()}, month = {month %>% as.numeric()}, 
                beef_prod_lb = {as.numeric(gsub(",","",beef_prod_lb))}) 

# USDA out weight   
OUTWT_USDA_quantity <- usdampr::mpr_request(slugIDs = "2477", report_time = report_time_analysis)$Detail %>% 
  tidyr::separate(., selling_basis_description, c("selling_type", "selling_transportation")) %>% 
  dplyr::filter(., class_description == "STEER", grade_description == "Total all grades", selling_type == "LIVE", selling_transportation == "FOB") %>%
  dplyr::mutate(., date = {report_date %m-% months(1)}, year = {year(date)}, month = {month(date)}) %>%
  dplyr::select(., date, year, month, report_title, class_description, grade_description, selling_type, selling_transportation, head_count, weight_range_avg, weighted_avg_price) %>%
  dplyr::arrange(., year, month) %>% dplyr::mutate(., steer_live_OTWT = weight_range_avg) %>% 
  dplyr::select(., year, month, steer_live_OTWT) %>% 
  group_by(year, month) %>% 
  summarize(., steer_live_OTWT = mean(steer_live_OTWT))

# USDA cattle prices
OUTWT_USDA_price <- usdampr::mpr_request(slugIDs = "2477", report_time = report_time_analysis)$Detail %>% 
  tidyr::separate(., selling_basis_description, c("selling_type", "selling_transportation")) %>% 
  dplyr::filter(., class_description == "STEER", grade_description == "Total all grades", selling_type == "LIVE", selling_transportation == "FOB") %>%
  dplyr::mutate(., date = {report_date %m-% months(1)}, year = {year(date)}, month = {month(date)}) %>%
  dplyr::select(., date, year, month, report_title, class_description, grade_description, selling_type, selling_transportation, head_count, weight_range_avg, weighted_avg_price) %>%
  dplyr::arrange(., year, month) %>% dplyr::mutate(., steer_live_OTWT_price = weighted_avg_price) %>% 
  dplyr::select(., year, month, steer_live_OTWT_price) %>% 
  group_by(year, month) %>% 
  summarize(., steer_live_OTWT_price = mean(steer_live_OTWT_price))

# Combing all data    
combined_data <- master_df %>% 
  left_join(., OUTWT_USDA_quantity, by = c("year", "month")) %>% 
  left_join(., OUTWT_USDA_price, by = c("year", "month")) %>% 
  left_join(., corn_price, by = c("year", "month")) %>% 
  dplyr::mutate(., lagged_corn = dplyr::lag(corn_price, 1), 
         cos_1 = cos((2*pi*month)/1),
         sin_1 = sin((2*pi*month)/1),
         cos_2 = cos((2*pi*month)/(1*2)),
         sin_2 = sin((2*pi*month)/(1*2)),
         cos_3 = cos((2*pi*month)/(1*3)),
         sin_3 = sin((2*pi*month)/(1*3)),
         cos_4 = cos((2*pi*month)/(1*4)),
         sin_4 = sin((2*pi*month)/(1*4)),
         trend_yearly = year - 1999,
         lagged_outwt = dplyr::lag(steer_live_OTWT)) 

# Regression   
reg <- feols(steer_live_OTWT ~ I(trend_yearly) + I(steer_live_OTWT_price/lagged_corn) +
               lagged_outwt + cos_1 + sin_1 + cos_2 + sin_2 + 
               cos_3 + sin_3 + cos_4 + sin_4, 
             # vcov = "hetero",
             cluster = ~ year,
             data = combined_data %>%  na.omit())

outwt_data_na_omit <- combined_data %>% na.omit()
outwt_data_predicted <- cbind(outwt_data_na_omit, fitted_outwt = reg$fitted.values) %>% 
  dplyr::select(., year, month, fitted_outwt, steer_live_OTWT)

outwt_data_predicted <- outwt_data_predicted %>% 
  dplyr::mutate(., marketed_date = make_date(year, month, 15),
         purchased_date = {marketed_date %m-% months(4)},
         year_purch = {year(purchased_date)}, 
         month_purch = {month(purchased_date)}) %>% 
  na.omit()

# OUTWT graph
graph_OUTWT <- ggplot(data = outwt_data_predicted) +
  geom_line(aes(x = marketed_date, y = steer_live_OTWT, color = "OUTWT_actual")) +
  geom_line(aes(x = marketed_date, y = fitted_outwt, color = "OUTWT_fitted")) +
  scale_color_manual("", 
                     breaks = c("OUTWT_actual", "OUTWT_fitted"),
                     values = c("red", "blue")) +
  xlab("Year") + theme_bw() + labs(title = "") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(1200, max(outwt_data_predicted$fitted_outwt)+200, 25)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        axis.text.y = element_text(size = 6)) +
  geom_hline(yintercept = mean(outwt_data_predicted$steer_live_OTWT, na.rm = TRUE), col = "red", linetype='dashed') +
  geom_hline(yintercept = mean(outwt_data_predicted$fitted_outwt, na.rm = TRUE), col = "blue")

outwt_data_predicted <- outwt_data_predicted %>% 
  dplyr::select(., year_purch, month_purch, fitted_outwt, steer_live_OTWT)



# Combining prices and OUTWT data    
directory_path <- "data"
all_prices <- readRDS(file = file.path(directory_path, "Prices.rds")) 

all_prices_and_outwt <- left_join(master_df, all_prices, by = c("year", "month")) %>% 
  left_join(., outwt_data_predicted, by = c("year" = "year_purch", "month" = "month_purch")) %>% na.omit()

directory_path <- "data"
saveRDS(all_prices_and_outwt, file = file.path(directory_path, "Prices_and_OUTWT.rds"))

