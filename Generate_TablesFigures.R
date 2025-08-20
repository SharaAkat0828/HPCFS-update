
#***********************************************************************************************************************************************************************************************************
# 1. Loading all feedlot data and joining them into 1
#***********************************************************************************************************************************************************************************************************

# 1.1. Load Old HPCFS Simulator data 
old_HPCFS_data <- read_excel("./data/High Plains Cattle Feeding Simulator.xls", sheet = 2, skip = 6) %>% 
  .[, c(1, 2, 6, 7, 8, 10, 14, 15, 18, 42, 39, 38)] %>% 
  `colnames<-`(c("Purchased_date","Marketed_date", "Total_handling_feed_old","NON_feed_cost_old", "Death_loss_old", "Total_expense_old", 
                 "Steer_35_65_price", "Net_margin_old_cwt", "Feed_costs_old", "ADG_old", "DOF_old", "Outweight_old")) %>% 
  dplyr::mutate(year_purchased = year(Purchased_date), month_purchased = month(Purchased_date)) %>% 
  dplyr::select(., year_purchased, month_purchased, Purchased_date, Marketed_date, 
         Total_handling_feed_old, NON_feed_cost_old, Death_loss_old, Total_expense_old, Steer_35_65_price,  
         Net_margin_old_cwt, Feed_costs_old, ADG_old, DOF_old, Outweight_old) %>% 
  dplyr::mutate(., Steer_35_65_price = as.numeric(as.character(Steer_35_65_price)),
         Net_margin_old_cwt = as.numeric(as.character(Net_margin_old_cwt)),
         Total_expense_old = as.numeric(as.character(Total_expense_old)),
         NON_feed_cost_old = as.numeric(as.character(NON_feed_cost_old)),
         Death_loss_old = as.numeric(as.character(Death_loss_old)),
         Outweight_old = as.numeric(as.character(Outweight_old)))



# 1.2. Load Iowa State feedlots' data
wfile <- "https://www2.econ.iastate.edu/estimated-returns/Finishing%20Yearling%20Steers%20Excel.xlsx" #file to download
xfile <- "./data/IOWA_FEEDLOTS_data.xlsx" #what we want to call it once it has been downloaded
download.file(wfile,
              destfile = xfile,
              method = "libcurl", mode = "wb") #download the file

IOWA_feedlots_data <- read_excel ("./data/IOWA_FEEDLOTS_data.xlsx", sheet=1, col_names = TRUE, skip = 2) %>% t() %>% as.data.frame() %>% 
  .[-1,]


# Master date date frame 
begin1 <- "2001-08-01" %>% as.Date()
end1 <- Sys.Date()
report_time_analysis1 <- paste0(begin1, ":", end1)
Date1 <- seq(from = begin1, to = as.Date(end1), by = "month")

master_df_2 <- data.frame(date_purchased = Date1) %>% 
  dplyr::mutate(., year = {year(date_purchased)}, month = {month(date_purchased)},
                date_marketed = {Date1 %m+% months(5)},
                year_m = year(date_marketed), month_m = month(date_marketed)) 

master_df_2 <- master_df_2[1:nrow(IOWA_feedlots_data),]

IOWA_feedlots_data <- IOWA_feedlots_data %>% 
  dplyr::mutate(., Purchase_date = master_df_2[,"date_purchased"], Marketed_date = master_df_2[,"date_marketed"]) %>% 
  dplyr::select(5, 14, 19, 22, 23, 25, 29, 30, 31) %>% 
  `colnames<-`(c("Feeder_price_per_cwt_IOWA", "Feed_cost_hd_IOWA", "NonFeed_cost_hd_IOWA", 
                 "Death_loss_hd_IOWA", "Total_cost_hd_IOWA", "Selling_price_cwt_IOWA",
                 "Net_margin_hd_IOWA", "Purchase_date_IOWA", "Marketed_date_IOWA")) %>% 
  dplyr::mutate(., year = {year(Marketed_date_IOWA)}, month = {month(Marketed_date_IOWA)}) %>% 
  relocate(., Purchase_date_IOWA, Marketed_date_IOWA, year, month)




# 1.3. Load Kansas State feedlots' data
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

# Kansas feedlots NE per head data 
KSU_Net_Margin_data <- readxl::read_xlsx ("./data/KSfeedlotSteersValues.xlsx",col_names = TRUE, skip = 2) %>% 
  `colnames<-`(c("date", "KSU_NE_per_head")) %>% 
  dplyr::mutate(., year = year(date), month = month(date)) %>% 
  dplyr::select(., year, month, KSU_NE_per_head)



# Comparing and Joining all feedlots data
directory_path <- "data"
Total_expenses <- readRDS(file = file.path(directory_path, "TotalExpense.rds")) 

ok <- sapply(Total_expenses, is.numeric)
Total_expenses <- replace(Total_expenses, ok, round(Total_expenses[ok], 2))

COMBINED_data <- left_join(Total_expenses, KSU_feedlots_data, 
                           by = c("year_marketed" = "year", 
                                  "month_marketed" = "month")) %>%
  left_join(., IOWA_feedlots_data, by = c("year_marketed" = "year","month_marketed" = "month")) %>%
  left_join(., old_HPCFS_data, by = c("year_purch" = "year_purchased", "month_purch" = "month_purchased")) %>%
  left_join(., KSU_Net_Margin_data, by = c("year_marketed" = "year", "month_marketed" = "month"))

#***********************************************************************************************************************************************************************************************************
# 2. Generating graphs 
#***********************************************************************************************************************************************************************************************************

# Historical Total expenses
min <- as.Date("2010-4-15")
max <- as.Date(max(COMBINED_data$date_marketed))

Total_expense <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = Total_Expenses, color = "Simulator data", linetype = "Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Simulator data"),
                     values = c("blue")) +
  scale_linetype_manual("",
                        breaks = c("Simulator data"),
                        values = c("solid")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Dollar per head") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min, max), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(1100, max(COMBINED_data$Total_Expenses, na.rm = TRUE)+50, 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position =  "none",
        text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  geom_hline(yintercept = mean(COMBINED_data$Total_Expenses, na.rm = TRUE), col = "blue", linetype='dashed') +
  ggtitle("Total Expense") +
  xlab("Closeout month")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Historical Average Daily Gain (ADG)
ADG <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_purchased, y = ADG, color = "Simulator data", linetype = "Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Simulator data"),
                     values = c("blue")) +
  scale_linetype_manual("", 
                        breaks = c("Simulator data"),
                        values = c("solid"))  +
  xlab("") + theme_bw() + labs(title = "") + ylab("Pounds per day") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min, max), expand=c(0,0)) +
  theme_classic() +
  scale_y_continuous(breaks = seq(2, max(COMBINED_data$ADG, na.rm = TRUE)+0.5, 0.05)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none",
        text = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  geom_hline(yintercept = mean(COMBINED_data$ADG, na.rm = TRUE), col = "blue", linetype='dashed') +
  ggtitle("Average Daily Gain") +
  xlab("Closeout month")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Historical Days on feed (DOF)
DOF <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_purchased, y = DOF, color = "Simulator data", linetype = "Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Simulator data"),
                     values = c("blue")) +
  scale_linetype_manual("", 
                        breaks = c("Simulator data"),
                        values = c("solid")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Days") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min, max), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(130, max(COMBINED_data$DOF, na.rm = TRUE)+10, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none",
        text = element_text(size = 12)) +
  geom_hline(yintercept = mean(COMBINED_data$DOF, na.rm = TRUE), col = "blue", linetype = 'dashed') +
  ggtitle("Days on Feed") +
  xlab("Closeout month")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Out weight (finishing weight) of all feedlots 
Outweight <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = 1300, 
                color = "Iowa Feedlot data", 
                linetype = "Iowa Feedlot data"), 
            lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = Outweight_old, 
                color = "HPCFS", 
                linetype = "HPCFS"), 
            lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = as.numeric(as.character(outweight_KSU)), 
                color = "KSU Feedlot data", 
                linetype = "KSU Feedlot data"), 
            lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = fitted_outwt, 
                color = "Simulator data", 
                linetype = "Simulator data"), 
            lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = steer_live_OTWT_actualUSDA, 
                color = "USDA data (actual)", 
                linetype = "USDA data (actual)"), 
            lwd = 0.8) +
  scale_color_manual("Legend", 
                     breaks = c("Iowa Feedlot data", "HPCFS", "KSU Feedlot data", "Simulator data", "USDA data (actual)"),
                     values = c("red", "purple", "green", "blue", "black")) +
  scale_linetype_manual("Legend", 
                        breaks = c("Iowa Feedlot data", "HPCFS", "KSU Feedlot data", "Simulator data", "USDA data (actual)"),
                        values = c("dashed", "dotdash", "dotted", "solid", "dotdash")) +
  xlab("Closeout month") + 
  theme_bw() + 
  labs( y = "Pounds") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", 
               date_labels = "%b %Y", 
               limits = c(min, max), 
               expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(1100, max(COMBINED_data$fitted_outwt, na.rm = TRUE) + 50, 25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.9), # Places legend inside the plot (80% right, 20% up)
        text = element_text(size = 15),
        legend.text = element_text(size = 10))

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Historical New margin per hundredweight (cwt) 
Net_margin <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = Net_margin_cwt, color = "Simulator data", linetype = "Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Simulator data"),
                     values = c("blue")) +
  scale_linetype_manual("", 
                        breaks = c("Simulator data"),
                        values = c("solid")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Dollar per hundredweight (cwt)") + theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min, max), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none",
        text = element_text(size = 15),
        legend.text = element_text(size = 10)) +
  geom_hline(yintercept = mean(COMBINED_data$Net_margin_cwt, na.rm = TRUE), col = "blue", linetype='dashed') +
  ggtitle("Net Margin") +
  xlab("Closeout month")


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Historical Cost of gain (COG)
COG <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = Total_COG, color = "Simulator data", linetype = "Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Simulator data"),
                     values = c("blue")) +
  scale_linetype_manual("", 
                        breaks = c("Simulator data"),
                        values = c("solid")) +
  ylab("Dollar per hundredweight (cwt) gain") + theme_bw() + labs(title = "") + xlab("") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min, max), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(65, max(COMBINED_data$Total_COG, na.rm = TRUE)+10, 5), limits = c(65, 150)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position =  "none",
        text = element_text(size = 15),
        legend.text = element_text(size = 14)) +
  geom_hline(yintercept = mean(COMBINED_data$Total_COG, na.rm = TRUE), col = "blue", linetype='dashed') +
  ggtitle("Cost of Gain") +
  xlab("Closeout month")


#******************************************************************************************************************************************************************************************
# 3. Generating out_figures
#******************************************************************************************************************************************************************************************

# Out_figures
ggsave(gridExtra::grid.arrange(Total_expense, Outweight,  nrow = 2), filename = "out_figures/p1.pdf", width = 10, height = 9.02)
ggsave(gridExtra::grid.arrange(ADG, DOF,  nrow = 2), filename = "out_figures/p2.pdf", width = 10, height = 9.02)
ggsave(gridExtra::grid.arrange(Net_margin, COG,  nrow = 2), filename = "out_figures/p3.pdf", width = 10, height = 9.02)

qpdf::pdf_combine(input = c("out_figures/p1.pdf", "out_figures/p2.pdf", "out_figures/p3.pdf"),
                  output = "out_figures/Simulator_graphs.pdf")
file.remove("out_figures/p1.pdf", "out_figures/p2.pdf", "out_figures/p3.pdf")

#******************************************************************************************************************************************************************************************
# 4. Generating out_data
#******************************************************************************************************************************************************************************************

# Out_table (historical)
## This block generates simulator historical data
Expenses_table_historical <- COMBINED_data %>%
  dplyr::mutate(., Marketing = "F.O.B.", total_handling_cost = Feed_cost+Yardage_cost+Vet_cost,
         total_interest_cost = Interest_on_feeder + Interest_on_feed) %>%
  dplyr::mutate(., Purchased_date = format(date_purchased, "%h-%Y"), Marketed_date = format(date_marketed, "%h-%Y")) %>%
  dplyr:: select (., Purchased_date, Marketed_date, Feeder_cost, total_handling_cost, 
                  total_interest_cost, Death_loss, Marketing, Total_Expenses, 
                  steer_price, Selling_price_covering_tot_expense, Net_margin_cwt, Net_margin_head,
                  Feed_COG, Total_COG, feeder_cattle_price, monthly_sorg_steam_flk, monthly_corn_steam_flk, monthly_wht_steam_rolled,
                  monthly_average_sweet_br, monthly_average_urea, monthly_average_cottn_meal, monthly_average_DDGS,
                  monthly_average_alfalfa, monthly_corn_silage, monthly_average_other_hay,
                  monthly_average_tallow, monthly_average_yellow_gr, monthly_average_limestone, monthly_average_suppl, feeder_interest_rate)

Expenses_table_historical_transposed <- t(Expenses_table_historical) %>% as.data.frame() %>% 
`rownames<-`(c("Purchased during", "Marketed during", "750 pounds feeder steer (dollar per head)", 'Total feed, handling, and management charge (dollar per head)',
                 'Interest on feeder and feed (dollar per head)', 'Death loss (1.3 percent of purchase)','Marketing 1/', 'Total expenses (dollar per head)', 
                 'Selling price (dollar per cwt 2/) 3/', "All cost (dollar per cwt)", 
                 'Net margin (dollar per cwt)', 
                 "Net margin (dollar per head)",
                 'Feed costs (dollar per cwt)', 'Total costs (dollar per cwt)',
                 'Choice feeder steer 750-800 pounds, Oklahoma city', 'Sorghum steam flaked (dollar per cwt) 4/',  'Corn Steam Flaked (dollar per cwt) 5/',
                 'Wheat Steam rolled (dollar per cwt) 6/', 'Corn gluten feed, wet (dollar per cwt) 7/', 'Urea (dollar per cwt) 8/', 
                 'Cottonseed meal (41 percent) (dollar per cwt) 9/',
                 'DDGS (dollar per cwt) 10/', 'Alfalfa hay (dollar per cwt) 11/', 'Corn silage (dollar per cwt) 12/', 'Cornstalks (dollar per cwt) 13/', 
                 'Tallow (dollar per cwt) 14/',
                 'Yellow grease (dollarper cwt) 15/', 'Limestone (dollar per cwt)', 'Supplmemts (dollar per cwt)',
                 'Interest, annual rate (percent) 16/'))
  
Expenses_table_historical_transposed <- Expenses_table_historical_transposed %>%
  tibble::rownames_to_column(var = "RowNames")
colnames(Expenses_table_historical_transposed) <- Expenses_table_historical_transposed[1,]
rownames(Expenses_table_historical_transposed) <- NULL 

wb <- createWorkbook()
addWorksheet(wb, "Historical data")
writeData(wb, sheet = 1, x = Expenses_table_historical_transposed, startRow = 1,  colNames = FALSE, rowNames = FALSE)

# Apply bold formatting to the first two rows
headerStyle <- createStyle(fontSize = 11, halign = "left", textDecoration = "bold")
addStyle(wb, sheet = 1, style = headerStyle, rows = 1, cols = 1:(ncol(Expenses_table_historical_transposed)), gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = 1, style = headerStyle, rows = 2, cols = 1:(ncol(Expenses_table_historical_transposed)), gridExpand = TRUE, stack = TRUE)

# Auto-size the columns for better appearance
setColWidths(wb, sheet = 1, cols = 1:(ncol(Expenses_table_historical_transposed)), widths = "auto")

# Add footnotes
footnotes <- c("1. Cattle sold free on board (F.O.B.), 4 percent shrink.", 
               "2. Hundredweight (cwt) = 100 pounds.",
               "3. Steers, total all grades, Texas/Oklahoma/New Mexico direct.",
               "4. USDA, Agricultural Marketing Service (AMS) sorghum price (with added premium for steam flaking).",
               "5. USDA, Agricultural Marketing Service (AMS) corn price (with added premium for steam flaking).",
               "6. USDA, Agricultural Marketing Service (AMS) wheat price (with added premium for steam rolling).",
               "7. USDA, Agricultural Marketing Service (AMS).",
               "8. World Bank Commodity Price Data of monthly prices (Pink sheet).",
               "9. National Grain and Oilseed Processor Feedstuff Report (AMS_3511), USDA, Agricultural Marketing Service (AMS).",
               "10. USDA, Agricultural Marketing Service (AMS).",
               "11. Texas, 'Agricultural Prices', USDA, National Agricultural Statistics Service (NASS).",
               "12. USDA, Agricultural Marketing Service (AMS).",
               "13. U.S. Ag. Prices Feedgrains and Hay, Livestock Marketing Information Center (LMIC).",
               "14. Weekly Tallows & Proteins prices, Livestock Marketing Information Center (LMIC).",
               "15. National Animal By-Product Feedstuff Report (AMS-3510), USDA, Agricultural Marketing Service (AMS).",
               "16. Fixed interest rate, feeder cattle, 11th District Federal Reserve.")

writeData(wb, sheet = 1, x = footnotes, startRow = 35, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb, "out_data/HPCFS_table_historical.xlsx", overwrite = TRUE)


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Out table (last 12 months)
Expenses_table <- COMBINED_data %>%
  dplyr::mutate(., Marketing = "F.O.B.", total_handling_cost = Feed_cost+Yardage_cost+Vet_cost,
                total_interest_cost = Interest_on_feeder + Interest_on_feed) %>%
  dplyr::filter(., date_purchased >= COMBINED_data$date_purchased %>%  max() %m-% months(12)) %>% 
  dplyr::mutate(., Purchased_date = format(date_purchased, "%h-%Y"), Marketed_date = format(date_marketed, "%h-%Y")) %>%
  dplyr:: select (., Purchased_date, Marketed_date, Feeder_cost, total_handling_cost, 
                  total_interest_cost, Death_loss, Marketing, Total_Expenses, 
                  steer_price, Selling_price_covering_tot_expense, Net_margin_cwt, Net_margin_head,
                  Feed_COG, Total_COG, feeder_cattle_price, monthly_sorg_steam_flk, monthly_corn_steam_flk, monthly_wht_steam_rolled,
                  monthly_average_sweet_br, monthly_average_urea, monthly_average_cottn_meal, monthly_average_DDGS,
                  monthly_average_alfalfa, monthly_corn_silage, monthly_average_other_hay,
                  monthly_average_tallow, monthly_average_yellow_gr, monthly_average_limestone, monthly_average_suppl, feeder_interest_rate)

Expenses_table_transposed <- t(Expenses_table) %>% as.data.frame() %>% 
  `rownames<-`(c("Purchased during", "Marketed during", "750 pounds feeder steer (dollar per head)", 'Total feed, handling, and management charge (dollar per head)',
                 'Interest on feeder and feed (dollar per head)', 'Death loss (1.3 percent of purchase)','Marketing 1/', 'Total expenses (dollar per head)', 
                 'Selling price (dollar per cwt 2/) 3/', "All cost (dollar per cwt)", 
                 'Net margin (dollar per cwt)', 
                 "Net margin (dollar per head)",
                 'Feed costs (dollar per cwt)', 'Total costs (dollar per cwt)',
                 'Choice feeder steer 750-800 pounds, Oklahoma city', 'Sorghum steam flaked (dollar per cwt) 4/',  'Corn Steam Flaked (dollar per cwt) 5/',
                 'Wheat Steam rolled (dollar per cwt) 6/', 'Corn gluten feed, wet (dollar per cwt) 7/', 'Urea (dollar per cwt) 8/', 
                 'Cottonseed meal (41 percent) (dollar per cwt) 9/',
                 'DDGS (dollar per cwt) 10/', 'Alfalfa hay (dollar per cwt) 11/', 'Corn silage (dollar per cwt) 12/', 'Cornstalks (dollar per cwt) 13/', 
                 'Tallow (dollar per cwt) 14/',
                 'Yellow grease (dollarper cwt) 15/', 'Limestone (dollar per cwt)', 'Supplmemts (dollar per cwt)',
                 'Interest, annual rate (percent) 16/'))

Expenses_table_transposed <- Expenses_table_transposed %>%
  tibble::rownames_to_column(var = "RowNames")
colnames(Expenses_table_transposed) <- Expenses_table_transposed[1,]
rownames(Expenses_table_transposed) <- NULL 

wb2 <- createWorkbook()
addWorksheet(wb2, "Historical data")
writeData(wb2, sheet = 1, x = Expenses_table_transposed, startRow = 1,  colNames = FALSE, rowNames = FALSE)

# Apply bold formatting to the first two rows
headerStyle <- createStyle(fontSize = 11, halign = "left", textDecoration = "bold")
addStyle(wb2, sheet = 1, style = headerStyle, rows = 1, cols = 1:(ncol(Expenses_table_transposed)), gridExpand = TRUE, stack = TRUE)
addStyle(wb2, sheet = 1, style = headerStyle, rows = 2, cols = 1:(ncol(Expenses_table_transposed)), gridExpand = TRUE, stack = TRUE)

# Auto-size the columns for better appearance
setColWidths(wb2, sheet = 1, cols = 1:(ncol(Expenses_table_transposed)), widths = "auto")

# Add footnotes
writeData(wb2, sheet = 1, x = footnotes, startRow = 35, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb2, "out_data/HPCFS_table_12months.xlsx", overwrite = TRUE)


#******************************************************************************************************************************************************************************************
# 4. Generating out_tables (FOR PUBLIC REPORTING)
#******************************************************************************************************************************************************************************************

# Out table (html version)
report_1 <- t(Expenses_table) %>% as.data.frame() %>% 
  `rownames<-`(c("Purchased during", "Marketed during", "750 pounds feeder steer", 'Total feed, handling, and management charge',
                 'Interest on feeder and feed', 'Death loss (1.3 percent of purchase)','Marketing 1/', 'Total expenses', 
                 'Selling price (dollar per cwt 2/) 3/', "All cost (dollar per cwt)", 
                 'Net margin (dollar per cwt)', 
                 "Net margin (dollar per head)",
                 'Feed costs', 'Total costs',
                 'Choice feeder steer 750-800 pounds, Oklahoma city', 'Sorghum steam flaked 4/',  'Corn Steam Flaked 5/',
                 'Wheat Steam rolled 6/', 'Corn gluten feed, wet 7/', 'Urea 8/', 
                 'Cottonseed meal (41 percent) 9/',
                 'DDGS 10/', 'Alfalfa hay 11/', 'Corn silage 12/', 'Cornstalks 13/', 
                 'Tallow 14/',
                 'Yellow grease 15/', 'Limestone', 'Supplmemts',
                 'Interest, annual rate (percent) 16/')) %>% 
  `colnames<-`(NULL) %>%
kbl(caption = "High Plains Cattle Feeding Simulator", bold = TRUE, italics = TRUE, escape = FALSE, align = "c", longtable = T) %>%
  kable_styling('striped', full_width = T) %>%
  row_spec (1:2,italic = TRUE, bold = TRUE) %>%
  pack_rows("Expenses (dollar per head):", 3, 8, italic = TRUE, bold = FALSE) %>%
  pack_rows("Selling price required to cover:", 9, 12, italic = TRUE, bold = FALSE) %>%
  pack_rows("Cost per 100 pounds gain (per 1 cwt gain):", 13, 14, italic = TRUE, bold = FALSE) %>%
  pack_rows("Prices (dollar per cwt):", 15, 30, italic = TRUE, bold = FALSE)  %>%
  kable_classic(full_width = F) %>% 
  kableExtra::footnote(., number = c("Cattle sold free on board (F.O.B.), 4 percent shrink.", 
                                     "Hundredweight (cwt) = 100 pounds.",
                                     "Steers, total all grades, Texas/Oklahoma/New Mexico direct.",
                                     "USDA, Agricultural Marketing Service (AMS) sorghum price (with added premium for steam flaking).",
                                     "USDA, Agricultural Marketing Service (AMS) corn price (with added premium for steam flaking).",
                                     "USDA, Agricultural Marketing Service (AMS) wheat price (with added premium for steam rolling).",
                                     "USDA, Agricultural Marketing Service (AMS).",
                                     "World Bank Commodity Price Data of monthly prices (Pink sheet).",
                                     "National Grain and Oilseed Processor Feedstuff Report (AMS_3511), USDA, Agricultural Marketing Service (AMS).",
                                     "USDA, Agricultural Marketing Service (AMS).",
                                     "Texas, 'Agricultural Prices', USDA, National Agricultural Statistics Service (NASS).",
                                     "USDA, Agricultural Marketing Service (AMS).",
                                     "U.S. Ag. Prices Feedgrains and Hay, Livestock Marketing Information Center (LMIC).",
                                     "Weekly Tallows & Proteins prices, Livestock Marketing Information Center (LMIC).",
                                     "National Animal By-Product Feedstuff Report (AMS-3510), USDA, Agricultural Marketing Service (AMS).",
                                     "Fixed interest rate, feeder cattle, 11th District Federal Reserve."),
                       alphabet = c("Source: USDA, NASS; USDA, AMS; USDA, ERS.",
                                    "Contact: Russell H. Knight (+1-816-412-4161, russell.h.knight@usda.gov)")) %>% 
  kable_classic(full_width = T, html_font = "Times New Roman")  

landscape(report_1, margin = "1inch") %>% 
  save_kable("out_tables/Simulator_table.html") 


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Out table (excel version)
wb3 <- createWorkbook()
addWorksheet(wb3, "Simulator table")
transposed_table <- t(Expenses_table) %>% as.data.frame() %>% 
  `rownames<-`(c("Purchased during", "Marketed during", "750 pounds feeder steer", 'Total feed, handling, and management charge',
                 'Interest on feeder and feed', 'Death loss (1.3 percent of purchase)','Marketing 1/', 'Total expenses', 
                 'Selling price (dollar per cwt 2/) 3/', "All cost (dollar per cwt)", 
                 'Net margin (dollar per cwt)', 
                 "Net margin (dollar per head)",
                 'Feed costs', 'Total costs',
                 'Choice feeder steer 750-800 pounds, Oklahoma city', 'Sorghum steam flaked 4/',  'Corn Steam Flaked 5/',
                 'Wheat Steam rolled 6/', 'Corn gluten feed, wet 7/', 'Urea 8/', 
                 'Cottonseed meal (41 percent) 9/',
                 'DDGS 10/', 'Alfalfa hay 11/', 'Corn silage 12/', 'Cornstalks 13/', 
                 'Tallow 14/',
                 'Yellow grease 15/', 'Limestone', 'Supplmemts',
                 'Interest, annual rate (percent) 16/'))
empty_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(transposed_table)))
names(empty_row) <- names(transposed_table)

transposed_table <- rbind(empty_row, transposed_table[1:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:3, ], empty_row, transposed_table[4:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:4, ], empty_row, transposed_table[5:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:10, ], empty_row, transposed_table[11:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:12, ], empty_row, transposed_table[13:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:13, ], empty_row, transposed_table[14:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:18, ], empty_row, transposed_table[19:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:19, ], empty_row, transposed_table[20:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:22, ], empty_row, transposed_table[23:nrow(transposed_table), ])
transposed_table <- rbind(transposed_table[1:23, ], empty_row, transposed_table[24:nrow(transposed_table), ])

ItalicStyle <- createStyle(textDecoration = c("italic"))
BoldStyle <- createStyle(textDecoration = c("bold"))
BoldItalicStyle <- createStyle(textDecoration = c("italic", "bold"))
upperBorderStyle <- createStyle(border = "top", borderStyle = "thin")
lowerBorderStyle <- createStyle(border = "bottom", borderStyle = "thin")

writeData(wb3, sheet = 1, transposed_table, startRow = 1, colNames = FALSE, rowNames = TRUE)

# Styling Marketed/Purchased rows of the table
addStyle(wb3, sheet = 1, style = BoldStyle, rows = 2:3, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE, stack = TRUE)
addStyle(wb3, sheet = 1, style = upperBorderStyle, rows = 2, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE, stack = TRUE)
addStyle(wb3, sheet = 1, style = lowerBorderStyle, rows = 3, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE, stack = TRUE)


writeData(wb3, sheet = 1, x = "High Plains Cattle Feeding Simulator", startRow = 1, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = " ", startRow = 4, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = "Expenses (dollar per head):", startRow = 5, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = " ", startRow = 4, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = " ", startRow = 11, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = " ", startRow = 13, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = "Selling price required to cover:", startRow = 14, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = " ", startRow = 19, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = "Cost per 100 pounds gain (per 1 cwt gain):", startRow = 20, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = " ", startRow = 23, startCol = 1, colNames = FALSE)
writeData(wb3, sheet = 1, x = "Prices (dollar per cwt):", startRow = 24, startCol = 1, colNames = FALSE)

addStyle(wb3, sheet = 1, style = BoldItalicStyle, rows = 1, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE)
addStyle(wb3, sheet = 1, style = ItalicStyle, rows = 12, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE)
addStyle(wb3, sheet = 1, style = BoldItalicStyle, rows = 5, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE)
addStyle(wb3, sheet = 1, style = BoldItalicStyle, rows = 14, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE)
addStyle(wb3, sheet = 1, style = BoldItalicStyle, rows = 20, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE)
addStyle(wb3, sheet = 1, style = BoldItalicStyle, rows = 24, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE)

addStyle(wb3, sheet = 1, style = lowerBorderStyle, rows = 41, cols = 1:(ncol(transposed_table) + 1), gridExpand = TRUE, stack = TRUE)

setColWidths(wb3, sheet = 1, cols = 1, widths = 40)

# Add footnotes
writeData(wb3, sheet = 1, x = footnotes, startRow = 44, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb3, "out_tables/Simulator_table.xlsx", overwrite = TRUE)











