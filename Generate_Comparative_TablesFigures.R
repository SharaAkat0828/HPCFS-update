
#***********************************************************************************************************************************************************************************************************
# 1. Loading OLD and NEW feedlot data and joining them into 1
#***********************************************************************************************************************************************************************************************************

# 1.1. Old Simulator data 
old_HPCFS_data <- read_excel("./data/High Plains Cattle Feeding Simulator.xls", sheet = 2, skip = 6) %>% 
  .[, c(1, 2, 6, 7, 8, 10, 14, 15, 18, 42, 39, 38)] %>% 
  `colnames<-`(c("Purchased_date","Marketed_date", "Total_handling_feed_old","NON_feed_cost_old", "Death_loss_old", "Total_expense_old", 
                 "Steer_35_65_price", "Net_margin_old_cwt", "Feed_costs_old", "ADG_old", "DOF_old", "Outweight_old")) %>% 
  dplyr::mutate(year_purchased = year(Purchased_date), month_purchased = month(Purchased_date)) %>% 
  dplyr::select(., year_purchased, month_purchased, Purchased_date, Marketed_date, 
         Total_handling_feed_old, NON_feed_cost_old, Death_loss_old, Total_expense_old, Steer_35_65_price,  
         Net_margin_old_cwt, Feed_costs_old, ADG_old, DOF_old, Outweight_old) %>% 
  mutate(., Steer_35_65_price = as.numeric(as.character(Steer_35_65_price)),
         Net_margin_old_cwt = as.numeric(as.character(Net_margin_old_cwt)),
         Total_expense_old = as.numeric(as.character(Total_expense_old)),
         NON_feed_cost_old = as.numeric(as.character(NON_feed_cost_old)),
         Death_loss_old = as.numeric(as.character(Death_loss_old)),
         Outweight_old = as.numeric(as.character(Outweight_old)))


# 1.2. New Simulator data
directory_path <- "data"
Total_expenses <- readRDS(file = file.path(directory_path, "TotalExpense.rds")) 
COMBINED_data <- left_join(Total_expenses, old_HPCFS_data, by = c("year_purch" = "year_purchased", "month_purch" = "month_purchased")) 

min <- as.Date("2010-4-15")
max <- as.Date("2021-8-15")

#***********************************************************************************************************************************************************************************************************
# 2. Generating Comparative graphs 
#***********************************************************************************************************************************************************************************************************

# Historical Total expenses compared
Total_expense_compared_OldNew <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = Total_expense_old, color = "Old Simulator data", linetype = "Old Simulator data"), lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = Total_Expenses, color = "Updated Simulator data", linetype = "Updated Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Old Simulator data", "Updated Simulator data"),
                     values = c("purple",  "blue")) +
  scale_linetype_manual("",
                        breaks = c("Old Simulator data", "Updated Simulator data"),
                        values = c("dashed", "solid")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Dollar per head") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min,max)) +
  scale_y_continuous(breaks = seq(1100, 3000, 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position =  c(.2, .8),
        text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  geom_hline(yintercept = mean(COMBINED_data$Total_Expenses, na.rm = TRUE), col = "blue", linetype='solid') +
  geom_hline(yintercept = mean(as.numeric(as.character(COMBINED_data$Total_expense_old)), na.rm = TRUE), col = "purple", linetype='dashed') +
  ggtitle("Total Expense") +
  xlab("Closeout month")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Historical Average Daily Gain (ADG) compared
ADG_compared_OldNew <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = as.numeric(as.character(ADG_old)), color = "Old Simulator data", linetype = "Old Simulator data"), lwd = 0.8) +
  geom_line(aes(x = date_purchased, y = ADG, color = "Updated Simulator data", linetype = "Updated Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Updated Simulator data", "Old Simulator data"),
                     values = c("blue", "purple")) +
  scale_linetype_manual("", 
                        breaks = c("Updated Simulator data", "Old Simulator data"),
                        values = c("solid", "dashed"))  +
  xlab("") + theme_bw() + labs(title = "") + ylab("Pounds per day") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme_classic() +
  scale_y_continuous(breaks = seq(2, 4.5, 0.05)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position =  c(.7, 1.025),
        text = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  geom_hline(yintercept = mean(COMBINED_data$ADG, na.rm = TRUE), col = "blue",  linetype = "solid") +
  geom_hline(yintercept = mean(as.numeric(as.character(COMBINED_data$ADG_old)), na.rm = TRUE), col = "purple",linetype = "dashed") +
  ggtitle("Average Daily Gain") +
  xlab("Closeout month")


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Historical Days on feed (DOF) compared
DOF_compared_OldNew <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_purchased, y = DOF, color = "Updated Simulator data", linetype = "Updated Simulator data"), lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = DOF_old, color = "Old Simulator data", linetype = "Old Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Updated Simulator data", "Old Simulator data"),
                     values = c("blue", "purple")) +
  scale_linetype_manual("", 
                        breaks = c("Updated Simulator data", "Old Simulator data"),
                        values = c("solid", "dashed")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Days") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(130, 500, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position =  c(.2, 0.9),
        text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  geom_hline(yintercept = mean(COMBINED_data$DOF, na.rm = TRUE), col = "blue", linetype = 'solid') +
  geom_hline(yintercept = mean(COMBINED_data$DOF_old, na.rm = TRUE), col = "purple", linetype = 'dashed') +
  ggtitle("Days of Feed") +
  xlab("Closeout month")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Historical out weight compared ALL
OUTWT_USDA_quantity <- usdampr::mpr_request(slugIDs = "2477", report_time = report_time_analysis)$Detail %>% 
  tidyr::separate(., selling_basis_description, c("selling_type", "selling_transportation")) %>% 
  dplyr::filter(., class_description == "STEER", grade_description == "Total all grades", selling_type == "LIVE", selling_transportation == "FOB") %>%
  dplyr::mutate(., date = {report_date %m-% months(1)}, year = {year(date)}, month = {month(date)}) %>%
  dplyr::select(., date, year, month, report_title, class_description, grade_description, selling_type, selling_transportation, head_count, weight_range_avg, weighted_avg_price) %>%
  dplyr::arrange(., year, month) %>% mutate(., steer_live_OTWT = weight_range_avg) %>% 
  dplyr::select(., year, month, steer_live_OTWT) %>% 
  group_by(year, month) %>% 
  summarize(., steer_live_OTWT = mean(steer_live_OTWT))

OUTWT_USDA_quantity <- OUTWT_USDA_quantity %>% 
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))
  
combined_data <- dplyr::left_join(COMBINED_data, OUTWT_USDA_quantity, by = c("date_marketed" = "date")) 

Outweight_compared_OldNew <- ggplot(data = combined_data) +
  geom_line(aes(x = date_marketed, y = Outweight_old, color = "Old Simulator Data", linetype = "Old Simulator Data"),  lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = fitted_outwt, color = "Updated Simulator data", linetype = "Updated Simulator data"),  lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = steer_live_OTWT, color = "Actual (USDA) data", linetype = "Actual (USDA) data"),  lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Old Simulator Data", "Updated Simulator data", "Actual (USDA) data"),
                     values = c("purple", "blue", "black")) +
  scale_linetype_manual("", 
                        breaks = c("Old Simulator Data", "Updated Simulator data", "Actual (USDA) data"),
                        values = c("dashed", "solid", "dotted")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Pounds") +
  theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min,max)) +
  scale_y_continuous(breaks = seq(1100, 1500, 25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position =  c(.2, .9),
        text = element_text(size = 15),
        legend.text = element_text(size = 10)) +
  geom_hline(yintercept = mean(COMBINED_data$fitted_outwt, na.rm = TRUE), col = "blue", linetype = 'solid') +
  geom_hline(yintercept = mean(COMBINED_data$Outweight_old, na.rm = TRUE), col = "purple", linetype = 'dashed') +
  geom_hline(yintercept = mean(COMBINED_data$steer_live_OTWT, na.rm = TRUE), col = "black", linetype = 'solid') +
  ggtitle("Finishing Weight") +
  xlab("Closeout month")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Historical New margin per hundredweight compared 
Net_margin_compared_OldNew <- ggplot(data = COMBINED_data) +
  geom_line(aes(x = date_marketed, y = Net_margin_old_cwt, color = "Old Simulator data", linetype = "Old Simulator data"), lwd = 0.8) +
  geom_line(aes(x = date_marketed, y = Net_margin_cwt, color = "Updated Simulator data", linetype = "Updated Simulator data"), lwd = 0.8) +
  scale_color_manual("", 
                     breaks = c("Old Simulator data", "Updated Simulator data"),
                     values = c("purple", "blue")) +
  scale_linetype_manual("", 
                        breaks = c("Old Simulator data", "Updated Simulator data"),
                        values = c("dashed", "solid")) +
  xlab("") + theme_bw() + labs(title = "") + ylab("Dollar per cwt") + theme_classic() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min,max)) +
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position =  c(.2, .9),
        text = element_text(size = 15),
        legend.text = element_text(size = 10)) +
  geom_hline(yintercept = mean(COMBINED_data$Net_margin_old_cwt, na.rm = TRUE), col = "purple", linetype='dashed') +
  ggtitle("New Margin per cwt")  +
  geom_hline(yintercept = mean(COMBINED_data$Net_margin_cwt, na.rm = TRUE), col = "blue", linetype='solid') +
  xlab("Closeout month")




