
# 1. Create Master date Data.Frame to Merge ALL data
begin <- "2000-01-15" %>% as.Date()
end <- Sys.Date()        #Sys.Date() - current system date

report_time_analysis <- paste0(begin, ":", end)
Date <- seq(from = begin, to = as.Date(end), by = "month")
master_df <- data.frame(date = Date) %>% 
  dplyr::mutate(., year = {year(date)}, month = {month(date)}) 


# Setting necessary fixed parameters, tables and scalars and reading prices data                                                     
lb_to_kg <- 0.454   #lb to kg conversion 
directory_path <- "data"
all_prices <- readRDS(file = file.path(directory_path, "Prices_and_OUTWT.rds")) 

all_prices_and_outwt <- left_join(master_df, all_prices, by = c("year", "month")) %>% 
  na.omit() %>% dplyr::select(.,-date)




# 2. Diet composition                                                 
# 2.1. NRC - Grain feed composition
TABLEAU <- readxl::read_xlsx("data/NRBC_feed_composition.xlsx") %>% .[,-1] %>% 
  as.data.frame()

rownames(TABLEAU) <- (c("DM", "TDN", "DE", "ME", "NEm",
                        "NEg", "NDF", "Roughage_NDF", "CP", "Ca",
                        "P"))




# 2.2. Ration composition optimization
# Overall, there are 14 feed types. We will try to determine the most optimal dietary inclusion proportions.
storage_data_frame <- data.frame(matrix(nrow = nrow(all_prices_and_outwt), ncol = 47))

## prices are given in $ per cwt as-fed
DIET_COMPOSITION <- function(prices_data) {
  
  for(i in 1:nrow(prices_data)) {
    
    # prices are $ per cwt DM
    P_C_cwt = prices_data[i, 'monthly_corn_steam_flk']/(TABLEAU["DM", "corn_sf"]/100)   # corn
    P_S_cwt = prices_data[i, 'monthly_sorg_steam_flk']/(TABLEAU["DM", "sorghum_sf"]/100)   # sorghum
    P_W_cwt = prices_data[i, 'monthly_wht_steam_rolled']/(TABLEAU["DM", "wheat_sf"]/100) 
    P_SwtBr_cwt = prices_data[i, 'monthly_average_sweet_br'] /(TABLEAU["DM", "sweet_bran"]/100)
    
    P_U_cwt = prices_data[i, 'monthly_average_urea']/(TABLEAU["DM", "urea"]/100)  
    P_c_S_cwt = prices_data[i, 'monthly_average_cottn_meal']/(TABLEAU["DM", "cottons_m"]/100)
    P_DDGS_cwt = prices_data[i, 'monthly_average_DDGS']/(TABLEAU["DM", "DDGS"]/100)
    
    P_A_cwt = prices_data[i, 'monthly_average_alfalfa']/(TABLEAU["DM", "alfalfa"]/100) 
    P_C_sil_cwt = prices_data[i, 'monthly_corn_silage']/(TABLEAU["DM", "silage_corn"]/100) 
    P_Cornstlk_cwt = prices_data[i, 'monthly_average_other_hay']/(TABLEAU["DM", "cornstalks"]/100) 
    
    P_T_cwt = prices_data[i, 'monthly_average_tallow']/(TABLEAU["DM", "tallow"]/100)
    P_Y_cwt =  prices_data[i, 'monthly_average_yellow_gr']/(TABLEAU["DM", "yellow_grease"]/100)
    P_L_cwt = prices_data[i, 'monthly_average_limestone']/(TABLEAU["DM", "limestone"]/100)
    P_Supl_cwt  = prices_data[i, 'monthly_average_suppl']/(TABLEAU["DM", "supplmt"]/100)
    
    DMI_changed = prices_data[i, 'DMI_simulator_monthly']
    
    # prices are $ per kg DM
    ## 1 cwt = 100 lb
    P_C = P_C_cwt/100*lb_to_kg
    P_S = P_S_cwt/100*lb_to_kg
    P_W = P_W_cwt/100*lb_to_kg
    P_SwtBr = P_SwtBr_cwt/100*lb_to_kg
    
    P_U = P_U_cwt/100*lb_to_kg
    P_c_S = P_c_S_cwt/100*lb_to_kg
    P_DDGS = P_DDGS_cwt/100*lb_to_kg
    
    P_A = P_A_cwt/100*lb_to_kg
    P_C_sil = P_C_sil_cwt/100*lb_to_kg
    P_Cornstlk = P_Cornstlk_cwt/100*lb_to_kg
    
    P_T = P_T_cwt/100*lb_to_kg
    P_Y = P_Y_cwt/100*lb_to_kg
    P_L = P_L_cwt/100*lb_to_kg
    P_Supl = P_Supl_cwt/100*lb_to_kg
    
    
    # Objective function:
    f.obj <- c(P_C*DMI_changed, P_S*DMI_changed, P_W*DMI_changed, P_SwtBr*DMI_changed, 
               P_U*DMI_changed, P_c_S*DMI_changed, P_DDGS*DMI_changed,
               P_A*DMI_changed, P_C_sil*DMI_changed, P_Cornstlk*DMI_changed,
               P_T*DMI_changed, P_Y*DMI_changed, 
               P_L*DMI_changed, P_Supl*DMI_changed)
    
    # DI_C + DI_S + DI_W + DI_SwtBr + DI_U + DI_C_S + DI_DDGS + DI_A + DI_c_sil + DI_Cornstlk + DI_T + DI_Y + DI_L + DI_Supl
    
    f.con <- matrix (c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   #1.1
                       1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   #1.2
                       1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #1.3
                       0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  #1.4.1
                       0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  #1.4.2
                       0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,   #1.6
                       0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,   #1.7
                       0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,  #1.8
                       0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,   #1.9
                       0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,   #1.10
                       0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,   #1.11
                       0, 0, 0, 0, 1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0,   #1.11.1  (12)
                       0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0,   #1.12
                       0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0,   #1.13
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,  #1.14
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,  #1.15
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,  #1.16
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,  #1.17
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,  #1.18
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  #1.18.1 (20)
                       
                       TABLEAU["DM", 1], TABLEAU["DM", 2],TABLEAU["DM", 3],TABLEAU["DM", 4],TABLEAU["DM", 5],TABLEAU["DM", 6],TABLEAU["DM", 7],
                       TABLEAU["DM", 8],TABLEAU["DM", 9],TABLEAU["DM", 10],TABLEAU["DM", 11],TABLEAU["DM", 12],TABLEAU["DM", 13],TABLEAU["DM", 14], #2.19
                       
                       TABLEAU["DM", 1], TABLEAU["DM", 2],TABLEAU["DM", 3],TABLEAU["DM", 4],TABLEAU["DM", 5],TABLEAU["DM", 6],TABLEAU["DM", 7],
                       TABLEAU["DM", 8],TABLEAU["DM", 9],TABLEAU["DM", 10],TABLEAU["DM", 11],TABLEAU["DM", 12],TABLEAU["DM", 13],TABLEAU["DM", 14], #2.20
                       
                       TABLEAU["Roughage_NDF", 1], TABLEAU["Roughage_NDF", 2],TABLEAU["Roughage_NDF", 3],TABLEAU["Roughage_NDF", 4],TABLEAU["Roughage_NDF", 5],TABLEAU["Roughage_NDF", 6],TABLEAU["Roughage_NDF", 7],
                       TABLEAU["Roughage_NDF", 8],TABLEAU["Roughage_NDF", 9],TABLEAU["Roughage_NDF", 10],TABLEAU["Roughage_NDF", 11],TABLEAU["Roughage_NDF", 12],TABLEAU["Roughage_NDF", 13],TABLEAU["Roughage_NDF", 14], #2.21
                       TABLEAU["Roughage_NDF", 1], TABLEAU["Roughage_NDF", 2],TABLEAU["Roughage_NDF", 3],TABLEAU["Roughage_NDF", 4],TABLEAU["Roughage_NDF", 5],TABLEAU["Roughage_NDF", 6],TABLEAU["Roughage_NDF", 7],
                       TABLEAU["Roughage_NDF", 8],TABLEAU["Roughage_NDF", 9],TABLEAU["Roughage_NDF", 10],TABLEAU["Roughage_NDF", 11],TABLEAU["Roughage_NDF", 12],TABLEAU["Roughage_NDF", 13],TABLEAU["Roughage_NDF", 14], #2.22
                       
                       TABLEAU["CP", 1], TABLEAU["CP", 2],TABLEAU["CP", 3],TABLEAU["CP", 4],TABLEAU["CP", 5],TABLEAU["CP", 6],TABLEAU["CP", 7],
                       TABLEAU["CP", 8],TABLEAU["CP", 9],TABLEAU["CP", 10],TABLEAU["CP", 11],TABLEAU["CP", 12],TABLEAU["CP", 13],TABLEAU["CP", 14], #2.23
                       TABLEAU["CP", 1], TABLEAU["CP", 2],TABLEAU["CP", 3],TABLEAU["CP", 4],TABLEAU["CP", 5],TABLEAU["CP", 6],TABLEAU["CP", 7],
                       TABLEAU["CP", 8],TABLEAU["CP", 9],TABLEAU["CP", 10],TABLEAU["CP", 11],TABLEAU["CP", 12],TABLEAU["CP", 13],TABLEAU["CP", 14], #2.24
                       TABLEAU["Ca", 1], TABLEAU["Ca", 2],TABLEAU["Ca", 3],TABLEAU["Ca", 4],TABLEAU["Ca", 5],TABLEAU["Ca", 6],TABLEAU["Ca", 7],
                       TABLEAU["Ca", 8],TABLEAU["Ca", 9],TABLEAU["Ca", 10],TABLEAU["Ca", 11],TABLEAU["Ca", 12],TABLEAU["Ca", 13],TABLEAU["Ca", 14], #2.25
                       TABLEAU["Ca", 1], TABLEAU["Ca", 2],TABLEAU["Ca", 3],TABLEAU["Ca", 4],TABLEAU["Ca", 5],TABLEAU["Ca", 6],TABLEAU["Ca", 7],
                       TABLEAU["Ca", 8],TABLEAU["Ca", 9],TABLEAU["Ca", 10],TABLEAU["Ca", 11],TABLEAU["Ca", 12],TABLEAU["Ca", 13],TABLEAU["Ca", 14], #2.26
                       TABLEAU["P", 1], TABLEAU["P", 2],TABLEAU["P", 3],TABLEAU["P", 4],TABLEAU["P", 5],TABLEAU["P", 6],TABLEAU["P", 7],
                       TABLEAU["P", 8],TABLEAU["P", 9],TABLEAU["P", 10],TABLEAU["P", 11],TABLEAU["P", 12],TABLEAU["P", 13],TABLEAU["P", 14], #2.27
                       TABLEAU["P", 1], TABLEAU["P", 2],TABLEAU["P", 3],TABLEAU["P", 4],TABLEAU["P", 5],TABLEAU["P", 6],TABLEAU["P", 7],
                       TABLEAU["P", 8],TABLEAU["P", 9],TABLEAU["P", 10],TABLEAU["P", 11],TABLEAU["P", 12],TABLEAU["P", 13],TABLEAU["P", 14], #2.28
                       
                       TABLEAU["Ca", 1]-1.2*TABLEAU["P", 1], TABLEAU["Ca", 2]-1.2*TABLEAU["P", 2],TABLEAU["Ca", 3]-1.2*TABLEAU["P", 3],TABLEAU["Ca", 4]-1.2*TABLEAU["P", 4],TABLEAU["Ca", 5]-1.2*TABLEAU["P", 5],
                       TABLEAU["Ca", 6]-1.2*TABLEAU["P", 6],TABLEAU["Ca", 7]-1.2*TABLEAU["P", 7],TABLEAU["Ca", 8]-1.2*TABLEAU["P", 8],TABLEAU["Ca", 9]-1.2*TABLEAU["P", 9],
                       TABLEAU["Ca", 10]-1.2*TABLEAU["P", 10],TABLEAU["Ca", 11]-1.2*TABLEAU["P", 11],TABLEAU["Ca", 12]-1.2*TABLEAU["P", 12],TABLEAU["Ca", 13]-1.2*TABLEAU["P", 13],TABLEAU["Ca", 14]-1.2*TABLEAU["P", 14], # 2.29
                       
                       TABLEAU["Ca", 1]-1.5*TABLEAU["P", 1], TABLEAU["Ca", 2]-1.5*TABLEAU["P", 2],TABLEAU["Ca", 3]-1.5*TABLEAU["P", 3],TABLEAU["Ca", 4]-1.5*TABLEAU["P", 4],TABLEAU["Ca", 5]-1.5*TABLEAU["P", 5],
                       TABLEAU["Ca", 6]-1.5*TABLEAU["P", 6],TABLEAU["Ca", 7]-1.5*TABLEAU["P", 7],TABLEAU["Ca", 8]-1.5*TABLEAU["P", 8],TABLEAU["Ca", 9]-1.5*TABLEAU["P", 9],
                       TABLEAU["Ca", 10]-1.5*TABLEAU["P", 10],TABLEAU["Ca", 11]-1.5*TABLEAU["P", 11],TABLEAU["Ca", 12]-1.5*TABLEAU["P", 12],TABLEAU["Ca", 13]-1.5*TABLEAU["P", 13],TABLEAU["Ca", 14]-1.5*TABLEAU["P", 14], # 2.30
                       
                       # NEg
                       TABLEAU["NEg", 1], TABLEAU["NEg", 2],TABLEAU["NEg", 3],TABLEAU["NEg", 4],TABLEAU["NEg", 5],TABLEAU["NEg", 6],TABLEAU["NEg", 7],
                       TABLEAU["NEg", 8],TABLEAU["NEg", 9],TABLEAU["NEg", 10],TABLEAU["NEg", 11],TABLEAU["NEg", 12],TABLEAU["NEg", 13],TABLEAU["NEg", 14], #2.31
                       TABLEAU["NEg", 1], TABLEAU["NEg", 2],TABLEAU["NEg", 3],TABLEAU["NEg", 4],TABLEAU["NEg", 5],TABLEAU["NEg", 6],TABLEAU["NEg", 7],
                       TABLEAU["NEg", 8],TABLEAU["NEg", 9],TABLEAU["NEg", 10],TABLEAU["NEg", 11],TABLEAU["NEg", 12],TABLEAU["NEg", 13],TABLEAU["NEg", 14]), #2.32
                     
                     nrow=34, byrow=TRUE)
    
    f.dir <- c("==", ">=", "<=",
               ">=", "<=", 
               ">=","<=", #(7)
               ">=","<=", 
               ">=","<=", # (11)
               "==",
               ">=","<=", 
               ">=","<=", 
               
               ">=","<=", "==", 
               "==",  # 20
               
               ">=", "<=", ">=", "<=",">=", "<=", ">=", "<=", 
               ">=", "<=",">", "<", ">=", "<=")
    
    f.rhs <- c(1, 0.65, 0.8, 
               0, 0.20, 
               # 0.2, 0.2,
               0, 0.015, 
               0, 0.05, 
               0, 0.20, #(11)
               0.015,
               
               0.06, 0.15, 
               0.00, 0.03, 
               
               0, 0.025, 0.015,
               0.010,  # 20
               
               65, 85, 4, 5, 13.3, 13.6, 0.2, 1,
               0, 0.7, 0, 0, 1.37, 1.52)
    
    min <- lpSolve::lp("min", f.obj, f.con, f.dir, f.rhs)
    
    dietary_inclusions <- (min$solution %>% as.data.frame())
    rownames(dietary_inclusions) <- c("corn_sf", "sorghum_sf", "wheat_sf", "Sweet_bran", "urea", "cottns_m", "DDGS", "alfalfa", 
                                      "silage_corn", "cornstalks", "tallow", "yellow_grease", "limestone", "supplmt")
    
    colnames(dietary_inclusions) <- (c("Dietary_inlcution"))
    
    dietary_inclusions_quant <- dietary_inclusions %>% 
      dplyr::mutate(., quantity_kg = Dietary_inlcution * DMI_changed)
    
    
    storage_data_frame[i,] <-  prices_data[i,] %>% 
      dplyr::mutate(., dietary_inclusions["corn_sf", "Dietary_inlcution"], dietary_inclusions["sorghum_sf", "Dietary_inlcution"],
             dietary_inclusions["wheat_sf", "Dietary_inlcution"], dietary_inclusions["Sweet_bran", "Dietary_inlcution"],
             
             dietary_inclusions["urea", "Dietary_inlcution"], dietary_inclusions["cottns_m", "Dietary_inlcution"], 
             dietary_inclusions["DDGS", "Dietary_inlcution"], 
             
             dietary_inclusions["alfalfa", "Dietary_inlcution"], dietary_inclusions["silage_corn", "Dietary_inlcution"],
             dietary_inclusions["cornstalks", "Dietary_inlcution"],
             
             dietary_inclusions["tallow", "Dietary_inlcution"], dietary_inclusions["yellow_grease", "Dietary_inlcution"],
             dietary_inclusions["limestone", "Dietary_inlcution"], dietary_inclusions["supplmt", "Dietary_inlcution"],
             
             dietary_inclusions_quant["corn_sf", "quantity_kg"], dietary_inclusions_quant["sorghum_sf", "quantity_kg"], 
             dietary_inclusions_quant["wheat_sf", "quantity_kg"], dietary_inclusions_quant["Sweet_bran", "quantity_kg"],
             
             dietary_inclusions_quant["urea", "quantity_kg"], dietary_inclusions_quant["cottns_m", "quantity_kg"], 
             dietary_inclusions_quant["DDGS", "quantity_kg"], 
             
             dietary_inclusions_quant["alfalfa", "quantity_kg"], dietary_inclusions_quant["silage_corn", "quantity_kg"], 
             dietary_inclusions_quant["cornstalks", "quantity_kg"], 
             
             dietary_inclusions_quant["tallow", "quantity_kg"], dietary_inclusions_quant["yellow_grease", "quantity_kg"], 
             dietary_inclusions_quant["limestone", "quantity_kg"], dietary_inclusions_quant["supplmt", "quantity_kg"]
      )
  }
  
  storage_data_frame <- storage_data_frame %>% 
    `colnames<-`(c("year", "month", "monthly_corn_steam_flk", "monthly_sorg_steam_flk", "monthly_wht_steam_rolled",   
                   "monthly_average_sweet_br", "monthly_average_urea", "monthly_average_cottn_meal", "monthly_average_DDGS", "monthly_average_alfalfa",   
                   "monthly_corn_silage", "monthly_average_other_hay",  "monthly_average_tallow", "monthly_average_yellow_gr", "monthly_average_limestone", 
                   "monthly_average_suppl", "DMI_simulator_monthly", "fitted_outwt","steer_live_OTWT_actualUSDA",
                   "DI_corn_sf", "DI_sorg_sf", "DI_wheat_sf", "DI_sweet_bran",
                   "DI_urea", "DI_cottn_m", "DI_DDGS",
                   "DI_alfalfa", "DI_corn_silage", "DI_cornstalks",
                   "DI_tallow", "DI_yellow_gr", "DI_limestone", "DI_supplmt", 
                   "DM_Q_corn", "DM_Q_sorg", "DM_Q_wheat", "DM_Q_sweet_br", 
                   "DM_Q_urea", "DM_Q_cottn_m", "DM_Q_DDGS", 
                   "DM_Q_alfalfa", "DM_Q_corn_silage", "DM_Q_cornstalks", 
                   "DM_Q_tallow", "DM_Q_yellow_gr", "DM_Q_limestone", "DM_Q_supplmt"))
  return(storage_data_frame) 
}
storage_data_frame <- DIET_COMPOSITION(all_prices_and_outwt) 

# storage_data_frame %>% summary()





# 2.3. Ration composition
storage_data_frame_2 <- data.frame(matrix(nrow = nrow(storage_data_frame), ncol = 50))

ration_composition <- function(storage_data_frame) {
  
  for(i in 1:nrow(storage_data_frame)) {
    
    feed_formation <- cbind(t(TABLEAU), dietary_inclusion = c(storage_data_frame[i, "DI_corn_sf"], storage_data_frame[i, "DI_sorg_sf"], storage_data_frame[i, "DI_wheat_sf"],
                                                              storage_data_frame[i, "DI_sweet_bran"], 
                                                              storage_data_frame[i, "DI_urea"], storage_data_frame[i, "DI_cottn_m"], storage_data_frame[i, "DI_DDGS"],
                                                              storage_data_frame[i, "DI_alfalfa"], storage_data_frame[i, "DI_corn_silage"], storage_data_frame[i, "DI_cornstalks"],
                                                              storage_data_frame[i, "DI_tallow"], storage_data_frame[i, "DI_yellow_gr"], 
                                                              storage_data_frame[i, "DI_limestone"], storage_data_frame[i, "DI_supplmt"])) %>% as.data.frame() %>%
      dplyr::relocate(., dietary_inclusion) %>%
      dplyr::mutate(., cal_NEm = NEm*0.454*100,
                    DM_inlclusion = (DM * dietary_inclusion),
                    TDN_inclusion = (TDN * dietary_inclusion),
                    DE_inclusion = DE * dietary_inclusion,
                    ME_inclusion = ME * dietary_inclusion,
                    NEm_inclusion = NEm * dietary_inclusion,
                    NEg_inclusion = NEg * dietary_inclusion,
                    NDF_inclusion = NDF * dietary_inclusion,
                    Roughage_NDF_inclusion = (Roughage_NDF * dietary_inclusion),
                    CP_inclusion = (CP * dietary_inclusion),
                    Ca_inclusion = (Ca * dietary_inclusion),
                    P_inclusion = (P * dietary_inclusion),
                    cal_NEm_inclusion = cal_NEm * dietary_inclusion) %>%
      rbind(., diet_composition = bind_rows(summarise(.,across(where(is.numeric), sum)))) %>%
      .[c("diet_composition"),c(14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)]
    
    storage_data_frame_2[i,] <-  storage_data_frame[i,] %>% 
      dplyr::mutate(., feed_formation["diet_composition", "NEm_inclusion"],
             feed_formation["diet_composition", "NEg_inclusion"],
             feed_formation["diet_composition", "DM_inlclusion"])
    
  }
  storage_data_frame_2 <- storage_data_frame_2 %>% 
    `colnames<-`(c("year", "month", "monthly_corn_steam_flk", "monthly_sorg_steam_flk", "monthly_wht_steam_rolled",   
                   "monthly_average_sweet_br", "monthly_average_urea", "monthly_average_cottn_meal", "monthly_average_DDGS", "monthly_average_alfalfa",   
                   "monthly_corn_silage", "monthly_average_other_hay",  "monthly_average_tallow", "monthly_average_yellow_gr",  "monthly_average_limestone", 
                   "monthly_average_suppl", "DMI_simulator_monthly", "fitted_outwt", "steer_live_OTWT_actualUSDA",
                   "DI_corn_sf", "DI_sorg_sf", "DI_wheat_sf", "DI_sweet_bran",
                   "DI_urea", "DI_cottn_m", "DI_DDGS",
                   "DI_alfalfa", "DI_corn_silage", "DI_cornstalks",
                   "DI_tallow", "DI_yellow_gr", "DI_limestone", "DI_supplmt", 
                   "DM_Q_corn", "DM_Q_sorg", "DM_Q_wheat", "DM_Q_sweet_br", 
                   "DM_Q_urea", "DM_Q_cottn_m", "DM_Q_DDGS", 
                   "DM_Q_alfalfa", "DM_Q_corn_silage", "DM_Q_cornstalks", 
                   "DM_Q_tallow", "DM_Q_yellow_gr", "DM_Q_limestone", "DM_Q_supplmt", "NEm_inclusion", "NEg_inclusion", "DM_inlclusion"))
  return(storage_data_frame_2)
}
ration_composition_calc <- ration_composition(storage_data_frame)





# 2.4. Animal performance calculation with minimum NEg and NEm                                                   
storage_data_frame_3 <- data.frame(matrix(nrow = nrow(ration_composition_calc), ncol = 54))

animal_performance_ADG <- function(ration_composition_calc) {
  
  for (i in 1:nrow(ration_composition_calc)) { 
    DMI_kg <-  ration_composition_calc[i, 'DMI_simulator_monthly']
    gain <- ration_composition_calc[i, "fitted_outwt"] - inwt
    average_BW <- mean(c(inwt, ration_composition_calc[i, "fitted_outwt"])) * 0.454
    shrunk_BW_kg <- ration_composition_calc[i, "fitted_outwt"] * 0.96 * lb_to_kg
    
    SRW <- 478   # standard reference weight from NRC book 
    
    final_SBW <- ration_composition_calc[i, "fitted_outwt"] * lb_to_kg 
    equivalent_SBW <- (SRW/(shrunk_BW_kg)) * average_BW
    maint_requirement <- (average_BW)^0.75 * 0.077
    
    maint_feed <- maint_requirement/ration_composition_calc[i,"NEm_inclusion"]
    
    residual <- DMI_kg - maint_feed
    feed_maint_proportion <- maint_feed/DMI_kg
    retained_energy <- ration_composition_calc[i,"NEg_inclusion"] * residual
    SWG <- (12.341 * (retained_energy)^(0.9116) * (0.891 * equivalent_SBW)^(-0.6837))/0.96   # shrunk bodyweight gain
    
    ADG <- SWG/lb_to_kg   # average daily gain in lbs
    DOF <- gain/ADG 
    Feed_DM_to_ADG <- (DMI_kg/0.454)/ADG
    
    storage_data_frame_3[i,] <- ration_composition_calc[i,] %>% 
      dplyr::mutate(., ADG, DOF, Feed_DM_to_ADG, feed_maint_proportion)
    
  }
  
  return(storage_data_frame_3)
}
animal_performance_ADG_DOF <- animal_performance_ADG(ration_composition_calc) %>% 
  `colnames<-`(c("year", "month", "monthly_corn_steam_flk", "monthly_sorg_steam_flk", "monthly_wht_steam_rolled",   
                 "monthly_average_sweet_br", "monthly_average_urea", "monthly_average_cottn_meal", "monthly_average_DDGS", "monthly_average_alfalfa",   
                 "monthly_corn_silage", "monthly_average_other_hay",  "monthly_average_tallow", "monthly_average_yellow_gr", "monthly_average_limestone", 
                 "monthly_average_suppl", "DMI_simulator_monthly", "fitted_outwt", "steer_live_OTWT_actualUSDA",
                 "DI_corn_sf", "DI_sorg_sf", "DI_wheat_sf", "DI_sweet_bran",
                 "DI_urea", "DI_cottn_m", "DI_DDGS",
                 "DI_alfalfa", "DI_corn_silage", "DI_cornstalks",
                 "DI_tallow", "DI_yellow_gr", "DI_limestone", "DI_supplmt", 
                 "DM_Q_corn", "DM_Q_sorg", "DM_Q_wheat", "DM_Q_sweet_br", 
                 "DM_Q_urea", "DM_Q_cottn_m", "DM_Q_DDGS", 
                 "DM_Q_alfalfa", "DM_Q_corn_silage", "DM_Q_cornstalks", 
                 "DM_Q_tallow", "DM_Q_yellow_gr", "DM_Q_limestone", "DM_Q_supplmt", "NEm_inclusion", "NEg_inclusion","DM_inlclusion", "ADG", "DOF", 
                 "Feed_DM_to_ADG", "feed_maint_proportion")) 

animal_performance_ADG_DOF_graph <- animal_performance_ADG_DOF %>% 
  dplyr::mutate(feed_maint_proportion_old = 0.449,
         date = as.Date(zoo::as.yearmon(paste(year, month), "%Y %m"))) 


# maintenance feed proportion
animal_performance_ADG_DOF %>%  summary()

directory_path <- "data"
saveRDS(animal_performance_ADG_DOF, file = file.path(directory_path, "Diet_Performance.rds"))


