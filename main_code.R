##############################################################################################
#################  GAMS Code / Elliott Dennis and Shara Akat   ###############################
##############################################################################################

# 1. Setting working directory and loading packages/libraries
# setwd("/Users/sharaakat/Dropbox/akat_shara/usda_cattle_gams/HPCFS_ERS/")  # EXAMPLE (iOS)
# setwd("C:/Users/edennis8/Dropbox/Nebraska/students/akat_shara/usda_cattle_gams/HPCFS_ERS/") # EXAMPLE (Windows)
# setwd("C:/........../HPCFS_ERS/")    # put directory here (comment out (delete the #) to run)

## Download all necessary libraries
source("code/setwd.R")

# 2. Setting USDA-NASS key: copy and paste your USDA-NASS key below
# usdarnass::nass_set_key(key = "9D9051CD-D01C-3B05-85C3-5FD81DB8F8A4")
# usdarnass::nass_set_key(key = "......").   # put the key here (comment out (delete the #) to run)

# 3. Loading intake prices data
source("code/prices.R")


# 4. Estimating OUTWT
source("code/OUTWT.R")


# 5. Finding optimal diet and animal performace
## Inweight (lbs)
inwt <- 750   
source("code/OptimalDiet_AnimalPerformance.R")
# animal_performance_ADG_DOF %>%  summary()


# 6. Total expense calculation
source("code/TotalExpense.R")


# 7. Comparing simulators
source("code/Generate_TablesFigures.R")











