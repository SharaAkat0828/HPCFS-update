################################################################################################################################
#Supplementary code verification -> NOT REQUIRED FOR SIMULATOR
################################################################################################################################

## Outweight verification
## 1). Compare simulator predicted OUTWT VS 
# USDA 5 Area Weekly Weighted Average Direct Slaughter Cattle report; Steer, all grades, Live, FOB
source("code/OUTWT.R")
graph_OUTWT


## 2). 2023 HPCFS vs. Pre 2023 comparison
source("code/Generate_Comparative_TablesFigures.R")
Total_expense_compared_OldNew
ADG_compared_OldNew
Outweight_compared_OldNew
Net_margin_compared_OldNew
DOF_compared_OldNew