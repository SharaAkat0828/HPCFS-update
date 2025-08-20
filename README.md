# Updating High Plains Cattle Feeding Simulator (HPCFS)

This project aims to update HPCFS in accordance with the latest Nutrient Requirements of Beef Cattle (NRBC) manual. The script loads data from various sources and graphs/tables about feedlot closeout estimations. 



## Contents

- R scripts
| Script | Description |
|--------|-------------|
| `main_code.R` | Reads all codes and generates final output files/figures/tables. |
| `OptimalDiet_AnimalPerformance.R` | Estimates optimal diet composition and animal performance variables (ADG, AFC, DOF, etc.). |
| `OUTWT.R` | Estimates predicted finishing weights and model fitted values. |
| `prices.R` | Pulls all prices and finance variables for the model, joins and creates an `.RDS` document used for other R scripts. |
| `setwd.R` | Sets working directory and loads all necessary package/libraries to R. |
| `Generate_TablesFigures.R` | Generates figures that compare feedlots' variables. |
| `TotalExpense.R` | Estimates total feedlot expenses and generates `.RDS` file. |
| `Generate_Comparative_TablesFigures.R` | Generates figures that compare previous (old) vs new Simulator variables. |
| `Suppl_comparison.R` | Generates supplemental comparative graphs (combines several scripts and sources them using `Generate_Comparative_TablesFigures.R`). |


- **`README.md`**: Project overview.




