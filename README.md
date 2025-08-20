# Updating High Plains Cattle Feeding Simulator (HPCFS)

This project aims to update HPCFS in accordance with the latest Nutrient Requirements of Beef Cattle (NRBC) manual. The script loads data from various sources and generates graphs/tables about feedlot closeout estimations. 



## Contents

### R scripts

| Script                         | Description                                                                 |
|--------------------------------|-----------------------------------------------------------------------------|
| `main_code.R`                  | Reads all codes and generates final output files/figures/tables.            |
| `OptimalDiet_AnimalPerformance.R` | Estimates optimal diet composition and animal performance variables (ADG, AFC, DOF, etc.). |
| `OUTWT.R`                      | Estimates predicted finishing weights and model fitted values.              |
| `prices.R`                     | Pulls all prices and finance variables, joins, and creates an `.RDS` file used for other R scripts. |
| `setwd.R`                      | Sets working directory and loads all necessary package/libraries.           |
| `Generate_TablesFigures.R`     | Generates figures that compare feedlots' variables.                         |
| `TotalExpense.R`               | Estimates total feedlot expenses and generates an `.RDS` file.              |
| `Generate_Comparative_TablesFigures.R` | Generates figures comparing previous (old) vs new Simulator variables. |
| `Suppl_comparison.R`           | Generates supplemental comparative graphs using `Generate_Comparative_TablesFigures.R`. |

- **`README.md`**: Project overview.




