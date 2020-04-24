######
## 22 April 2020
## Creating CBSA Guide from U.S. Census Guidelines
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")

library(readxl)
library(dplyr)

## Import Data:
cbsa <- read_xls("../../Data/Raw Data/list1_2020.xls", skip = 2)
## Data Includes:
## CBSA titles, codes
## Counties that exist within each CBSA as well as county fips codes

## Fips codes for: NY, MA, CT
fips = c("36", "25", "09")
cbsa %>% 
  filter(`FIPS State Code` %in% fips) %>% 
  distinct(`CBSA Code`, `CBSA Title`, .keep_all = T) %>% 
  select(cbsa.code = `CBSA Code`, cbsa.title = `CBSA Title`, state = `State Name`) %>% 
  filter(state == "Connecticut" | cbsa.title == "Boston-Cambridge-Newton, MA-NH" |
           cbsa.title == "New York-Newark-Jersey City, NY-NJ-PA") %>% 
  mutate(cbsa.title = gsub("\\, .*", "", cbsa.title)) -> cbsa.subset

write.csv(cbsa.subset, file = "../../Data/cbsa_subset_table.csv")
  
