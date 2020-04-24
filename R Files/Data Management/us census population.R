######
## 22 April 2020
## Creating CBSA Guide from U.S. Census Guidelines
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")

library(dplyr)
library(tidyr)
library(stringr)
population <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv")
cbsa <- read.csv("../../Data/cbsa_subset_table.csv")

drop.cols = c("MDIV", "LSAD", "STCOU", "CENSUS2010POP", "ESTIMATESBASE2010")
population %>% 
  filter(CBSA %in% cbsa$cbsa.code & 
           grepl("(Micropolitan|Metropolitan) Statistical Area", LSAD) == T) %>%  
  select(-drop.cols) %>%  
  mutate(NAME = as.character(NAME),
         NAME = gsub("\\, .*", "", NAME)) %>% 
  gather("variable", "value", -CBSA, -NAME) %>% 
  mutate(year = str_extract(variable, "[0-9]{4}$"),
         variable = gsub("[0-9]{4}$", "", variable)) %>% 
  spread(variable, value) %>% 
  mutate_at(vars(BIRTHS:RESIDUAL), as.numeric) -> pop.managed

write.csv(pop.managed, "../../Data/Census Data/population.csv")




