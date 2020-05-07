######
## 7 May 2020
## US Census (Household Income)
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")


library(dplyr)
library(tidyr)
library(readr)

## Load MSA Keys:
msa <- read_csv("../../Data/cbsa_subset_table.csv")[,-1]

files <- list.files(path = "../../Data/Census Data/HH Income/",
                    pattern = "income[0-9]{4}\\.csv") %>%
  sapply(FUN = function(x){paste0("../../Data/Census Data/HH Income/", x)}) %>% 
  lapply(read.csv, na.strings = c("(X)", "N"))

`%notin%` <- Negate(`%in%`)
years = 2010:2018

df_store = data.frame(geo_id = numeric(),
                      name = character(),
                      variable = character(),
                      value = numeric())

for (i in 1:length(files)){
  acs = files[[i]]
  acs %>% 
    slice(-1) %>% 
    select(geo_id = GEO_ID, name = NAME, names(acs)[grepl(".*[0-9]{3}E$", names(acs))]) %>% 
    mutate(geo_id = as.numeric(gsub("[0-9]{3}M[0-9]{3}US", "", geo_id))) %>% 
    filter(geo_id %in% msa$cbsa.code) -> acs_sub
  
  acs_sub %>% 
    mutate(name = gsub("\\,.*", "", name)) %>% 
    gather("variable", "value", -name, -geo_id) %>% 
    mutate(year = years[i]) -> set
  
  df_store = rbind(df_store, set)
}


empty_set <- expand.grid(name = unique(df_store$name),
                         variable = unique(df_store$variable),
                         year = unique(df_store$year))

df_store %>% 
  left_join(empty_set, by = c("name", "variable", "year")) %>% 
  mutate(value = as.numeric(value)) %>% 
  spread("variable", "value") -> files_raw

cbind(files_raw[,c(1:3)],
      files_raw[,-c(1:3)] %>% 
        select_if(function(col){ sum(is.na(col)) == 0 })) -> completed_set


#### Check What the Variables represent:
vars <- colnames(completed_set)[-c(1:3)]

ref = read_csv("../../Data/Census Data/HH Income/ACSST1Y2018.S1901_metadata_2020-05-07T132300.csv")

ref %>% 
  filter(GEO_ID %in% vars) %>% View()

#### Selected Vars
## Household Income Brackets:
## S1901_C01_002E to S1901_C01_011E

## NonFamily Household Income Brackets:
## S1901_C04_002E to S1901_C04_011E

library(stringr)
brackets <- ref %>% 
  filter(GEO_ID %in% paste0("S1901_C04_0", c(paste0("0", 2:9), "10", "11"), "E")) %>% 
  mutate(id = gsub("^Estimate!!Nonfamily households!!Total!!", "", id)) %>% pull(id)

variable_defs <- data.frame(variable = c(paste0("S1901_C01_0", c(paste0("0", 2:9), "10", "11"), "E"), 
                                         paste0("S1901_C04_0", c(paste0("0", 2:9), "10", "11"), "E")),
                            new_var = c(rep("poverty", 3), "low_income", rep("middle_class", 3), 
                                        rep("upper_middle_class", 2), "upper_class", brackets),
                            type = rep(c("family", "nonfamily"), each = 10))

completed_set %>% 
  gather("variable", "value", -geo_id, -name, -year) %>% 
  left_join(variable_defs, by = "variable") %>% 
  group_by(geo_id, name, year, new_var, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  select(-name) %>% 
  na.omit() %>% 
  arrange(geo_id, year) -> income_info



write.csv(income_info, file = "../../Data/Census Data/income_info.csv")

