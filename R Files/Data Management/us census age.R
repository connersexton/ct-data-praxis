######
## 6 May 2020
## Exploring US Census Data
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")

library(dplyr)
library(tidyr)
library(readr)

## Load MSA Keys:
msa <- read_csv("../../Data/cbsa_subset_table.csv")[,-1]

## Read in ACS Files
files <- list.files(path = "../../Data/Census Data/ACS/",
                    pattern = "ACS[0-9]{4}\\.csv") %>%
  sapply(FUN = function(x){paste0("../../Data/Census Data/ACS/", x)}) %>% 
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
    select(geo_id = GEO_ID, name = NAME, names(acs)[grepl(".*PE$", names(acs))]) %>% 
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
        select_if(function(col){ sum(is.na(col)) == 0 && {sum(col) < 100*72}})) -> completed_set


#### Check What the Variables represent:
vars <- colnames(completed_set)[-c(1:3)]

ref = read_csv("../../Data/Census Data/ACS/ACSDP1Y2018.DP05_metadata_2020-05-06T113029.csv")

ref %>% 
  filter(GEO_ID %in% vars) %>% View()

#DP05_0002PE - p.Male
#DP05_0003PE - p.Female

## Age Brackets:
## DP05_00##PE
## (6:8) 19 and below
## (9:10) 20 - 34
## (11:12) 35 - 54
## (13:14) 55 - 64
## (15:16) 65 - 84

completed_set %>% 
  select(-name) %>% 
  arrange(geo_id, year) -> completed_set

age_data <- data.frame(
  age_one = rowSums(completed_set[,paste0("DP05_000",6:8,"PE")]),
  age_two = rowSums(completed_set[,paste0("DP05_00",c("09", "10"),"PE")]),
  age_three = rowSums(completed_set[,paste0("DP05_00",11:12,"PE")]),
  age_four = rowSums(completed_set[,paste0("DP05_00",13:14,"PE")]),
  age_five = rowSums(completed_set[,paste0("DP05_00",15:16,"PE")])
)

age_brackets <- cbind(completed_set[,1:2], age_data)

write.csv(age_brackets, "../../Data/Census Data/msa_age_brackets.csv")
