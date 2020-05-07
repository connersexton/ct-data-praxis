######
## 7 May 2020
## US Census (Educational Attainment)
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")


library(dplyr)
library(tidyr)
library(readr)

## Load MSA Keys:
msa <- read_csv("../../Data/cbsa_subset_table.csv")[,-1]

files <- list.files(path = "../../Data/Census Data/Education/",
                    pattern = "edu[0-9]{4}\\.csv") %>%
  sapply(FUN = function(x){paste0("../../Data/Census Data/Education/", x)}) %>% 
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
        select_if(function(col){ sum(is.na(col)) == 0 && {sum(col) < 100*72}})) -> completed_set


#### Check What the Variables represent:
vars <- colnames(completed_set)[-c(1:3)]

ref = read_csv("../../Data/Census Data/Education/ACSST1Y2018.S1501_metadata_2020-05-07T124543.csv")

ref %>% 
  filter(GEO_ID %in% vars) %>% View()

#### Selected Vars
## 18-24:
## y_adult_nohs - less than hs
## y_adult_hs - S1501_C02_003E - hs graduate (or equivalent)
## y_adult_col_high - S1501_C02_005E - Bachelor's degree or higher

## 25 and over:
## adult_nohs - S1501_C02_008E - some high school, no degree
## adult_hs - S1501_C02_009E - hs graduate (or equivalent)
## adult_col_high - S1501_C02_015E - bachelors or higher

vars <- c("geo_id", "name", "year",
          y_adult_nohs = "S1501_C02_002E", y_adult_hs = "S1501_C02_003E", 
          y_adult_col_high = "S1501_C02_005E", adult_nohs = "S1501_C02_008E", 
          adult_hs = "S1501_C02_009E", adult_col_high = "S1501_C02_015E")

completed_set %>% 
  select(vars) %>% 
  arrange(geo_id, year) -> education_info

write.csv(education_info, file = "../../Data/Census Data/education_info.csv")

