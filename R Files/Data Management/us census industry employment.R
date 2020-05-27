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

files <- list.files(path = "../../Data/Census Data/Industry Employment/",
                    pattern = "emp[0-9]{4}\\.csv") %>%
  sapply(FUN = function(x){paste0("../../Data/Census Data/Industry Employment/", x)}) %>% 
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

ref = read_csv("../../Data/Census Data/Industry Employment/ACSST1Y2018.S2403_metadata_2020-05-07T161526.csv")

ref %>% 
  filter(GEO_ID %in% vars) %>% 
  filter(grepl("Estimate!!Total", id)) %>% pull(id)

#### Selected Vars
## (S2403_C01_001E)Total
## (S2403_C01_002E)Agriculture/Forestry/Fishing&Hunting/Mining
## (S2403_C01_005E)Construction
## (S2403_C01_007E)Wholesale trade
## (S2403_C01_008E)Retail trade
## (S2403_C01_009E)Transportation/Warehousing/Utilities
## (S2403_C01_012E)Information
## (S2403_C01_013E)Finance/Insurance/Real Estate&Rental&Leasing
## (S2403_C01_016E)Professional/Scientific/Management/Administrative/Waste Management
## (S2403_C01_020E)Educational Services/Healthcare/Social Assistance
## (S2403_C01_023E)Arts/Entertainment/Recreation/Accommodation/Food Services

var_codes <- data.frame(variable = paste0("S2403_C01_0", 
                                          c(paste0("0", c(1:2, 5, 7:9)), 12:13, 16, 20, 23), 
                                          "E"),
                        label = c("total_emp", "agri", "construction", "wholesale", "retail", "transportation",
                                  "information", "finance", "professional", "education", "amenities"))

var_codes %>% 
  left_join(ref %>% select(-X3) %>% filter(GEO_ID %in% vars), by = c("variable" = "GEO_ID")) %>% 
  mutate(id = gsub("Estimate!!Total!!Civilian employed population 16 years and over!!", "", id)) -> var_codes

var_codes$id[1] <- "total"

cbind(completed_set[,c(1:3)],
      completed_set[,-c(1:3)] %>% 
        select_if(function(col){ min(col) > 100 })) %>% 
  gather("variable", "value", -geo_id, -name, -year) %>% 
  left_join(var_codes %>% select(-id), by = "variable") %>% 
  na.omit() %>%
  select(-variable) %>% 
  spread(label, value) -> emp_industries_counts

## Compute percentages:
var_codes %>% 
  mutate(label = as.character(label)) %>% 
  filter(label != "total_emp") %>% pull(label) -> mutate_vars

emp_industries_p <- emp_industries_counts
for (var in mutate_vars) {
  emp_industries_p[,var] <- round(emp_industries_p[,var]/emp_industries_p[,"total_emp"], 3)*100
}


write.csv(var_codes, file = "../../Data/Census Data/employment_industry_codebook.csv")

write.csv(emp_industries_counts, file = "../../Data/Census Data/emp_industries_counts.csv")
write.csv(emp_industries_p, file = "../../Data/Census Data/emp_industries_p.csv")
