######
## 22 April 2020
## Collecting NonFarm Employment Numbers
## Connecticut, New York, Massachusetts
######
setwd("~/Documents/GitHub/ct-data-praxis/Data Collection R Files")
library(rvest)
library(xml2)
library(tidyr)
library(dplyr)

## Base URL
url = "https://www.bls.gov/opub/ee/2019/sae/annavg1_2018.htm"

## 4 separate tables to scrape...
tables <- list()
t = paste0("table1", c("a", "b", "c", "d"))
## Create Variable Codes
## (refer to table description file for variable descriptions)
var_names = list(tab1a = c("total", "ml", "con"),
                 tab1b = c("man", "ttu", "inf"),
                 tab1c = c("fin", "probu", "eduhe"),
                 tab1d = c("lehos", "oser", "gov"))

for (i in 1:length(t)){
  p = paste0("//*[@id='saaa_", t[i],"']")
  table <- url %>%
    read_html() %>%
    html_nodes(xpath = p) %>%
    html_table(fill = T)
  
  table <- table[[1]]
  names(table) <- c("area",
                    paste0(2016:2018, var_names[[i]][1]),
                    paste0(2016:2018, var_names[[i]][2]),
                    paste0(2016:2018, var_names[[i]][3]))
  
  table$tbl_id <- cumsum(!nzchar(table$area))
  states = c("New York", "Massachusetts", "Connecticut")
  for (q in 1:length(states)) {
    x = table$tbl_id[table$area == states[q]]
    table$tbl_id[table$tbl_id == x] = states[q]
  }
  
  label.vars = c("area", "tbl_id")
  table %>% 
    filter(tbl_id %in% states & nzchar(area)) %>% 
    na_if(., "-") %>% 
    mutate_at(vars(-label.vars), function(x){as.numeric(gsub(",", "", x))}) %>% 
    mutate(area = gsub("\\([0-9]{1}\\)", "", area)) %>% 
    select(-tbl_id) -> tables[[t[i]]]
  
}

tables
full_data <- cbind(tables[[1]],tables[[2]][,-1],tables[[3]][,-1],tables[[4]][,-1])
## From here, need to go long, split variable column to year and variable name then back to wide
full_data %>% 
  gather("variable", "value", -area) %>% 
  mutate(year = str_extract(variable, "^[0-9]{4}"),
         variable = gsub("^[0-9]{4}", "", variable)) %>% 
  spread(variable, value) -> full_data_wide

write.csv(full_data_wide, file = "../Data/bls_nonfarm_employment.csv")
