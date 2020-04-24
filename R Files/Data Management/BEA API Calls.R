######
## 23 April 2020
## Querying BEA's API
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")
library(httr)
library(devtools)
library(bea.R)
library(dplyr)
library(tidyr)

## GDP DATA
bea.key <- "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"

msa = read.csv("../../Data/cbsa_subset_table.csv")

## Query Real GDP data by MSA from BEA's 'Regional' Data Set
## Table: CAGDP9

years = paste0(2010:2019, collapse = ",")
areas = paste0(msa$cbsa.code, collapse = ",")
userSpecList <- list('UserID' = bea.key,
                     'Method' = 'GetData',
                     'datasetname' = 'Regional',
                     'TableName' = 'CAGDP9',
                     'LineCode' = 1,
                     'GeoFips' = areas,
                     'Year' = years)	
BDT <- beaGet(userSpecList, asTable = TRUE)

drop.cols = c("Code", "UNIT_MULT")
BDT %>% 
  mutate(GeoName = gsub("\\, .*", "", GeoName)) %>% 
  select(-drop.cols) -> BDT

BDT
names(BDT)[4:12] <- paste0("GDP_", 2010:2018)

write.csv(BDT, file = "../../Data/BEA Data/real_gdp_msa.csv")

