#####
## Pulling SUSB data
#####

setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")
library(dplyr)

cbsa_codes <- read.csv("../../Data/cbsa_subset_table.csv")
df <- read.csv("https://www2.census.gov/programs-surveys/susb/tables/2016/msa_3digitnaics_2016.txt?#")

df %>% 
  filter(MSA %in% cbsa_codes$cbsa.code & NCSDSCR == "Total") %>% 
  mutate(year = "2016")

links = c("https://www2.census.gov/programs-surveys/susb/datasets/2010/msa_3digitnaics_2010.txt",
          "https://www2.census.gov/programs-surveys/susb/datasets/2011/msa_3digitnaics_2011.txt",
          "https://www2.census.gov/programs-surveys/susb/datasets/2012/msa_3digitnaics_2012.txt",
          "https://www2.census.gov/econ/susb/data/2013/msa_3digitnaics_2013.txt",
          "https://www2.census.gov/programs-surveys/susb/datasets/2014/msa_3digitnaics_2014.txt",
          "https://www2.census.gov/programs-surveys/susb/datasets/2015/msa_3digitnaics_2015.txt?#",
          "https://www2.census.gov/programs-surveys/susb/tables/2016/msa_3digitnaics_2016.txt?#")

data_list = list()
years = 2010:2016
for (l in 1:length(links)){
  d = read.csv(links[l])
  d %>% 
    filter(MSA %in% cbsa_codes$cbsa.code & {if("NCSDSCR" %in% names(.)) NCSDSCR else NAICSDSCR} == "Total") %>% 
    mutate(year = years[l]) -> t_d
  data_list[[l]] <- t_d
  print(l)
}

names(data_list[[7]])

lapply(data_list, function(x){
  x <- x %>% select(MSA, year, ENTRSIZE, 
                    {if("FIRM" %in% names(.)) FIRM else FIRMTOT}, 
                    {if("ESTB" %in% names(.)) ESTB else ESTBTOT})
})

full_data <- data.frame(MSA = character(0),
                        year = integer(0),
                        ENTRSIZE = integer(0),
                        FIRM = integer(0),
                        ESTB = integer(0))

for (i in 1:length(data_list)){
  data_list[[i]] %>% 
    select(MSA, year, ENTRSIZE, 
           FIRM = {if("FIRM" %in% names(.)) "FIRM" else "FIRMTOT"}, 
           ESTB = {if("ESTB" %in% names(.)) "ESTB" else "ESTBTOT"}) -> t_d
  full_data = rbind(full_data, t_d)
}

write.csv(full_data, "../../Data/Census Data/Firm Entries and Exits/firm_estab_totals.csv")
