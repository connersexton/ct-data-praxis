######
## 22 April 2020
## Collecting Metropolitan NECTA and MSA areas
## (New England City and Town Areas)
## (Metropolitan Statistical Areas)
######

setwd("~/Documents/GitHub/ct-data-praxis/Data Collection R Files")
library(rvest)
library(tidyr)
library(dplyr)

## NECTAS:
## Massachusetts and Connecticut

url = "https://en.wikipedia.org/wiki/New_England_city_and_town_area"
necta <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()
necta_unnest <- necta[[1]]

necta_unnest %>% 
  select(area = NECTA, state = `State(s)`) %>% 
  filter(grepl('CT|MA', state)) -> necta_unnest

write.csv(necta_unnest, file = '../Data/necta_table.csv')


## MSA's:
## New York
url = "https://en.wikipedia.org/wiki/List_of_metropolitan_statistical_areas"
msa <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
msa_unnest <- msa[[1]]

msa_unnest %>% 
  select(msa = 2) %>% 
  filter(grepl('NY', msa)) -> msa_unnest
l.msa <- strsplit(msa_unnest$msa, ",")
areas = lapply(l.msa, function(x){ x[[1]] })
msa_ny <- data.frame(area = unlist(areas),
                     state = "NY")

write.csv(msa_ny, file = '../Data/msa_ny.csv')


necta
msa_ny
