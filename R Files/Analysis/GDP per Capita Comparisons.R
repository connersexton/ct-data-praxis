######
## 22 April 2020
## Creating CBSA Guide from U.S. Census Guidelines
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(DataCombine)

gdp = read.csv("../../Data/BEA Data/real_gdp_msa.csv")[,-1]
pop = read.csv("../../Data/Census Data/population.csv")[,-1]

pop %>% 
  select(CBSA, year, POPESTIMATE) %>% 
  filter(year != 2019) %>% 
  mutate(year = as.character(year))-> pop.sub

gdp %>% 
  select(-CL_UNIT) %>% 
  mutate_at(paste0("GDP_", 2010:2018), function(x){x*1000}) %>% 
  gather("variable", "value", -GeoFips, -GeoName) %>% 
  mutate(year = str_extract(variable, "[0-9]{4}$"),
         variable = NULL,
         year = as.character(year)) %>% 
  left_join(pop.sub, by = c("GeoFips" = "CBSA", "year" = "year")) %>% 
  mutate(r.gdp.per.cap = value/POPESTIMATE) %>% 
  select(msa = GeoName, year, r.gdp.per.cap) -> graph_data

## Real GDP Per Capita:
ggplot(data = graph_data, aes(x = year, y = r.gdp.per.cap, group = msa)) +
  geom_line(aes(color = msa))

### Percentage Change:
graph_data %>% 
  group_by(msa) %>% 
  arrange(msa, year) %>% 
  mutate(pct.change = ((r.gdp.per.cap - lag(r.gdp.per.cap))/lag(r.gdp.per.cap))*100) %>% 
  ggplot(aes(x = year, y = pct.change, group = msa)) +
  geom_line(aes(color = msa))

