######
## 22 April 2020
## GDP per Capita Computation and Exploration
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
graph_data %>% 
  mutate(msa = as.character(msa)) %>% 
  mutate(area = ifelse(msa %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       "Not CT", "CT")) %>% 
  ggplot(aes(x = year, y = r.gdp.per.cap, group = msa)) +
    geom_line(aes(color = msa, linetype = area), size = 0.8) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_y_continuous(labels = scales::dollar)+
    guides(linetype = FALSE) +         
    labs(x = "Year",
         y = "Real GDP Per Capita",
         title = "Real GDP Per Capita 2010-2018",
         subtitle = "Connecticut MSA's compared to Boston and New York Metro Areas",
         color = "MSA") +
    theme_bw()

### Percentage Change:
graph_data %>% 
  mutate(msa = as.character(msa)) %>% 
  group_by(msa) %>% 
  arrange(msa, year) %>% 
  mutate(pct.change = ((r.gdp.per.cap - lag(r.gdp.per.cap))/lag(r.gdp.per.cap))*100) %>% 
  ungroup() %>% 
  mutate(area = ifelse(msa %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       msa, "Connecticut")) %>% 
  group_by(area, year) %>% 
  summarise(pct.change = mean(pct.change)) %>%
  ungroup() %>%
  filter(year != "2010") %>% 
  ggplot(aes(x = year, y = pct.change, group = area)) +
  geom_line(aes(color = area), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(-6.5,5)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent Change",
       title = "Percent Change in Real GDP Per Capita 2010-2018",
       subtitle = "Connecticut compared to Boston and New York Metro Areas",
       color = "Area")

