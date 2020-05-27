######
## 20 May 2020
## Amenity Analysis
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Analysis")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

df <- read.csv("../../Data/joined_data.csv")
naics <- read.csv("../../Data/Census Data/Firm Entries and Exits/firm_estab_naics_wide.csv")
naics %>% 
  filter(variable == "ESTB") %>% 
  select(-X, -variable) -> NAICS

NAICS[,-unname(which(colSums(is.na(NAICS))>0))] -> NAICS

df %>% 
  left_join(NAICS, by = c("CBSA" = "MSA", "year")) %>% 
  na.omit() -> full_df

## Amenity Subset:
full_df %>% 
  select(NAME, year, gdp.pcap, age_two, upper_class, y_adult_col_high, n_423, n_44.45, n_447, n_452, n_488,
         n_53, n_531, n_72, n_722) %>% 
  gather("variable", "value", -NAME, -year, -gdp.pcap, -age_two, -upper_class, -y_adult_col_high)->amenities


#Accommodation and Food Services:
cor.test(full_df$gdp.pcap, full_df$n_72)

#Real Estate and Rental and Leasing:
cor.test(full_df$gdp.pcap, full_df$n_53)
#Support Activities for Transportation:
cor.test(full_df$gdp.pcap, full_df$n_488)
#Securities, commodity contracts, and other financial activities:
cor.test(full_df$gdp.pcap, full_df$n_523)


## Facet Grid Plot:
full_df %>% 
  mutate(area = ifelse(NAME %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       "Not CT", "CT")) %>% 
  select(NAME, area, year, n_72, n_53, n_488, n_523, gdp.pcap) %>% 
  gather("variable", "value", -NAME, -year, -area, -gdp.pcap) %>% 
  group_by(NAME, variable) %>% 
  summarise(value = mean(value), gdp.pcap = mean(gdp.pcap)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable,
                           levels = c("n_72", "n_53", "n_488", "n_523"),
                           labels = c("Acommodation", "Real Estate", "Transportation", "Financial")),
         value = value*100) %>% 
  ggplot(aes(x = gdp.pcap, y = value))+
  geom_point(aes(color = NAME))+
  stat_smooth(method = "lm", color = "black", cex = 0.4, alpha = 0.2)+
  facet_grid(variable~., scales = "free")+
  theme_bw()+
  scale_x_continuous(labels = scales::dollar)+
  labs(x = "Real GDP per Capita",
       y = "Percent of all Establishments",
       color = "MSA",
       title = "Comparing Real GDP per Capita and\nPercent NAICS Establishments of Interest")
  
  
  

## VIZ
full_df %>% 
  mutate(area = ifelse(NAME %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       "Not CT", "CT")) %>% 
  select(NAME, area, year, n_523) %>% 
  gather("variable", "value", -NAME, -year, -area) %>% 
  mutate(value = value*100) %>% 
  ggplot(aes(x = as.character(year), y = value, group = NAME)) +
  geom_line(aes(color = NAME, linetype = area), size = 0.8) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  guides(linetype = FALSE) +
  theme_bw()+
  labs(x = "Year",
       y = "Percent of all Establishments",
       color = "MSA",
       title = "Securities and Other Financial Activities Establishment Percentages",
       subtitle = "across MSAs from 2010 to 2018")
