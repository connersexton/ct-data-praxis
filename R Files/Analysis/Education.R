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

full_df %>% 
  ggplot(aes(x = y_adult_col_high, y = gdp.pcap))+
  geom_point()

##
my.lm<-lm(gdp.pcap~y_adult_col_high, data = full_df)
summary(my.lm)
my.lm<-lm(gdp.pcap~y_adult_col_high+adult_col_high+age_one+age_two+age_three, data = full_df)
summary(my.lm)



df %>% 
  mutate(area = ifelse(NAME %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       "Not CT", "CT")) %>% 
  select(NAME, area, year, y_adult_col_high, adult_col_high) %>% 
  gather("variable", "value", -NAME, -year, -area) %>% 
  mutate(value = value/100,
         variable = ifelse(variable == "adult_col_high", "Adult (25+) w/ College +", "Young Adult (18-24) w/ College +")) %>% 
  ggplot(aes(x = as.character(year), y = value, group = NAME)) +
  geom_line(aes(color = NAME, linetype = area), size = 0.8) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_y_continuous(labels = scales::percent) +
  guides(linetype = FALSE) +
  facet_grid(variable~., scales = "free") +
  theme_bw()+
  labs(x = "Year",
       y = "Percent of Population",
       color = "MSA",
       title = "Educational Attainment Percentages",
       subtitle = "young adults and adults with college or more")

df %>% 
  mutate(area = ifelse(NAME %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       "Not CT", "CT")) %>% 
  select(NAME, area, year, y_adult_col_high, adult_col_high) %>% 
  gather("variable", "value", -NAME, -year, -area) %>% 
  mutate(value = value/100,
         variable = ifelse(variable == "adult_col_high", "Adult (25+) w/ College +", "Young Adult (18-24) w/ College +")) %>% 
  group_by(NAME, variable) %>% 
  summarise(mean = mean(value))
