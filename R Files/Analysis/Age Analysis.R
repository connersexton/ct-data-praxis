######
## 22 April 2020
## Exploring Age Bracket Data
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")

library(dplyr)
library(readr)
library(ggplot2)

age_data <- read_csv("../../Data/Census Data/msa_age_brackets.csv")
msa_data <- read_csv("../../Data/cbsa_subset_table.csv")

age_data %>% 
  select(-X1) %>% 
  left_join(msa_data %>% select(-X1), by = c("geo_id" = "cbsa.code")) -> age_joined

age_joined %>% 
  select(cbsa.title, state, year, age_one, age_two, age_three, age_four, age_five) %>% 
  gather("age_category", "percent", -cbsa.title, -state, -year) -> age_long

age_long %>% 
  group_by(state, year, age_category) %>% 
  summarise(percent = mean(percent)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = percent, group = age_category))+
  geom_line(aes(color = age_category)) +
  facet_grid(.~state)

age_long %>% 
  ggplot(aes(x = year, y = percent, group = age_category))+
  geom_line(aes(color = age_category), size = 1, alpha = 0.6) +
  facet_grid(.~cbsa.title)+
  theme_bw()

## You would want a higher population of group 1 and 2, the highest are noticibly
## the New York and Boston, respectively. 