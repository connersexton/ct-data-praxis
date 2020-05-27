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
  mutate(age_category = factor(age_category,
                               levels = c("age_one", "age_two", "age_three", "age_four", "age_five"),
                               labels = c("19 and below", "20 - 34", "35 - 54", "55 - 64", "65 - 84"),
                               ordered = T)) %>% 
  mutate(state = ifelse(state == "Massachusetts", "Boston", state)) %>% 
  ggplot(aes(x = year, y = percent, group = age_category))+
  geom_line(aes(color = age_category), size = 1) +
  scale_color_brewer(palette="Set2")+
  facet_grid(.~state)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year", y = "Percent", color = "Age Category",
       title = "Age Distributions in Connecticut\ncompared to New York and Boston Metro Areas")

age_long %>% 
  mutate(age_category = factor(age_category,
                               levels = c("age_one", "age_two", "age_three", "age_four", "age_five"),
                               labels = c("age_one", "age_two", "age_three", "age_four", "age_five"),
                               ordered = T)) %>% 
  ggplot(aes(x = year, y = percent, group = age_category))+
  geom_line(aes(color = age_category), size = 1, alpha = 0.6) +
  facet_grid(.~cbsa.title)+
  theme_bw()

## You would want a higher population of group 1 and 2, the highest are noticibly
## the New York and Boston, respectively. 



### AGE across MSA's


