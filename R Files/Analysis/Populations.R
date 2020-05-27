######
## 26 May 2020
## Populations
######

setwd("~/Documents/GitHub/ct-data-praxis/R Files/Analysis")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

df <- read.csv("../../Data/joined_data.csv")

### year to year net migration:
df %>% 
  mutate(area = ifelse(NAME %in% c("New York-Newark-Jersey City", "Boston-Cambridge-Newton"),
                       "Not CT", "CT")) %>% 
  ggplot(aes(x = as.character(year), y = net.mig, group = NAME)) +
    geom_line(aes(color = NAME, linetype = area), size = 0.8) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_y_continuous(labels = scales::percent)+
    guides(linetype = FALSE) +     
    labs(x = "Year",
         y = "Net Migration (as percent of population)",
         title = "Net Migration Across CT MSAs",
         subtitle = "compared to Boston and New York metro areas",
         color = "MSA")+
    theme_bw()
