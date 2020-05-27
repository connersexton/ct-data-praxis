######
## 17 May 2020
## Primary Research Question Explorations
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Analysis")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

### Areas of focus:
# relalationship of following factors to an MSA's GDP:

# Population age/education
# Population migration
# Ammenities
# Establishments (+ sizes)

df <- read.csv("../../Data/joined_data.csv")

naics <- read.csv("../../Data/Census Data/Firm Entries and Exits/firm_estab_naics_wide.csv")
naics %>% 
  filter(variable == "ESTB") %>% 
  select(-X, -variable) -> NAICS

NAICS[,-unname(which(colSums(is.na(NAICS))>0))] -> NAICS

df %>% 
  left_join(NAICS, by = c("CBSA" = "MSA", "year")) %>% 
  na.omit() -> full_df


## Amenities:
amenities <- read.csv("../../Data/naics_guide_labeled.csv")

full_df %>% 
  gather("variable", "value", -X, -NAME, -year) %>% 
  filter(grepl("^n\\_[0-9]+.*", variable)) %>% 
  mutate(variable = ifelse(grepl("^n\\_[0-9]+\\.", variable), gsub("\\.", "-", variable), variable)) %>% 
  left_join(amenities %>% select(-X) %>% mutate(NAICS = paste0("n_", as.character(NAICS))), by = c("variable"="NAICS")) %>% 
  filter(Label == "Amenity") %>% 
  group_by(NAME, year, Label) %>% 
  summarise(p = sum(value)) %>% 
  left_join(full_df %>% select(NAME, year, gdp.pcap), by = c("NAME", "year")) %>% 
  ggplot(aes(x = p, y = gdp.pcap, color = NAME))+
  geom_point() +
  theme_bw()

### Establishment Sizes:

estab_sizes <- read.csv("../../Data/Census Data/Firm Entries and Exits/firm_estab_totals.csv")
estab_sizes %>% 
  left_join(estab_sizes %>% filter(ENTRSIZE == 1) %>% select(MSA, year, total = ESTB), by = c("MSA", "year")) %>% 
  mutate(ESTB = ESTB/total) %>% 
  filter(ENTRSIZE != 1) %>% 
  mutate(ENTRSIZE = paste0("size_", ENTRSIZE)) %>% 
  select(MSA, year, ENTRSIZE, ESTB) %>% 
  spread("ENTRSIZE", "ESTB") -> sizes.wide

#write.csv(sizes.wide, "../../Data/Census Data/Firm Entries and Exits/estab_sizes_wide.csv")
## 2010 - 2016
estab_sizes %>% 
  left_join(estab_sizes %>% filter(ENTRSIZE == 1) %>% select(MSA, year, total = ESTB), by = c("MSA", "year")) %>% 
  mutate(ESTB = ESTB/total) %>% 
  filter(ENTRSIZE != 1) %>% 
  mutate(ENTRSIZE = paste0("size_", ENTRSIZE)) %>% 
  select(MSA, year, ENTRSIZE, ESTB) %>% 
  left_join(df %>% select(MSA = CBSA, NAME, year, gdp.pcap), by = c("MSA", "year")) %>% 
  na.omit() %>% 
  group_by(NAME, ENTRSIZE) %>% 
  summarise(mean.estb.p = mean(ESTB)) %>% 
  mutate(ENTRSIZE = factor(ENTRSIZE, levels = paste0("size_",2:9), labels = c("0-4", "5-9", "10-19",
                                                                    "<20", "20-99", "100-499",
                                                                    "<500", "500+"))) %>% 
  ggplot(aes(x = NAME, y = mean.estb.p, fill = NAME)) +
  geom_histogram(stat = "identity")+
  facet_grid(ENTRSIZE~.)+
  theme_bw()+
  labs(fill = "MSA",
       y = "Percentage of Establishments",
       x = "")+
  theme(axis.text.x=element_text(angle=-90, color="black",hjust=0,vjust=0.2))


estab_sizes %>% 
  left_join(estab_sizes %>% filter(ENTRSIZE == 1) %>% select(MSA, year, total = ESTB), by = c("MSA", "year")) %>% 
  mutate(ESTB = ESTB/total) %>% 
  filter(ENTRSIZE != 1) %>% 
  mutate(ENTRSIZE = paste0("size_", ENTRSIZE)) %>% 
  select(MSA, year, ENTRSIZE, ESTB) %>% 
  left_join(df %>% select(CBSA, year, gdp.pcap), by = c("MSA"="CBSA", "year")) %>% 
  na.omit() %>% 
  filter(ENTRSIZE == "size_4")->startups

cor.test(startups$gdp.pcap, startups$ESTB)
plot(startups$gdp.pcap, startups$ESTB)



