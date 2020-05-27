######
## 7 May 2020
## KNN Cluster Analysis
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Analysis")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
df <- read.csv("../../Data/joined_data.csv")
cbsa.codes <- read.csv("../../Data/cbsa_subset_table.csv")
## Cluster Analysis:
library(factoextra)

naics <- read.csv("../../Data/Census Data/Firm Entries and Exits/firm_estab_naics_wide.csv")
naics %>% 
  filter(variable == "ESTB") %>% 
  select(-X, -variable) -> NAICS

NAICS[,-unname(which(colSums(is.na(NAICS))>0))] -> NAICS
estab.sizes <- read.csv("../../Data/Census Data/Firm Entries and Exits/estab_sizes_wide.csv")[,-1]

df %>% 
  left_join(NAICS, by = c("CBSA" = "MSA", "year")) %>% 
  left_join(estab.sizes, by = c("CBSA" = "MSA", "year")) %>% 
  na.omit() -> full_df

full_df %>% 
  group_by(CBSA) %>% 
  summarise_if(is.numeric, mean) %>% 
  select(CBSA, all_of(top20.features), gdp.pcap) -> df_summary

df_summary %>% 
  left_join(cbsa.codes %>% select(-X), by = c("CBSA" = "cbsa.code")) -> df_summary



rownames(df_summary) <- df_summary$cbsa.title
df_summary %>% select(-cbsa.title, -state) -> df_sum

df_st <- scale(df_sum[2:length(df_sum)])
rownames(df_st) <- df_summary$cbsa.title
set.seed(1234)
fit.km <- kmeans(df_st, 3, nstart = 25)
fit.km$size
fit.km$centers
# WSS (Between WSS/Total WSS): 59.9

# plot clusters (method 1)
fviz_cluster(fit.km, df_st,
             ellipse.type = "convex",#creates shaded regions for clusters
             palette = "Set2",#color choice using rcolorbrewer
             ggtheme = theme_minimal(),
             geom = "text", repel = T)#minimal theme
means <- as.data.frame(fit.km$centers)
means$cluster <- row.names(means)


library(tidyr)
dfm <- gather(means, key = "Variable", value = "Value", n_331:gdp.pcap)

naics_labels <- read.csv("../../Data/naics_guide.csv")

lvls = c("gdp.pcap", "Merchant wholesalers, durable goods", "Support activities for transportation", "Real estate", "Retail trade",
         "Real estate and rental and leasing", "Food services and drinking places", "Gasoline stations", "General merchandise stores",
         "Accommodation and food services", "finance", "Securities, commodity contracts, and other financial investments and related activities",
         "adult_col_high", "adult_hs", "y_adult_hs", "population", "Mining (except oil and gas)", "Paper manufacturing",
         "Petroleum and coal products manufacturing", "Publishing industries (except Internet)", "Primary metal manufacturing")


dfm %>% 
  mutate(Variable = as.character(Variable)) %>% 
  mutate(Variable = gsub("^n\\_","",as.character(Variable))) %>% 
  mutate(Variable = ifelse(grepl("[0-9]+\\.", Variable), gsub("\\.", "-", Variable), Variable)) %>% 
  left_join(naics_labels %>% select(-X) %>% mutate(NAICS = as.character(NAICS)), by = c("Variable" = "NAICS")) %>% 
  mutate(NAICSDSCR = ifelse(is.na(as.character(NAICSDSCR)), Variable, as.character(NAICSDSCR))) %>% 
  mutate(NAICSDSCR = factor(NAICSDSCR,
                            levels = lvls,
                            labels = lvls,
                            ordered = T)) %>% 
  ggplot(aes(x = NAICSDSCR, y = Value, group = cluster,
             color = cluster, shape = cluster)) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_manual(values=c("#7fc97f", "#fdc085", "#beaed4"))+
  #  facet_wrap(.~cluster) +
  labs(title = "K-Means Clusters",
       subtitle = "on top 10 most important factors",
       x = "Factor",
       y = "Scaled Value")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#7fc97f
#beaed4
#fdc085

library(ggplot2) 
ggplot(data = dfm,
       aes(x = Variable, y = Value, group = cluster,
           color = cluster, shape = cluster)) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_manual(values=c("#7fc97f", "#fdc085", "#beaed4"))+
#  facet_wrap(.~cluster) +
  labs(title = "K-Means Clusters",
       subtitle = "on top 10 most important factors",
       x = "Factor",
       y = "Scaled Value")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

df_summary$cluster = fit.km$cluster

df_summary %>% 
  left_join(cbsa.codes %>% select(-X), by = c("CBSA" = "cbsa.code")) -> df_summary
df_summary %>% select(cluster, cbsa.title)

