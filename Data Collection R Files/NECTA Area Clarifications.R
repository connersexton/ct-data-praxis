######
## 22 April 2020
## Clarifying text decrepencies between NECTA's from Wiki and BLS
######

setwd("~/Documents/GitHub/ct-data-praxis/Data Collection R Files")
library(rvest)
library(tidyr)
library(dplyr)

url = "https://www.bls.gov/opub/ee/2019/sae/annavg1_2018.htm"
p = paste0("//*[@id='saaa_table1a']")
table <- url %>%
  read_html() %>%
  html_nodes(xpath = p) %>%
  html_table(fill = T)

table <- table[[1]]
names(table) <- c("area",
                  paste0(2016:2018, "var1"),
                  paste0(2016:2018, "var2"),
                  paste0(2016:2018, "var3"))

table$tbl_id <- cumsum(!nzchar(table$area))
states = c("New York", "Massachusetts", "Connecticut")
for (q in 1:length(states)) {
  x = table$tbl_id[table$area == states[q]]
  table$tbl_id[table$tbl_id == x] = states[q]
}

`%notin%` <- Negate(`%in%`)
table %>% 
  filter(tbl_id %in% states & nzchar(area)) %>% 
  select(area, tbl_id) %>% 
  mutate(area = gsub("\\([0-9]{1}\\)", "", area)) %>% 
  filter(area %notin% states) -> necta_clarifications

write.csv(necta_clarifications, file = "../Data/area_state_pairs.csv")

