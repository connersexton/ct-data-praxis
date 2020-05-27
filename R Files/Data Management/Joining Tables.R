setwd("~/Documents/GitHub/ct-data-praxis/R Files/Data Management")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

### Load Data:
# Population:
population <- read.csv("../../Data/Census Data/population.csv")
population %>% 
  mutate(pop.change.p = NPOPCHG/POPESTIMATE,
         dom.mig = DOMESTICMIG/POPESTIMATE,
         net.mig = NETMIG/POPESTIMATE) %>%
  select(CBSA, NAME, year, pop.change.p, dom.mig, net.mig, population = POPESTIMATE) -> pop

# Ages:
age_brackets <- read.csv("../../Data/Census Data/msa_age_brackets.csv")[,-1]

# Income Info:
income_brackets <- read.csv("../../Data/Census Data/income_info.csv")
income_brackets %>% 
  filter(type == "family") %>% 
  select(-X, -type) %>% 
  spread(new_var, value) -> income

# Education:
education <- read.csv("../../Data/Census Data/education_info.csv")[,-1] %>% 
  select(-name)

# Employment:
employment <- read.csv("../../Data/Census Data/emp_industries_p.csv") %>% 
  select(-1, -total_emp)

# GDP Data:
gdp <- read.csv("../../Data/BEA Data/real_gdp_msa.csv") 
gdp %>% 
  select(-X, -CL_UNIT, -GeoName) %>% 
  gather("variable", "value", -GeoFips) %>% 
  mutate(year = as.integer(sub("GDP_", "", variable))) %>% 
  left_join(pop %>% select(CBSA, year, population), by = c("GeoFips" = "CBSA", "year")) %>% 
  mutate(gdp.pcap = value*1000/population) %>% 
  select(cbsa = GeoFips, year, gdp.pcap)-> gdp_info

###########
# JOIN DATA:

## GDP data doesn't include Torrington (Micro-Area)
## It's removed in the example below:

pop %>% 
  left_join(age_brackets, by = c("CBSA" = "geo_id", "year")) %>% 
  left_join(income, c("CBSA" = "geo_id", "year")) %>% 
  left_join(education, by = c("CBSA" = "geo_id", "year")) %>% 
  left_join(employment %>% select(-name), by = c("CBSA" = "geo_id", "year")) %>% 
  left_join(gdp_info, by = c("CBSA" = "cbsa", "year")) %>% 
  na.omit() -> joined_df

joined_df
write.csv(joined_df, file = "../../Data/joined_data.csv")
