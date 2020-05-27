######
## 7 May 2020
## ML Modeling
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Analysis")

df <- read.csv("../../Data/joined_data.csv")

library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(scales)

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


set.seed(12345)
index <- createDataPartition(full_df$gdp.pcap, p = .7, list = FALSE)

train <- full_df[index, ]
test <- full_df[-index, ]

### Random Forest:
set.seed(12345)
fit.forest <- randomForest(gdp.pcap ~ .-NAME-CBSA-X, data = train,
                           ntree = 100,
                           importance = TRUE)
fit.forest
pred.forest <- predict(fit.forest, test)
postResample(pred = pred.forest, obs = test$gdp.pcap)
# $4,929.68 R-squared - 0.933

varImpPlot(fit.forest, type=2, main="Variable Importance")

### Feature Importance
imp <- fit.forest$importance
importance.data <- data.frame(variable = rownames(imp),
                              p.increase.mse = imp[,1],
                              incnodepurity = imp[,2],
                              row.names = 1:nrow(imp))

importance.data %>% 
  ggplot(aes(x = reorder(variable, incnodepurity), y = incnodepurity)) +
  geom_segment(aes(xend = variable, yend = 0)) +
  geom_point( size=2, color="orange") +
  coord_flip() +
  theme_bw() +
  labs(y = "Increase in Node Purity",
       x = "Variable",
       title = "Random Forest Feature Importance") 


naics_labels <- read.csv("../../Data/naics_guide.csv")
### Top 10 most important features:
importance.data %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(variable = gsub("^n\\_","",as.character(variable))) %>% 
  mutate(variable = ifelse(grepl("[0-9]+\\.", variable), gsub("\\.", "-", variable), variable)) %>% 
  left_join(naics_labels %>% select(-X) %>% mutate(NAICS = as.character(NAICS)), by = c("variable" = "NAICS")) %>% 
  mutate(NAICSDSCR = ifelse(is.na(as.character(NAICSDSCR)), variable, as.character(NAICSDSCR))) %>%
  arrange(desc(incnodepurity)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = reorder(NAICSDSCR, incnodepurity), y = incnodepurity)) +
  geom_segment(aes(xend = NAICSDSCR, yend = 0)) +
  geom_point( size=2, color="orange") +
  coord_flip() +
  theme_bw() +
  labs(y = "Increase in Node Purity",
       x = "",
       title = "Random Forest Feature Importance",
       subtitle = "Top 20 Features") 

importance.data %>% 
  arrange(desc(incnodepurity)) %>% 
  slice(1:20) %>% 
  mutate(variable = as.character(variable)) %>% pull(variable)-> top20.features

my.lm <- lm(gdp.pcap ~ ., data = full_df[,c("gdp.pcap", top20.features)])
summary(my.lm)


trctrl <- trainControl(method="cv", 
                       number=10, 
                       selectionFunction="oneSE")
set.seed(1234)
rtree_fit <- train(gdp.pcap ~., data = full_df[,c("gdp.pcap", top20.features)], 
                   method = "rpart",
                   trControl=trctrl,
                   metric = "RMSE",
                   tuneLength = 10)
rtree_fit
library(rattle)
library(rpart.plot)
fancyRpartPlot(rtree_fit$finalModel)
rpart.plot(rtree_fit$finalModel)
# $5,295.85 R-square: 0.866

