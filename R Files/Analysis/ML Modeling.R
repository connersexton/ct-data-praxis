######
## 7 May 2020
## ML Modeling
######
setwd("~/Documents/GitHub/ct-data-praxis/R Files/Analysis")

df <- read.csv("../../Data/MLData/mldata.csv")

library(dplyr)
library(caret)
set.seed(12345)
index <- createDataPartition(df$gdp.pcap, p = .7, list = FALSE)

train <- df[index, ]
test <- df[-index, ]

### Random Forest:
library(randomForest)
set.seed(12345)
fit.forest <- randomForest(gdp.pcap ~ .-CBSA-NAME, data = train,
                           ntree = 100,
                           importance = TRUE)
fit.forest
pred.forest <- predict(fit.forest, test)
postResample(pred = pred.forest, obs = test$gdp.pcap)

varImpPlot(fit.forest, type=2, main="Variable Importance")

fit.forest$importance

## From here implement variable selection techniques...

my.lm <- lm(gdp.pcap ~ finance + adult_col_high + adult_hs + population + middle_class + upper_class + y_adult_hs, data = df)
summary(my.lm)
my.lm$coefficients
