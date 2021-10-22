library(tidyverse)
library(caret)

ks_cleaned <- readRDS(file = "dataset/ks_cleaned.rds")

data <- ks_cleaned[, c("restate", "category", "main_category", "usd_goal_real", "launch_period")]

set.seed(123)

training.samples <- data$restate %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

fit <- glm(relevel(as.factor(train.data$restate), ref = "failed")~category+main_category+usd_goal_real+as.numeric(launch_period), data = train.data, family = binomial)
summary(fit)

library(MASS)
stepAIC(fit, direction = "backward")


tab2_glm <- fit

fit2 <- stripGlmLR(tab2_glm)

saveRDS(test.data, "test_data.rds")
saveRDS(fit2, "tab2_glm_compressed.rds")

probabilities <- fit %>% predict(test.data[, c("category", "main_category", "usd_goal_real", "launch_period")])
predicted.classes <- ifelse(probabilities > 0.2, " successful", "failed")
mean(predicted.classes == test.data$restate)

cm <- confusionMatrix(data=as.factor(predicted.classes) , reference = as.factor(test.data$restate))

#----

kickstarter_rf <- train(as.factor(restate)~category+main_category+usd_goal_real+as.numeric(launch_period), data=train.data, method = "rf")
probabilities <- kickstarter_rf %>% predict(test.data[, c("category", "main_category", "usd_goal_real", "launch_period")], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, " successful", "failed")
mean(predicted.classes == test.data$restate)