library(tidyverse)
library(glue)
library(glmnet)
library(randomForest)
library(caret)

View(df_test)

# as factor
df$bubble = as.factor(df$bubble)

# train test split
set.seed(20210324)
train_rate = 0.8
train_label = sample.int(n = nrow(df), size = floor(train_rate * nrow(df)), replace = F)
df_train = df[train_label, ]
df_test = df[-train_label, ]

# the data set is imbalance
prevalence = nrow(df_train %>% filter(bubble == 1)) / nrow(df_train)

ratio = nrow(df_train %>% filter(bubble == 0)) / nrow(df_train %>% filter(bubble == 1))
df_train_bubble = df_train %>% filter(bubble == 1)
df_train_bubble %>% nrow

df_train_balanced = bind_rows(df_train, df_train_bubble[rep(seq_len(nrow(df_train_bubble)), each = round(ratio) - 1),])
summary(df_train_balanced$bubble)

# Logistic regression, threshold as prevalence
logit.out = glm(bubble ~ .-Date, data = df_train,
             family = binomial(link = 'logit'))
summary(logit.out)

df_test$logit.test = predict(logit.out, newdata = df_test, type = 'response')

df_test$logit.test_decision = ifelse(df_test$logit.test > prevalence, 1, 0)

logit.p.cfm = confusionMatrix(as.factor(ifelse(df_test$logit.test_decision == 1, 'bubble', 'non-bubble')),
                              as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

# # Logistic Regression with L2 penalty - overfitting?
# X_train = model.matrix(bubble ~ .-Date, data = df_train)
# X_test = model.matrix(bubble ~ .-Date, data = df_test)
# logitL2.out = cv.glmnet(X_train, df_train$bubble, alpha = 0, family = 'binomial')
# logitL2.out$lambda.min
# coef(logitL2.out, s = 'lambda.min')
# plot(logitL2.out) # no over-fitting problem


# Random Forest, CV-tuned cutoff
cv_threshold = function(threshold) {
  rf.out = randomForest(bubble ~ .-Date, data = df_train,
                        cutoff = c(1 - threshold, threshold))
  
  cfm_train = rf.out$confusion
  (cfm_train[1,3] + cfm_train[2,3]) / 2
}

thresholds = seq(from = 0.01, to = 0.5, by = 0.01)
balanced_error = rep(0, 49)

for (i in 1:length(thresholds)) {
  balanced_error[i] = cv_threshold(thresholds[i])
}

threshold = thresholds[which.min(balanced_error)]
threshold

plot(thresholds, balanced_error, type = 'l')

rf.out = randomForest(bubble ~ .-Date, data = df_train, importance = TRUE,
                      cutoff = c(1 - threshold, threshold))
rf$confusion

df_test$rf.test = predict(rf.out, newdata = df_test, type = 'response')

rf.cv.cfm = confusionMatrix(as.factor(ifelse(df_test$rf.test == 1, 'bubble', 'non-bubble')),
                            as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

# Random Forest, Balanced data set
rf_balance.out = randomForest(bubble ~ .-Date, data = df_train_balanced, importance = TRUE)
rf_balance.out$confusion

df_test$rf_balance.test = predict(rf_balance.out, newdata = df_test, type = 'response')
rf.rw.cfm = confusionMatrix(as.factor(ifelse(df_test$rf_balance.test == 1, 'bubble', 'non-bubble')),
                            as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

# Logistic regression, balanced
logit_balance.out = glm(bubble ~ .-Date, data = df_train_balanced, family = binomial(link = 'logit'))
summary(logit_balance.out)

df_test$logit_balance.test = predict(logit_balance.out, newdata = df_test, type = 'response')

df_test$logit_balance.test_decision = ifelse(df_test$logit_balance.test > 0.5, 1, 0)

logit.rw.cfm = confusionMatrix(as.factor(ifelse(df_test$logit_balance.test_decision == 1, 'bubble', 'non-bubble')),
                               as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

rf.cv.cfm
