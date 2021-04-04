library(tidyverse)
library(glue)
library(glmnet)
library(randomForest)
library(caret)

df$bubble = as.factor(df$bubble)

# pred_df = tibble(df$Date, df$bubble, name)
# names(pred_df) = c('Date', 'bubble')

set.seed(20210324)
train_rate = 0.8
train_label = sample.int(n = nrow(df), size = floor(train_rate * nrow(df)), replace = F)
df_train = df[train_label, ]
df_test = df[-train_label, ]

ratio = nrow(df_train %>% filter(bubble == 0)) / nrow(df_train %>% filter(bubble == 1))
prevalance = nrow(df_train %>% filter(bubble == 1)) / nrow(df_train)


# Logistic regression
logit.out = glm(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
               consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
             data = df_train,
             family = binomial(link = 'logit'))
summary(logit.out)

df_test$logit.test = predict(logit.out, newdata = df_test, type = 'response')

df_test$logit.test_decision = ifelse(df_test$logit.test > prevalance, 1, 0)

confusionMatrix(as.factor(ifelse(df_test$logit.test_decision == 1, 'bubble', 'non-bubble')),
                as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))



# Not important
df_test %>% filter(bubble == 1) %>% select(Date, logit.test) %>% summary
df_test %>% filter(bubble == 0) %>% select(Date, logit.test) %>% summary
filter(df_test, bubble == 0)$logit.test %>% mean

ggplot(data = pred_df, aes(x = Date, y = logfit)) + 
  geom_line() +
  geom_point(aes(y = bubble), col = 'red', size = 0.1)


# Logistic Regression with L2 penalty
X_train = model.matrix(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
           consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
          data = df_train)
X_test = model.matrix(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
           consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
          data = df_test)
logitL2.out = cv.glmnet(X_train, df_train$bubble, alpha = 0, family = 'binomial')
logitL2.out$lambda.min
coef(logitL2.out, s = 'lambda.min')
plot(logitL2.out) # no over-fitting problem


# Random Forest
cv_rf_threshold = function(threshold) {
  rf.out = randomForest(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
                      consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
                    data = df_train, cutoff = c(1 - threshold, threshold), importance = TRUE)
  
  cfm_train = rf.out$confusion
  (cfm_train[1,3] + cfm_train[2,3]) / 2
}

thresholds = seq(from = 0.01, to = 0.5, by = 0.01)
balanced_error = rep(0, 49)

for (i in 1:length(thresholds)) {
  balanced_error[i] = cv_threshold(thresholds[i])
}
plot(thresholds, balanced_error, type = 'l')

threshold = thresholds[which.min(balanced_error)]

rf.out = randomForest(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
                        consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
                      data = df_train, cutoff = c(1 - threshold, threshold), importance = TRUE)
rf$confusion

importance(rf.out)


df_test$rf.test = predict(rf.out, newdata = df_test, type = 'response')

confusionMatrix(as.factor(ifelse(df_test$rf.test == 1, 'bubble', 'non-bubble')),
                as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

# ggplot(data = df, aes(x = Date, y = rffit)) + 
#   geom_point() +
#   geom_point(aes(y = bubble), col = 'red', size = 0.1)


# Random Forest, Balanced data set
df_train_bubble = df_train %>% filter(bubble == 1)
df_train_bubble %>% nrow

df_train_balanced = bind_rows(df_train,
                              df_train_bubble[rep(seq_len(nrow(df_train_bubble)), each = floor(ratio) - 1),])
summary(df_train_balanced$bubble)

rf_balance.out = randomForest(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
                        consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
                        data = df_train_balanced, importance = TRUE)

rf_balance.out$confusion
df_test$rf_balance.test = predict(rf_balance.out, newdata = df_test, type = 'response')

confusionMatrix(as.factor(ifelse(df_test$rf_balance.test == 1, 'bubble', 'non-bubble')),
                as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

# Logistic regression, balanced
logit_balance.out = glm(bubble ~ real_gdp_growth + inflation + tbill_yield + shiller_pe +
                  consumer_confidence + mktcap_gdp_ratio + sp500_return + sp500_re3 + sp500_re6 + sp500_re12 + sp500_re60,
                data = df_train_balanced,
                family = binomial(link = 'logit'))
summary(logit_balance.out)

df_test$logit_balance.test = predict(logit_balance.out, newdata = df_test, type = 'response')

df_test$logit_balance.test_decision = ifelse(df_test$logit_balance.test > 0.5, 1, 0)

confusionMatrix(as.factor(ifelse(df_test$logit_balance.test_decision == 1, 'bubble', 'non-bubble')),
                as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

