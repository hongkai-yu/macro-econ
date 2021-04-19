library(tidyverse)
library(randomForest)
library(caret)

set.seed(20210324)

quant_vec = c(0.005, 0.01, 0.015, 0.05)
periods_vec = c(3, 5, 6, 7, 12)

# sample function to test
add = function(x, y) {
  x + y
}

cv_rf_threshold = function(threshold, df_train) {
  rf.out = randomForest(bubble ~ .-Date, data = df_train,
                        cutoff = c(1 - threshold, threshold))
  
  cfm_train = rf.out$confusion
  (cfm_train[1,3] + cfm_train[2,3]) / 2
}

robust = function(quant, periods) {
  
  # different definitions
  threshold_crash = quantile(df$sp500_return, quant)
  
  df$crash = 0
  for (i in 1:nrow(df)) {
    if (df$sp500_return[i] < threshold_crash) {
      df$crash[i] = 1
    }
  }
  
  df$bubble = 0
  for (i in 1:nrow(df)) {
    if (df$crash[i] != 1 && (1 %in% df$crash[(i+1):(i+periods)])) {
      df$bubble[i] = 1
    }
  }
  
  df = df %>% select(-crash)
  
  df$bubble = as.factor(df$bubble)
  
  # train test split
  train_rate = 0.8
  train_label = sample.int(n = nrow(df), size = floor(train_rate * nrow(df)), replace = F)
  df_train = df[train_label, ]
  df_test = df[-train_label, ]
  
  # best model

  thresholds = seq(from = 0.01, to = 0.5, by = 0.01)
  balanced_error = rep(0, 49)
  
  for (i in 1:length(thresholds)) {
    balanced_error[i] = cv_rf_threshold(thresholds[i], df_train)
  }
  
  threshold = thresholds[which.min(balanced_error)]
  print(paste('Quantile:', quant, 'Period:', periods, 'Threshold:', threshold))
  plot(thresholds, balanced_error, type = 'l')
  
  rf.out = randomForest(bubble ~ .-Date, data = df_train, importance = TRUE,
                        cutoff = c(1 - threshold, threshold))
  
  df_test$rf.test = predict(rf.out, newdata = df_test, type = 'response')
  
  cfm = confusionMatrix(as.factor(ifelse(df_test$rf.test == 1, 'bubble', 'non-bubble')),
                        as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))
  
  cfm$byClass[11]
}

tab.rob = matrix(0, length(periods_vec), length(quant_vec))
colnames(tab.rob) = quant_vec
rownames(tab.rob) = periods_vec

tab.rob

for (i in 1:length(quant_vec)) {
  for (j in 1:length(periods_vec)) {
    tab.rob[j, i] = robust(quant = quant_vec[i], periods = periods_vec[j])
  }
}

tab.rob


# time based training data and testing data


quant = 0.01
periods = 6

threshold_crash = quantile(df$sp500_return, quant)

df$crash = 0
for (i in 1:nrow(df)) {
  if (df$sp500_return[i] < threshold_crash) {
    df$crash[i] = 1
  }
}

df$bubble = 0
for (i in 1:nrow(df)) {
  if (df$crash[i] != 1 && (1 %in% df$crash[(i+1):(i+periods)])) {
    df$bubble[i] = 1
  }
}

df = df %>% select(-crash)

df$bubble = as.factor(df$bubble)

# train test split
train_rate = 0.8
train_label = 1:(train_rate * nrow(df))
# train_label = sample.int(n = nrow(df), size = floor(train_rate * nrow(df)), replace = F)
df_train = df[train_label, ]
df_test = df[-train_label, ]

# best model

thresholds = seq(from = 0.01, to = 0.5, by = 0.01)
balanced_error = rep(0, 49)

for (i in 1:length(thresholds)) {
  balanced_error[i] = cv_rf_threshold(thresholds[i], df_train)
}

threshold = thresholds[which.min(balanced_error)]
threshold = 0.3
print(paste('Quantile:', quant, 'Period:', periods, 'Threshold:', threshold))
plot(thresholds, balanced_error, type = 'l')

rf.out = randomForest(bubble ~ .-Date, data = df_train, importance = TRUE,
                      cutoff = c(1 - threshold, threshold))
rf.out$confusion

df_test$rf.test = predict(rf.out, newdata = df_test, type = 'response')

cfm = confusionMatrix(as.factor(ifelse(df_test$rf.test == 1, 'bubble', 'non-bubble')),
                      as.factor(ifelse(df_test$bubble == 1, 'bubble', 'non-bubble')))

cfm
cfm$byClass[11]

