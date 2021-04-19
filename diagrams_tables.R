library(tidyverse)
library(stargazer)
library(xtable)
library(Hmisc)
library(reporttools)



# result table
Model = c('Logit-P', 'Logit-RW', 'RF-CV', 'RF-RW', 'RNN-BiLSTM-focal')

tab.res = cbind(Model,
                rbind(
                  logit.p.cfm$byClass[c(1,2,11)] %>% round(3),
                  logit.rw.cfm$byClass[c(1,2,11)] %>% round(3),
                  rf.cv.cfm$byClass[c(1,2,11)] %>% round(3),
                  rf.rw.cfm$byClass[c(1,2,11)] %>% round(3),
                  c(0, 1, 0.5)
                      )
                )
tab.res
xtable(tab.res, label = 'tab:res', caption = 'The peformance of models') %>%  
  print(file = './thesis/tab_res.tex')

# input variables summary
df %>% 
  select(-bubble, -Date) %>%
  tableContinuous(cap = 'Summary of features', lab = 'tab:features', 
                  stats = list('min', 'q1', 'median', 'q3', 'max', 'mean')) %>% 
  capture.output(file = './thesis/tab_features.tex')

# Imbalance bubble (output variables summary)
df %>% select(bubble) %>% 
  tableNominal(cap = 'Bubble distribution', lab = 'tab:imbalance') %>% 
  capture.output(file = './thesis/tab_imbalance.tex')
  
# plot theme
th = theme(
      # legend.position = 'bottom',
      title = element_text(size = 14),
      text = element_text(size = 12),
      legend.title = element_text(size = 12),
      plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'))

# Bubble
df %>% 
  ggplot(aes(x = Date, y = bubble)) +
  geom_point(size = 1) +
  labs(
    title = 'Bubbles, 1960 – 2020',
    x = 'Date',
    y = 'Bubble'
  ) +
  # geom_text(data = df %>% filter(bubble == 1), aes(label = year(Date))) + 
  scale_x_date(date_breaks = '8 years', date_labels = '%Y') + 
  th
ggsave('./figures/bubble_def.png')


# CV threshold
cv_threshold.output = data.frame(thresholds, balanced_error)
cv_threshold.output %>% 
  ggplot(aes(x = thresholds, y = balanced_error)) + 
  geom_line() + 
  labs(
    title = 'CV-tuned decision threshold for Random Forest',
    x = 'Threshold',
    y = 'Balanced error = 1 - Balanced accuracy'
  ) +
  geom_vline(aes(xintercept = 0.5), col = 'blue') +
  geom_vline(aes(xintercept = threshold), col = 'red') +
  geom_text(aes(x = threshold - 0.015, y = 0.3, label = threshold), col = 'red') +
  geom_text(aes(x = 0.49, y = 0.3, label = 0.5), col = 'blue') +
  th
ggsave('./figures/cv_threshold.png')


# logit RW regression
stargazer(logit_balance.out, out = './thesis/tab_logitRW.tex', label = 'tab:logit')

# variance important RF-CV
png(filename = './figures/varImp.png')
varImpPlot(rf.out, type = 1, main = 'Variable importance for Random Forest')
dev.off()

# Random Forest
rf_full.out = randomForest(bubble ~ .-Date, data = df, importance = TRUE,
                      cutoff = c(1 - threshold, threshold))
rf_df.forplot = data.frame(Date = df$Date, bubble = df$bubble,
                           bubble_hat = predict(rf_full.out, newdata = df, type = 'response'))
rf_df.forplot %>% 
  ggplot(aes(x = Date, y = bubble)) +
  geom_point() +
  geom_vline(data = rf_df.forplot %>% filter(bubble_hat == 1),
             aes(xintercept = Date), 
             col = 'red', alpha= 0.3) +
  labs(
    title = 'Actual bubbles (black) & RF-CV predicted (red)',
    x = 'Date',
    y = 'Bubble'
  ) +
  scale_x_date(date_breaks = '8 years', date_labels = '%Y') + 
  th
ggsave('./figures/rfcv_full.png')

# RNN prob

rnn = read_csv('rnn_prob_output.csv') %>% select(-X1)
rnn$reference = as.factor(ifelse(rnn$reference == 1, 'bubble', 'non-bubble'))

summary(rnn$p)

rnn %>% 
  ggplot(aes(x = p, group = reference, fill = reference)) +
  geom_density(alpha = .3) + 
  xlim(0, 1) + 
  labs(
    title = 'The density of probabilty output of RNN',
    x = 'Probability',
    y = 'Density'
  ) +
  th
ggsave('./figures/rnn.png')


# robustness analysis

xtable(tab.rob, label = 'tab:rob', 
       caption = 'Balanced accuracy of RF-CV with different quantiles (column) and periods (row)') %>% 
  print(file = './thesis/tab_rob.tex')
