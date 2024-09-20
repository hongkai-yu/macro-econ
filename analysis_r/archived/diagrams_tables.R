library(tidyverse)
library(stargazer)
library(xtable)
library(Hmisc)
library(reporttools)
library(janitor)
library(ggrepel)

df_display = df %>% 
  rename('10-year T-bill yield (\\%)' = tbill_yield,
         'Annualized GDP Growth Rate (\\%)' = real_gdp_growth,
         'Annualized Inflation (\\%)' = inflation,
         'Shiller P/E Ratio' = shiller_pe,
         'Consumer Confidence Index' = consumer_confidence,
         'Market Capitalization-to-GDP Ratio' = mktcap_gdp_ratio,
         '1-Month S\\&P 500 Return (\\%)' = sp500_return,
         '3-Month S\\&P 500 Return (\\%)' = sp500_re3,
         '6-Month S\\&P 500 Return (\\%)' = sp500_re6,
         '12-Month S\\&P 500 Return (\\%)' = sp500_re12,
         '60-Month S\\&P 500 Return (\\%)' = sp500_re60,
         'Bubble' = 'bubble'
         )
  names

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
  print(file = '../../thesis/tab_res.tex')

# input variables summary
df_display %>% 
  select(-Bubble, -Date) %>%
  tableContinuous(cap = 'Summary of features', lab = 'tab:features', 
                  stats = list('min', 'q1', 'median', 'q3', 'max', 'mean', 's'),
                  longtable = FALSE, font.size = 'normalsize') %>% 
  capture.output(file = '../../thesis/tab_features.tex')

# Imbalance bubble (output variables summary)
df_display %>% select(Bubble) %>% 
  tableNominal(cap = 'Bubble distribution', lab = 'tab:imbalance',
               longtable = FALSE, font.size = 'normalsize') %>% 
  capture.output(file = '../../thesis/tab_imbalance.tex')
  
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
ggsave('../../figures/bubble_def.png')


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
ggsave('../../figures/cv_threshold.png')


# logit RW regression
stargazer(logit_balance.out, out = '../../thesis/tab_logitRW.tex', label = 'tab:logit')

# variance important RF-CV
png(filename = '../../figures/varImp.png')
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
ggsave('../../figures/rfcv_full.png')

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
ggsave('../../figures/rnn.png')

# result graph
tab.res %>% data.frame %>% 
  mutate(Specificity = as.numeric(Specificity),
         Sensitivity = as.numeric(Sensitivity)) %>% 
  ggplot(aes(x = Sensitivity, y = Specificity)) +
  geom_point(size = 10, shape = 18, alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) + 
  geom_label_repel(label = Model, size = 5) +
  labs(title = 'Sensitivity and Specificity by Model') +
  th
ggsave('../../figures/res.png')

# robustness analysis
colnames(tab.rob) = c('0.5 percentile', '1 percentile', '1.5 percentile', '5 percentile')
rownames(tab.rob) = c('3 months', '4 months', '6 months', '7 months', '12 months')
xtable(tab.rob, label = 'tab:rob', 
       caption = 'Balanced accuracy of RF-CV with different percentiles and periods') %>% 
  print(file = '../../thesis/tab_rob.tex')

# cfm
xtable(rf.cv.cfm$table, label = 'tab:cfm',
       caption = 'Confusion matrix of the RF-CV model, reference(columns) and prediction(rows)') %>% 
  print(file = '../../thesis/tab_cfm.tex')

# overfit 
1 - (rf.out$confusion[1,3] + rf.out$confusion[2,3]) / 2
rf.cv.cfm$byClass[11]
1 - (rf_balance.out$confusion[1,3] + rf_balance.out$confusion[2,3]) / 2
rf.rw.cfm$byClass[11]

