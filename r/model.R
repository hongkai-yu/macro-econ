library(tidyverse)
library(lubridate)
library(glue)
library(glmnet)
library(randomForest)
library(caret)
library(furrr)

source('r/util.R')

df = read_csv(glue('{DATA_FOLDER}/processed/labelled_data.csv')) %>%
    select(-starts_with('real_price_f_'),
           -starts_with('sp_price_f_'),
           -starts_with('real_price'),
           -all_of(c("year", "quarter", "real_gdp", "cpi", "real_price", "sp_price")))

df$bubble = as.factor(df$bubble) # as factor

set.seed(0827)
ml_fitter = rf_fitter
ml_predictor = rf_predictor

# rf_fitter(df %>% filter(date < ymd("2000-01-01")))
# model_back_test_one_time(df, ml_fitter, ml_predictor, "1950/01/01")
ml_predictions = model_historical_predict(df, ml_fitter, ml_predictor)
ml_predictions %>%
    transmute(date, bubble, prediction, scores, threshold, f_3m = lead(sp_price_d_3m)) %>%
    View

confusionMatrix(as.factor(ml_predictions$prediction), as.factor(ml_predictions$bubble))

simulated_res_ml = ml_predictions %>%
    mutate(action = ifelse(prediction == 1, 0, 1)) %>%
    trade_simulate

simulated_res_ml %>%
    # filter(date <= "1950/01/01") %>%
    # filter(date >= "1970/01/01") %>%
    # filter(date <= "1990/01/01") %>%
    # filter(date <= "2000/01/01") %>%
    plot_simulation

simulated_res_buy_hold = df %>%
    filter(date >= ymd(quantile(df$date, .25, type = 1))) %>%
    mutate(action = 1) %>%
    trade_simulate

# simulated_res_buy_hold %>% View

simulated_res_buy_hold %>%
    # filter(date >= "1930/01/01") %>%
    # filter(date <= "1950/01/01") %>%
    filter(date >= "1970/01/01") %>%
    # filter(date <= "1990/01/01") %>%
    plot_simulation

ml_predictions %>%
    mutate(action = scores < 0.5) %>%
    trade_simulate %>%
    filter(date >= "1950/01/01") %>%
    plot_simulation

ml_predictions %>%
    transmute(date, scores, bubble, sp_price_d_3m, f_3m = lead(sp_price_d_3m)) %>%
    filter(scores > 0.5)

df %>% arrange(sp_price_d_3m) %>% View


simulated_res_buy_hold %>%
    select(date, exposed_return, cumulative_return) %>%
    inner_join(simulated_res_ml %>% select(date, exposed_return, cumulative_return),
               by = 'date', suffix = c('_buy_hold', '_ml')) %>% View
    mutate(return_diff = exposed_return_logit - exposed_return_buy_hold) %>% View
    ggplot(aes(x = date, y = return_diff)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    labs(title = 'Return difference of ml model over buy and hold',
         x = 'Date',
         y = 'Return Difference') +
    theme_minimal()

#################### Explore the model ####################

fitted = ml_fitter(df %>% filter(date < '2023-01-01'))
fitted$ml_model

sum(ml_predictions$prediction == 1)
ml_predictions %>% View

ml_predictions %>%
    ggplot(aes(x = date, y = scores)) +
    geom_line() +
    geom_hline(yintercept = 0.5, linetype = 'dashed')

evaluation_date = "2008-10-01"
df_train = df %>% filter(date < evaluation_date)
df_test = df %>% filter(date == evaluation_date)
model = ml_fitter(df_train)
ml_predictor(model, df_test) %>% View
