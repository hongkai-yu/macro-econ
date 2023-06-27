library(tidyverse)
library(lubridate)
library(glue)
library(glmnet)
library(randomForest)
library(caret)
library(furrr)

# ideas:
# add inverted yield curve
# add debt ratio, maybe financial derivatives
source('r/util.R')
setup()

df = read_csv(glue('{DATA_FOLDER}/processed/labelled_data.csv')) %>%
    select(-starts_with('real_price_f_'),
           -starts_with('sp_price_f_'),
           -starts_with('real_price'),
           -starts_with("gold_price"),
           -starts_with("real_gold_price"),
           -starts_with("gs10_d_"),
           -all_of(c("year", "quarter", "real_gdp", "cpi", "real_price", "sp_price", "inflation_diff_3m"))
    )
df$bubble = as.factor(df$bubble) # as factor

set.seed(0827)
ml_fitter = rf_fitter
ml_predictor = rf_predictor

df = df %>% filter(date < ymd("1940-01-01"))
dated_weights = ifelse(
    last(dff$date) - dff$date <= years(25),
    3,
    -1
)

rf_model = randomForest(bubble ~ . - date, data = dff,
                        weights = dated_weights)

scores = rf_model %>% predict(type = 'prob') %>% .[, 2]
df_scores = tibble(date = dff$date, scores = scores)
threshold = 0.1
tuned_res = tibble(date = dff$date, scores = scores) %>% tune_treshold

# list(prob_model = rf_model, tuned_res = tuned_res)
# rf_fitter(df %>% filter(date < ymd("1930-01-01")))
# model_back_test_one_time(df, ml_fitter, ml_predictor, "1960/01/01")
ml_predictions = model_historical_predict(df, ml_fitter, ml_predictor)
ml_predictions %>%
    transmute(date, bubble, prediction, scores, threshold, f_3m = lead(sp_price_d_3m)) %>%
    View

confusionMatrix(as.factor(ml_predictions$prediction), as.factor(ml_predictions$bubble))

simulated_res_ml = ml_predictions %>%
    mutate(action = ifelse(prediction == 1, 0, 1)) %>%
    trade_simulate

# baseline strategy: buy and hold
simulated_res_buy_hold = df %>%
    filter(date >= ymd(quantile(df$date, .25, type = 1))) %>%
    mutate(action = 1) %>%
    trade_simulate

simulated_res_ml %>%
    # filter(date <= "1950/01/01") %>%
    # filter(date >= "1970/01/01") %>%
    # filter(date >= "1990/01/01") %>%
    # filter(date <= "2000/01/01") %>%
    plot_simulation("ml")

simulated_res_buy_hold %>%
    # filter(date >= "1930/01/01") %>%
    # filter(date <= "1950/01/01") %>%
    # filter(date >= "1950/01/01") %>%
    # filter(date >= "1970/01/01") %>%
    # filter(date >= "1990/01/01") %>%
    plot_simulation("baseline")

ml_predictions %>%
    mutate(action = scores < 0.5) %>%
    trade_simulate %>%
    # filter(date >= "1950/01/01") %>%
    plot_simulation

ml_predictions %>%
    transmute(date, scores, bubble, sp_price_d_3m, f_3m = lead(sp_price_d_3m)) %>%
    filter(scores > 0.5)

df %>% arrange(sp_price_d_3m) %>% View


simulated_res_buy_hold %>%
    select(date, exposed_return, cumulative_return) %>%
    inner_join(simulated_res_ml %>% select(date, exposed_return, cumulative_return),
               by = 'date', suffix = c('_buy_hold', '_ml')) %>%
    mutate(return_diff = exposed_return_ml - exposed_return_buy_hold) %>%
    View
    ggplot(aes(x = date, y = return_diff)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    labs(title = 'Return difference of ml model over buy and hold',
         x = 'Date',
         y = 'Return Difference') +
    theme_minimal()

#################### Explore the model ####################

fitted = ml_fitter(df %>% filter(date < '1950-01-01'))
rf = fitted$prob_model
varImpPlot(rf)

sum(ml_predictions$prediction == 1)
ml_predictions %>% View

ml_predictions %>%
    ggplot(aes(x = date, y = scores)) +
    geom_line() +
    geom_hline(yintercept = 0.5, linetype = 'dashed')