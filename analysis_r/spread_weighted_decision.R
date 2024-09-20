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
    mutate(real_return = real_price_d_3m, real_cash_return = (1 / cpi) / (1 / lag(cpi)) - 1) %>%
    mutate(real_return = ifelse(is.na(real_return), 0, real_return),
           real_cash_return = ifelse(is.na(real_cash_return), 0, real_cash_return)) %>%
    mutate(will_gain = ifelse(lead(real_return) > lead(real_cash_return), 1, 0)) %>%
    mutate(spread_weight = lead(abs(log(1 + real_return) - log(1 + real_cash_return)))) %>%
    select(-starts_with('real_price_f_'),
           -starts_with('sp_price_f_'),
           -starts_with('real_price'),
           -starts_with("gold_price"),
           -starts_with("real_gold_price"),
           -starts_with("gs10_d_"),
           -all_of(c("year", "quarter", "real_gdp", "cpi", "real_price", "sp_price", "inflation_diff_3m")),
           -all_of(c("real_return", "real_cash_return", "bubble"))
    )
df$will_gain = as.factor(df$will_gain) # as factor

rf_spread_weighted_fitter = function(df) {
    # recent is more important
    dated_weights = ifelse(
        last(df$date) - df$date <= years(20),
        1, 1
    )
    df$spread_weight * dated_weights
    rf_model = randomForest(
        will_gain ~ . - date,
        data = df %>% select(-spread_weight),
        weights = df$spread_weight * dated_weights
    )
    list(prob_model = rf_model)
}

rf_spread_weighted_predictor = function(model, new_data) {
    new_data$scores = predict(model$prob_model, newdata = new_data, type = 'prob')[, 2]
    new_data %>%
        mutate(threshold = 0.5) %>%
        mutate(prediction = ifelse(scores > threshold, 1, 0))
}

set.seed(0827)
ml_fitter = rf_spread_weighted_fitter
ml_predictor = rf_spread_weighted_predictor

model_back_test_one_time(df, ml_fitter, ml_predictor, "2023/01/01") %>%
    select(date, scores, prediction)

ml_predictions = model_historical_predict(df, ml_fitter, ml_predictor)
ml_predictions %>%
    transmute(date, will_gain, scores, prediction, f_3m = lead(sp_price_d_3m)) %>%
    # arrange(scores) %>%
    View

confusionMatrix(as.factor(ml_predictions$prediction), as.factor(ml_predictions$will_gain))

simulated_res_ml = ml_predictions %>%
    mutate(action = ifelse(prediction == 1, 1, 0)) %>%
    trade_simulate

# baseline strategy: buy and hold
simulated_res_buy_hold = df %>%
    filter(date >= ymd(quantile(df$date, .25, type = 1))) %>%
    mutate(action = 1) %>%
    trade_simulate

simulated_res_ml %>%
    # filter(date >= "1950/01/01") %>%
    # filter(date >= "1970/01/01") %>%
    # filter(date >= "1990/01/01") %>%
    # filter(date >= "1980/01/01") %>%
    # filter(date <= "2000/01/01") %>%
    plot_simulation("ml")

simulated_res_buy_hold %>%
    # filter(date >= "1930/01/01") %>%
    # filter(date >= "1950/01/01") %>%
    # filter(date >= "1950/01/01") %>%
    # filter(date >= "1970/01/01") %>%
    # filter(date >= "1980/01/01") %>%
    # filter(date >= "1990/01/01") %>%
    plot_simulation("baseline")

ml_predictions %>%
    mutate(action = scores < 0.5) %>%
    trade_simulate %>%
    # filter(date >= "1950/01/01") %>%
    plot_simulation

df %>%
    filter(date < "2023/01/01") %>%
    ml_fitter %>%
    .$prob_model %>%
    varImpPlot

hindsight_ml_predictions = df %>%
    filter(date < "2023/01/01") %>%
    ml_fitter %>%
    ml_predictor(df)

simulated_res_hindshight_ml = hindsight_ml_predictions %>%
    mutate(action = ifelse(prediction == 1, 1, 0)) %>%
    trade_simulate

simulated_res_hindshight_ml %>% View

simulated_res_hindshight_ml %>%
    filter(year(date) > 1900) %>%
    mutate(r = exposed_return + 1) %>%
    .$r %>% prod

1.03 ^ 493
simulated_res_hindshight_ml %>%
    filter(will_gain == 1) %>%
    summarise(mean(lead(sp_price_d_3m), na.rm = TRUE))
df %>% View

df %>%
    filter(date < "1950/01/01") %>%
    mutate(f_3m = lead(sp_price_d_3m)) %>%
    select(-date, -will_gain) %>%
    cor(use = "complete.obs") %>%
    corrplot::corrplot(method = "number")

df %>% View