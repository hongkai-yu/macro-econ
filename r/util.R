DATA_FOLDER = "/Users/hongkaiyu/Developer/macro-econ/data"
library(tidyverse)
library(lubridate)
library(glue)
library(magrittr)
library(furrr)

#' make a quarter date (the first day of the quarter)
#' for example, year = 2019, quarter = 1, will return 2019-01-01
make_quarter_date = function(year, quarter) {
    ymd(paste(year, quarter * 3 - 2, 1, sep = '-'))
}


PRICE_HISTORY = read_csv(glue("{DATA_FOLDER}/processed/labelled_data.csv")) %>%
    transmute(date, real_return = real_price_d_3m, real_cash_return = (1 / cpi) / (1 / lag(cpi)) - 1) %>%
    mutate(real_return = ifelse(is.na(real_return), 0, real_return),
           real_cash_return = ifelse(is.na(real_cash_return), 0, real_cash_return))


#' simulate a trading strategy
#' @param action a data frame with date and action column
#' @param price_history a data frame with date and real_price column
#' @param start_date the start date of the simulation
#' @param end_date the end date of the simulation
#' @return a data frame with date and cumulative_return, you can do last(df[cumulative_return]) to get the final return
trade_simulate = function(action, price_history = PRICE_HISTORY, start_date = '1900-01-01', end_date = '2100-01-01') {
    price_history %>%
        filter(date >= start_date, date <= end_date) %>%
        inner_join(action, by = "date") %>%
        # "action" is the buy/sell action for the next interval
        # so this interval's return is determined by the action of the previous interval
        mutate(last_action = ifelse(is.na(lag(action)), 0, lag(action))) %>%
        mutate(exposed_return = real_return * last_action + (1 - last_action) * real_cash_return) %>%
        mutate(cumulative_return = cumprod(1 + exposed_return))
}

#' plot a simulation result
#' @param simulated_res a data frame with date and cumulative_return, typically from trade_simulate
plot_simulation = function(simulated_res) {
    start_date = first(simulated_res$date)
    end_date = last(simulated_res$date)

    normalized = simulated_res %>%
        mutate(cumulative_return = cumulative_return / first(cumulative_return))

    normalized %>%
        ggplot(aes(x = date, y = cumulative_return)) +
        geom_line() +
        geom_hline(yintercept = 1, linetype = "dashed") +
        labs(title = "Strategy",
             subtitle = glue(
                 "Cumulative return of $1 invested in {year(start_date)} will be ${round(last(normalized$cumulative_return), 2)} in {year(end_date)}"
             ),
             x = "Date",
             y = "Cumulative return") +
        theme_bw()
}


#' see what would the model predict one time
model_back_test_one_time = function(df, model_fitter, model_predictor, prediction_date) {
    print(paste("train and evaluate on", prediction_date, "..."))
    df_train = df %>% filter(date < prediction_date)
    df_test = df %>% filter(date == prediction_date)
    model = model_fitter(df_train)
    model_predictor(model, new_data = df_test)
}


#' see what would the model predict in history
#' fit a model on training data, and predict on test data on the next interval
#' the first 25% of the data would not be evaluated on
#' @param df data frame with date column
#' @param model_fitter function that takes in a data frame and return a model
#' @param model_predictor function that takes in a model and a data frame and return a predicted dataframe
#' @return a data frame with prediction column
model_historical_predict = function(df, model_fitter, model_predictor) {
    evaluation_start = ymd(quantile(df$date, .25, type = 1))
    evaluation_dates = df %>%
        filter(date >= evaluation_start) %>%
        .$date

    plan(multisession, workers = 4)
    evaluation_dates %>%
        future_map(~model_back_test_one_time(df, model_fitter, model_predictor, prediction_date = .),
                   .options = future_options(seed = TRUE)) %>%
        bind_rows()
}

#' tune the decision threshold of a classification model
#' @param df_scores a data frame with scores column and date column
#' @return a list with return_star, threshold_star, thresholds, returns
tune_treshold = function(df_scores, thresholds = seq(from = 0.01, to = 0.99, by = 0.01)) {
    returns = thresholds %>%
        map_dbl(~{
            threshold = .

            df_scores %>%
                mutate(action = ifelse(scores > threshold, 0, 1)) %>%
                trade_simulate %>%
                .$cumulative_return %>%
                last
        })

    list(
        return_star = max(returns),
        threshold_star = thresholds[which.max(returns)],
        thresholds = thresholds,
        returns = returns
    )
}

logit_fitter = function(df) {
    logit_model = glm(bubble ~ . - date, data = df, family = binomial)
    scores = logit_model %>% predict(type = 'response')
    tuned_res = tibble(date = df$date, scores = scores) %>% tune_treshold

    list(prob_model = logit_model, tuned_res = tuned_res)
}

logit_predictor = function(model, new_data) {
    new_data$scores = predict(model$prob_model, newdata = new_data, type = 'response')
    new_data %>%
        mutate(threshold = model$tuned_res$threshold_star) %>%
        mutate(prediction = ifelse(scores > threshold, 1, 0))
}

rf_fitter = function(df) {
    rf_model = randomForest(bubble ~ . - date, data = df)
    scores = rf_model %>% predict(type = 'prob') %>% .[,2]
    tuned_res = tibble(date = df$date, scores = scores) %>% tune_treshold

    list(prob_model = rf_model, tuned_res = tuned_res)
}

rf_predictor = function(model, new_data) {
    new_data$scores = predict(model$prob_model, newdata = new_data, type = 'prob')[,2]
    new_data %>%
        mutate(threshold = model$tuned_res$threshold_star) %>%
        mutate(prediction = ifelse(scores > threshold, 1, 0))
}