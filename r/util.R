DATA_FOLDER = "/Users/hongkaiyu/Developer/macro-econ/data"
library(tidyverse)
library(lubridate)
library(glue)
library(magrittr)

#' make a quarter date (the first day of the quarter)
#' for example, year = 2019, quarter = 1, will return 2019-01-01
make_quarter_date = function(year, quarter) {
    ymd(paste(year, quarter * 3 - 2, 1, sep = '-'))
}


#' simulate a trading strategy
#' @param action a data frame with date and action column
#' @param price_history a data frame with date and real_price column
#' @param start_date the start date of the simulation
#' @param end_date the end date of the simulation
#' @return a data frame with date and cumulative_return, you can do last(df[cumulative_return]) to get the final return
trade_simulate = function(action, price_history, start_date = '1900-01-01', end_date = '2100-01-01') {
    price_history %>%
        filter(date >= start_date, date <= end_date) %>%
        inner_join(action, by = "date") %>%
        mutate(real_return = lead(real_price) / real_price - 1) %>%
        mutate(real_return = ifelse(is.na(real_return), 1, real_return)) %>%
        mutate(exposed_return = real_return * action) %>%
        mutate(cumulative_return = cumprod(1 + exposed_return))
}

#' plot a simulation result
#' @param simulated_res a data frame with date and cumulative_return, typically from trade_simulate
plot_simulation = function(simulated_res) {
    ggplot(simulated_res, aes(x = date, y = cumulative_return)) +
        geom_line() +
        geom_hline(yintercept = 1, linetype = "dashed") +
        labs(title = "Strategy",
             subtitle = glue(
                 "Cumulative return of $1 invested in {year(start_date)} will be ${round(last(simulated_res$cumulative_return), 2)} in {year(end_date)}"
             ),
             x = "Date",
             y = "Cumulative return") +
        theme_bw()
}

#' back test a model
#' fit a model on training data, and predict on test data on the next interval
#' the first 25% of the data would not be evaluated on
#' @param df data frame with date column
#' @param model_fitter function that takes in a data frame and return a model
#' @param model_predictor function that takes in a model and a data frame and return a vector of predictions
#' @return a data frame with prediction column
model_backtest = function(df, model_fitter, model_predictor) {

    model_back_test_one_time = function(df, model_fitter, model_predictor, evaluation_date) {
        df_train = df %>% filter(date < evaluation_date)
        df_test = df %>% filter(date == evaluation_date)
        model = model_fitter(df_train)
        df_test$prediction = model_predictor(model, new_data = df_test)
        df_test
    }

    evaluation_start = ymd(quantile(df$date, .25, type = 1))
    evaluation_dates = df %>%
        filter(date >= evaluation_start) %>%
        .$date

    evaluation_dates %>%
        map(~model_back_test_one_time(df, model_fitter, model_predictor, evaluation_date = .)) %>%
        bind_rows()
}

price_history = labelled_data %>% select(date, real_price)
buy_and_hold = price_history %>%
    mutate(action = 1) %>%
    select(date, action)

start_date = ymd("1980-01-01")
end_date = ymd("2010-01-01")

logit.p.res %>% View
logit.p.res %>% transmute(date, action = 1 - prediction)

# labelled_data %>%
# mutate(action = ifelse(real_price_d_3m > 0.20, 0, 1)) %>%
# mutate(action = ifelse(cpi_d_1y > 0.05, 0, 1)) %>%
# mutate(action = 1) %>%


logit.p.res %>%
    transmute(date, action = 1 - prediction) %>%
    # mutate(action = 1) %>%
    select(date, action) %>%
    trade_simulate(price_history, start_date = start_date, end_date = end_date) %>%
    plot_simulation()


