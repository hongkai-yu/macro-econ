library(tidyverse)
library(tidyquant)
library(Quandl)
library(lubridate)
library(glue)

explore_df = read_csv(glue('{DATA_FOLDER}/processed/labelled_data.csv'))

df %>%
    mutate(f_3m = lead(sp_price_d_3m)) %>%
    # filter(date >= "1980-01-01") %>%
    # filter(bubble == 1) %>%
    filter(date >= "1970-01-01", date <= "1990-01-01") %>%
    # filter(date >= "2005-01-01", date <= "2010-01-01") %>%
    View

explore_df %>%
    select(-date, -bubble) %>%
    cor(use = "complete.obs") %>%
    corrplot::corrplot(method = "number")

explore_df %>%
    filter(date >= "1950-01-01", date <= "1990-01-01") %>%
    ggplot(aes(x = date, y = real_gold_price)) +
    geom_line() +
    geom_line(aes(y = real_price), color = "red") +
    theme_minimal()

explore_df %>%
    filter(date >= "2000-01-01") %>%
    ggplot(aes(x = date, y = real_price / real_gold_price)) +
    geom_line() +
    theme_minimal()

total_return = last(explore_df$real_price) / first(explore_df$real_price) - 1
annualized_return = (1 + total_return) ^ (1 / (year(last(explore_df$date)) - year(first(explore_df$date)))) - 1
total_gdp_growth = last(explore_df$real_gdp) / first(explore_df$real_gdp) - 1
annualized_gdp_growth = (1 + total_gdp_growth) ^ (1 / (year(last(explore_df$date)) - year(first(explore_df$date)))) - 1

explore_df %>%
    select(date, real_price) %>%
    filter(month(date) == 1) %>%
    mutate(year_delta = year(date) - year(first(date))) %>%
    mutate(exponential = first(real_price) * (1 + annualized_return) ^ year_delta) %>%
    ggplot(aes(x = date, y = real_price)) +
    geom_line() +
    geom_line(aes(y = exponential), color = "red") +
    theme_minimal()

explore_df %>% select(date, real_price, real_gold_price) %>% View

explore_df %>%
    ggplot(aes(x = date, y = real_price)) +
    geom_line() +
    geom_vline(xintercept = explore_df$date[explore_df$bubble == 1], color = "red", linetype = "dashed") +
    labs(title = "S&P 500 Price", x = "Year", y = "Price") +
    theme_bw()

explore_df %>%
    ggplot(aes(x = date, y = bft_indicator)) +
    geom_line() +
    labs(title = "BFT Indicator", x = "Year", y = "BFT Indicator") +
    theme_bw()

explore_df %>%
    ggplot(aes(x = date, y = total_cape)) +
    geom_line() +
    labs(title = "Shiller PE", x = "Year", y = "Shiller PE") +
    theme_bw()

explore_df %>%
    ggplot(aes(x = date, y = real_gdp)) +
    geom_line() +
    labs(title = "Real GDP", x = "Year", y = "Real GDP") +
    theme_bw()

