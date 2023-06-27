library(tidyverse)
library(tidyquant)
library(Quandl)
library(lubridate)
library(glue)

source('r/util.R')
START_YEAR = 1900

# I. Raw data
modern_real_gdp = read_csv(glue('{DATA_FOLDER}/processed/real_gdp.csv'), col_types = cols(date = col_date(format = "%Y/%m/%d")))
shiller = read_csv(glue('{DATA_FOLDER}/processed/shiller_pe.csv'), col_types = cols(date = col_date(format = "%Y/%m")))
maddison_gdp = read_csv(glue('{DATA_FOLDER}/processed/maddison_gdp.csv'), col_types = cols(year = col_integer())) %>%
    filter(country == 'United States') %>%
    select("year", "gdp") %>%
    mutate(gdp = gdp / 1e6)

gold = read_csv(glue('{DATA_FOLDER}/processed/gold.csv'), col_types = cols(date = col_date())) %>%
    select(date, gold_price = usd_am)

historical_gold = read_csv(glue('{DATA_FOLDER}/processed/historical_gold_prices.csv'), col_types = cols(year = col_integer())) %>%
    transmute(year, gold_price = price)

# II. Clean data
# Note: date alignment: all the data is the for the end of a certain quarter, or the beginning of the next quarter
# For 2021-Q1, all the data should be available at the start of 2021-04, at that point we make predictions for 2021-Q2

# 1. real GDP
consistent_factor = modern_real_gdp %>%
    group_by(year = year(date)) %>%
    filter(year >= 1947, year <= 1952) %>%
    summarize(real_gdp = mean(real_gdp)) %>%
    ungroup() %>%
    inner_join(maddison_gdp, by = c("year" = "year")) %>%
    mutate(factor = real_gdp / gdp) %>%
    .$factor %>%
    mean

all_real_gdp = maddison_gdp %>%
    filter(year >= START_YEAR - 10, year < year(first(modern_real_gdp$date))) %>%
    mutate(real_gdp = gdp * consistent_factor) %>%
    # duplicate each row four times with 1, 2,3,4
    uncount(4, .id = 'quarter') %>%
    mutate(date = make_quarter_date(year, quarter)) %>%
    select(date, real_gdp) %>%
    bind_rows(modern_real_gdp)

# 2. Shiller PE
shiller_quarterly = shiller %>%
    group_by(year = year(date), quarter = quarter(date)) %>%
    summarize(across(all_of(c("cpi", "total_cape")), mean),
              across(all_of(c("real_price", "sp_price", "gs10")), first)) %>% # start of the quarter
    ungroup() %>%
    # the start of the next quarter, which is the end of the current quarter
    mutate(across(all_of(c("real_price", "sp_price", "gs10")), ~lead(.x, 1))) %>%
    mutate(date = make_quarter_date(year, quarter)) %>%
    select(date, everything()) %>%
    filter(year >= START_YEAR - 10)

# 3. Gold
all_gold_price = historical_gold %>%
    filter(year >= START_YEAR - 10, year < year(first(gold$date))) %>%
    uncount(4, .id = 'quarter') %>%
    mutate(date = make_quarter_date(year, quarter)) %>%
    select(date, gold_price) %>%
    bind_rows(
        gold %>%
            group_by(year = year(date), quarter = quarter(date)) %>%
            summarize(gold_price = last(gold_price)) %>%
            ungroup() %>%
            mutate(date = make_quarter_date(year, quarter)) %>%
            select(date, gold_price)
    )

cleaned_data = plyr::join_all(
    list(shiller_quarterly, all_real_gdp, all_gold_price),
    by = "date", type = "inner")

# III. Create features and the label
featurized = cleaned_data %>%
    mutate(bft_indicator = real_price / real_gdp) %>%
    mutate(real_gold_price = gold_price / cpi * last(cpi)) %>%
    mutate(sp_gold_ratio = sp_price / gold_price) %>%
    mutate(across(all_of(c("cpi", "real_gdp")), list(d_1y = ~.x / lag(.x, 4) - 1))) %>% # calculate yearly change in gdp and cpi
    mutate(real_gs10 = gs10 - cpi_d_1y) %>% # real interest rate
    mutate(across(all_of(c("real_price", "gs10", "sp_price", "real_gold_price", "gold_price")), list(   # the change of real price and interest rate
        d_3m = ~.x / lag(.x, 1) - 1,
        d_6m = ~.x / lag(.x, 2) - 1,
        d_1y = ~.x / lag(.x, 4) - 1,
        d_5y = ~.x / lag(.x, 20) - 1
    ))) %>%
    mutate(across(all_of(c("real_price", "sp_price")), list(
        f_3m = ~lead(.x, 1) / .x - 1,
        f_6m = ~lead(.x, 2) / .x - 1,
        f_1y = ~lead(.x, 4) / .x - 1
    ))) %>%
    mutate(inflation_diff_3m = cpi_d_1y - lag(cpi_d_1y)) %>%
    filter(year >= START_YEAR)

# quantile(featurized$cpi_d_1y, 0.75)
# quantile(featurized$inflation_d_3m, 0.3)

quantile_level = 0.1
threshold_3m = quantile(featurized$sp_price_d_3m, quantile_level)

labelled_data = featurized %>%
    mutate(bubble = ifelse(
        sp_price_f_3m < threshold_3m, 1, 0)
    )

labelled_data %>%
    write_csv(glue('{DATA_FOLDER}/processed/labelled_data.csv'))
