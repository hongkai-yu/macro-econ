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


# II. Clean data
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
    filter(year >= START_YEAR - 10, year < 1947) %>%
    mutate(real_gdp = gdp * consistent_factor) %>%
    # duplicate each row four times with 1, 2,3,4
    uncount(4, .id = 'quarter') %>%
    mutate(date = ymd(paste(year, quarter * 3 - 2, 1, sep = '-'))) %>%
    select(date, real_gdp) %>%
    bind_rows(modern_real_gdp)


# 2. Shiller PE
shiller_quarterly = shiller %>%
    group_by(year = year(date), quarter = quarter(date)) %>%
    summarize(across(all_of(c("cpi", "gs10", "total_cape")), mean),
              across(all_of("real_price"), last)) %>%
    ungroup() %>%
    mutate(date = make_quarter_date(year, quarter)) %>%
    select(date, everything()) %>%
    filter(year >= START_YEAR - 10)

cleaned_data = shiller_quarterly %>%
    inner_join(all_real_gdp, by = c("date" = "date"))

# III. Create features and the label
features_data = cleaned_data %>%
    mutate(bft_indicator = real_price / real_gdp) %>%
    mutate(across(all_of(c("cpi", "real_gdp")), list(d_1y = ~.x / lag(.x, 4) - 1))) %>% # calculate yearly change in gdp and cpi
    mutate(across(all_of("real_price"), list(   # difference between current and previous year
        d_3m = ~.x / lag(.x, 1) - 1,
        d_6m = ~.x / lag(.x, 2) - 1,
        d_1y = ~.x / lag(.x, 4) - 1,
        d_5y = ~.x / lag(.x, 20) - 1
    ))) %>%
    mutate(across(all_of("real_price"), list(
        f_3m = ~lead(.x, 1) / .x - 1,
        f_6m = ~lead(.x, 2) / .x - 1,
        f_1y = ~lead(.x, 4) / .x - 1
    ))) %>%
    filter(year >= START_YEAR)


quantile_level = 0.1
threshold_3m = quantile(features_data$real_price_d_3m, quantile_level)
threshold_6m = quantile(features_data$real_price_d_6m, quantile_level)
threshold_1y = quantile(features_data$real_price_d_1y, quantile_level)

labelled_data = features_data %>%
    mutate(bubble = ifelse(
        (real_price_f_3m < threshold_3m) |
            (real_price_f_6m < threshold_6m) |
            (real_price_f_1y < threshold_1y),
        1, 0)
    )

labelled_data %>%
    write_csv(glue('{DATA_FOLDER}/processed/labelled_data.csv'))

labelled_data %>%
    select(-date, -bubble) %>%
    cor(use = "complete.obs") %>%
    corrplot::corrplot(method = "number")

################################################## EXPLORATORY ANALYSIS ##################################################

labelled_data = read_csv(glue('{DATA_FOLDER}/processed/labelled_data.csv'))
# fit random forest
library(randomForest)
library(corrplot)


rf = randomForest(bubble ~ ., data = labelled_data, importance = TRUE, ntree = 1000)
varImpPlot(rf)


# IV. Plot data
labelled_data %>%
    ggplot(aes(x = date, y = real_price)) +
    geom_line() +
    geom_vline(xintercept = labelled_data$date[labelled_data$bubble == 1], color = "red", linetype = "dashed") +
    labs(title = "S&P 500 Price", x = "Year", y = "Price") +
    theme_bw()

labelled_data %>%
    ggplot(aes(x = date, y = bft_indicator)) +
    geom_line() +
    labs(title = "BFT Indicator", x = "Year", y = "BFT Indicator") +
    theme_bw()

labelled_data %>%
    ggplot(aes(x = date, y = total_cape)) +
    geom_line() +
    labs(title = "Shiller PE", x = "Year", y = "Shiller PE") +
    theme_bw()

labelled_data %>%
    ggplot(aes(x = date, y = real_gdp)) +
    geom_line() +
    labs(title = "Real GDP", x = "Year", y = "Real GDP") +
    theme_bw()


shiller$sp_price %>% plot(type = 'l')


sp500 = Quandl("MULTPL/SP500_REAL_PRICE_MONTH") %>% filter(day(Date) == 1)
tbill_yield = Quandl("FRED/GS10")


shiller_pe = Quandl("MULTPL/SHILLER_PE_RATIO_MONTH")
gdp = Quandl("FRED/GDP") # this is quarterly
real_gdp = Quandl("FRED/GDPC1") # this is quarterly
cpi = Quandl("FRED/CPIAUCSL")
consumer_confidence = read_csv('data/CSCICP03USM665S.csv') %>%
    rename(Date = DATE, Value = CSCICP03USM665S)

# II. Helper functions

# Calculate the change rate of time-series data; annualized, in percent
# If don't annualized, use period = 1
calculate_growth_rate = function(data, periods = 12, lagPeriod = 1) {
    data %>%
        arrange(Date) %>%
        mutate(Diff = Value - lag(Value, lagPeriod),
               Rate_period = Diff / lag(Value, lagPeriod),
               Percent = ((1 + Rate_period)^periods - 1) * 100) %>%
        select(Date, Percent) %>%
        return
}

# Convert quarterly data to monthly data by simple expansion
quarterly2monthly = function(data) {
    month_plus1 = data %>% mutate(Date = Date + months(1))
    month_plus2 = data %>% mutate(Date = Date + months(2))
    rbind(data, month_plus1, month_plus2) %>%
        arrange(Date) %>%
        return
}


# 7. S&P500 return, monthly (not annualized)
calc_sp500_return = function(return_months = 1) {
    if (return_months == 1) {
        colname = 'sp500_return'
    } else {
        colname = paste('sp500_re', return_months, sep = '')
    }

    sp500 %>%
        calculate_growth_rate(periods = 1, lagPeriod = return_months) %>%  # not annualized, so use periods = 1
        rename(!!colname := Percent) %>%
        filter(Date >= '1960-01-01' & Date <= '2020-12-01')
}

sp500_return = calc_sp500_return(1)


# IV. All together
df = plyr::join_all(list(real_gdp_growth, inflation, tbill_yield,
                         shiller_pe, consumer_confidence, mktcap_gdp_ratio,
                         # volatility,
                         sp500_return, sp500_re3, sp500_re6, sp500_re12, sp500_re60),
                    by = 'Date', type = 'inner')

# Define a crash as the return in the 1% quantile
threshold = quantile(df$sp500_return, 0.01)

df$crash = 0
for (i in 1:nrow(df)) {
    if (df$sp500_return[i] < threshold) {
        df$crash[i] = 1
    }
}

# Define a bubble as there will be a crash in 6 months
periods = 6
df$bubble = 0
for (i in 1:nrow(df)) {
    if (df$crash[i] != 1 && (1 %in% df$crash[(i + 1):(i + periods)])) {
        df$bubble[i] = 1
    }
}

df = df %>% select(-crash)

names(df)

# write_csv(df, './data/bubble_detection.csv')



