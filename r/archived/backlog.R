# Draft and Backlog
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

# write_csv(df, './data/bubble_detection.csv')k
library(randomForest)
library(corrplot)


rf = randomForest(bubble ~ ., data = df, importance = TRUE, ntree = 1000)
varImpPlot(rf)


m2 = Quandl("FRED/M2SL") 
plot(m2, type = 'l')
# 8. M2 / Real GDP ratio
plot(m2, type = 'l')
m2_gdp_ratio = inner_join(m2, gdp_monthly, by = 'Date') %>% 
  mutate(m2_gdp_ratio = Value.x / Value.y) %>% 
  select(Date, m2_gdp_ratio)
plot(m2_gdp_ratio, type = 'l')


# Not important
df_test %>% filter(bubble == 1) %>% select(Date, logit.test) %>% summary
df_test %>% filter(bubble == 0) %>% select(Date, logit.test) %>% summary
filter(df_test, bubble == 0)$logit.test %>% mean

ggplot(data = pred_df, aes(x = Date, y = logfit)) + 
  geom_line() +
  geom_point(aes(y = bubble), col = 'red', size = 0.1)

# Rename a dataset
rename_value = function(data, name) {
  if (missing(name)) {
    name = deparse(substitute(data))
  }
  if ("Value" %in% names(data)) {
    data %>% rename("{name}" := Value) %>% return
  } else if ("Percent" %in% names(data)) {
    data %>% rename("{name}" := Percent) %>% return
  } else {
    return(data)
  }
}
