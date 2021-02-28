library(tidyverse)
# library(quantmod)
library(Quandl)
library(lubridate)
library(glue)

# I. Raw data

# Data frequency is monthly unless specified
sp500 = Quandl("MULTPL/SP500_REAL_PRICE_MONTH") %>% filter(day(Date) == 1)
tbill_yield = Quandl("FRED/GS10")
shiller_pe = Quandl("MULTPL/SHILLER_PE_RATIO_MONTH")
gdp = Quandl("FRED/GDP") # this is quarterly
real_gdp = Quandl("FRED/GDPC1") # this is quarterly
cpi = Quandl("FRED/CPIAUCSL")
consumer_confidence = read_csv('data/CSCICP03USM665S.csv')%>%
  rename(Date = DATE, Value = CSCICP03USM665S)


# II. Help functions

# Calculate the change rate of time-series data; annualized, in percent
# If don't annualized, use period = 1
calculate_growth_rate = function(data, periods = 12) {
  data %>% 
    arrange(Date) %>% 
    mutate(Diff = Value - lag(Value),
           Rate_period = Diff / lag(Value),
           Percent = ((1 + Rate_period)^periods - 1) * 100) %>% 
    select(Date, Percent) %>% 
    return
}

# Convert quarterly data to monthly data by simple expansion
quarterly2monthly = function(data) {
  month_plus1 = data %>% mutate(Date = Date + months(1))
  month_plus2 = data %>% mutate(Date = Date + months(2))
  rbind(data, month_plus1, month_plus2) %>% arrange(Date) %>% return
}

# # Rename a dataset
# rename_value = function(data, name) {
#   if (missing(name)) {
#     name = deparse(substitute(data))
#   }
#   if ("Value" %in% names(data)) {
#     data %>% rename("{name}" := Value) %>% return
#   } else if ("Percent" %in% names(data)) {
#     data %>% rename("{name}" := Percent) %>% return
#   } else {
#     return(data)
#   }
# }


# III. Output data

# 1. real GDP growth rate, with monthly expansion
real_gdp_growth = real_gdp %>% 
  calculate_growth_rate(4) %>% 
  quarterly2monthly %>% 
  rename(real_gdp_growth = Percent)
plot(real_gdp_growth, type = 'l')
summary(real_gdp_growth %>% filter(Date >= '1960-01-01') %>% .[,2])

# 2. Inflation rate
inflation = cpi %>% calculate_growth_rate %>% rename(inflation = Percent)
plot(inflation, type = 'l')
summary(inflation %>% .[,2])

# 3. 10-year T-bill yield
tbill_yield = tbill_yield %>% rename(tbill_yield = Value)
plot(tbill_yield, type = 'l')
summary(tbill_yield %>% .[,2])

# 4. Consumer confidence
consumer_confidence = consumer_confidence %>% rename(consumer_confidence = Value)
plot(consumer_confidence, type = 'l')
summary(consumer_confidence %>% .[,2])

# 5. Shiler p/e ratio
shiller_pe = shiller_pe %>% rename(shiller_pe = Value)
plot(shiller_pe %>% filter(Date >= '1995-01-01'), type = 'l')

# 6. S&P500 return, monthly (not annualized)
sp500_return = sp500 %>% calculate_growth_rate(1) %>%  # not annualized, so use periods = 1
  rename(sp500_return = Percent)
plot(sp500_return, type = 'l')
summary(sp500_return$Percent)

# Stock market downturns
sp500_return %>% filter(sp500_return < -10)
sp500_return %>% filter(sp500_return > 10)


# 7. Market Capitalization to GDP ratio, "Buffet Indicator"
gdp_monthly = gdp %>% quarterly2monthly
mktcap_gdp_ratio = inner_join(sp500, gdp_monthly, by = 'Date') %>% 
  mutate(mktcap_gdp_ratio = Value.x / Value.y) %>% 
  select(Date, mktcap_gdp_ratio)
plot(mktcap_gdp_ratio, type = 'l')
  


