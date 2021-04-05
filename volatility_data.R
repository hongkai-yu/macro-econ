library(tidyquant)

sp500_daily = tq_get('^GSPC', from = '1950-12-01', to = '2021-01-31') %>% 
  select('date','close') %>% 
  rename(Date = date, Value = close)

sp500_daily_return = sp500_daily %>%
  calculate_growth_rate(periods = 1, lagPeriod = 1) %>% 
  filter(Date >= '1960-01-01', Date <= '2020-12-31')


volatility = sp500_daily_return %>% 
  group_by(Date = as.Date(paste(year(Date), month(Date), '01', sep = '-'))) %>% 
  summarise(sd = sd(Percent))
