# today's data!

data_today = data.frame(Date = '2021-04-04',
                         real_gdp_growth = 3,
                         inflation = 4.33,
                         tbill_yield = 1.61,
                         shiller_pe = 36.16,
                         consumer_confidence = 98.67,
                         mktcap_gdp_ratio = 4019.87 / (18794.426 * 1.03) ,
                         volatility = 1.07,
                         sp500_return = 4.63,
                         sp500_re3 = 6.52,
                         sp500_re6 = 17.93,
                         sp500_re12 = 50.91,
                         sp500_re60 = 96.32)

predict(rf.out, newdata = data_today, type = 'prob')

# names(df_test)
# names(data_today)
