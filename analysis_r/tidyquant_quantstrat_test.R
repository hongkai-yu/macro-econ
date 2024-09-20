# install.packages("devtools") # if not installed
# install.packages("FinancialInstrument") #if not installed
# install.packages("PerformanceAnalytics") #if not installed
#
# # next install blotter from GitHub
# devtools::install_github("braverock/blotter")
# # next install quantstrat from GitHub
# devtools::install_github("braverock/quantstrat")
# devtools::install_github("business-science/tidyquant")
#
# library(Quandl)
library(quantstrat)
library(tidyverse)
library(tidyquant)
library(lubridate)
# library(glue)

currency("USD")
stock("AAPL", currency = "USD", multiplier = 1)
stock()

macross_strategy = strategy("macross") %>%
    add.indicator("SMA", arguments = list(x = quote(Cl(mktdata)), n = 10), label = "ma10") %>%
    add.indicator("SMA", arguments = list(x = quote(Cl(mktdata)), n = 20), label = "ma20") %>%
    add.signal("sigCrossover", arguments = list(column = c("SMA10", "SMA20"), relationship = "gte"), label = "ma10.gte.ma20") %>%
    add.signal("sigCrossover", arguments = list(column = c("SMA10", "SMA20"), relationship = "lt"), label = "ma10.lt.ma20") %>%
    add.rule("ruleSignal", arguments = list(sigcol = "ma10.gte.ma20", sigval = TRUE, orderqty = 100, ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open", TxnFees = -10), type = "enter", label = "enter100") %>%
    add.rule("ruleSignal", arguments = list(sigcol = "ma10.lt.ma20", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open", TxnFees = -10), type = "exit", label = "exitAll")


initPortf("macross_test", symbols = "AAPL", initDate = "2010-01-01")
initAcct("macross_test", portfolios = "macross_test", initDate = "2010-01-01", initEq = 100000)
initOrders(portfolio = "macross_test", initDate = "2010-01-01")
getSymbols("AAPL", from = "2010-01-01", to = "2019-01-01", src = "yahoo", adjust = TRUE)
# AAPL = tq_get("AAPL")
result = applyStrategy(macross_strategy, portfolio = "macross_test")
getSymbols()
updatePortf(Portfolio = "macross_test", Dates = time(result))
updateAcct(Portfolio = "macross_test", Dates = time(result))
updateEndEq(Account = "macross_test", Dates = time(result))
chart.Posn("macross_test", Symbol = "AAPL", Dates="2010-01-01/2019-01-01")
result
stock()
SMA
sigCrossover()
Hi(AAPL)

# ---- tidyquant
tq_get_options()
tq_index_options()
sp500_index = tq_index("SP500")
nyse = tq_exchange("NYSE")
aapl_stock_prices = tq_get("AAPL")
quote
chart.Posn()

aapl_stock_prices %>% View


df %>%
    mutate(f_3m = lead(sp_price_d_3m)) %>%
    # filter(date >= "1980-01-01") %>%
    # filter(bubble == 1) %>%
    filter(date >= "1970-01-01", date <= "1990-01-01") %>%
    # filter(date >= "2005-01-01", date <= "2010-01-01") %>%
    View

FANG %>%
    filter(symbol == "META") %>%
    filter(date >= "2016-01-01") %>%
        # tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "fb.returns")
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period = "monthly",
        # type = "log",
        col_rename = "daily_return"
    ) %>%
    # tq_mutate(
    #     select = close,
    #     mutate_fun = periodReturn,
    #     period = "daily",
    #     type = "log",
    #     col_rename = "daily_return_2"
    # ) %>%
    View
    head(10)
tq_transmute_fun_options()
periodReturn(FANG$close, period = "daily", type = "log") %>% head(10)
rollapply()
z2 <- zoo(rnorm(6))
rollapply(z2, 3, mean, by = 3)      # means of
tq_mutate()
tq_transmute(
)
table.CAPM()