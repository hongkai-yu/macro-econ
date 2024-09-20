library(tidyverse)
library(tidyquant)
library(arrow)
Sys.setenv(LIBARROW_MINIMAL = "false")
install.packages("arrow")

DATA_FOLDER <- "/Users/hongkaiyu/Developer/macro-econ/data"

sp500 <- tq_index("SP500")

# sp500_stock <- tq_get(sp500$symbol, from = "2022-01-01", to = "2023-12-31")
# sp500_stock %>%
#     write_csv(str_glue("{DATA_FOLDER}/raw/sp500_stock.csv"))

sp500_stock <- read_csv(str_glue("{DATA_FOLDER}/raw/sp500_stock.csv"))

sp500_stock %>%
    write_parquet(str_glue("{DATA_FOLDER}/processed/sp500_stock.parquet"))

sp500_stock %>%
    tq_mutate(periodReturn())

sp500_stock %>%
    group_by(date) %>%
    summarise(count = n()) %>%
    View