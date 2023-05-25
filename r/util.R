DATA_FOLDER = "/Users/hongkaiyu/Developer/macro-econ/data"

make_quarter_date = function(year, quarter) {
    return(ymd(paste(year, quarter * 3 - 2, 1, sep = '-')))
}

monthly2quarterly = function(df, columns) {
    df %>%
        group_by(year = year(date), quarter = quarter(date)) %>%
        summarize(across(all_of(columns), mean)) %>%
        ungroup() %>%
        mutate(date = make_quarter_date(year, quarter)) %>%
        select(date, everything()) %>%
        return
}
