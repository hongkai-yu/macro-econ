# Draft and Backlog

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
