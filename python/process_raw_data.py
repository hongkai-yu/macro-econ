import pandas as pd

xls = pd.ExcelFile("./data/raw/shiller_pe.xls")
shiller_pe = pd.read_excel(xls, "Data")

shiller_pe = shiller_pe.iloc[8:len(shiller_pe) - 1]

header = ["date",
          "sp_price",
          "dividend",
          "earnings",
          "cpi",
          "date_fraction",
          "gs10",
          "real_price",
          "real_dividend",
          "real_total_return",
          "real_earnings",
          "real_total_scaled_earning",
          "cape",
          "empty1",
          "total_cape",
          "empty2",
          "excess_cape_yield",
          "monthly_bond_returns",
          "real_total_bond_returns",
          "10y_stock_real_return_annualized",
          "10y_bond_real_return_annualized",
          "10y_excess_return_annualized",
          ]
shiller_pe.columns = header
shiller_pe = shiller_pe.drop(["empty1", "empty2"], axis=1)
shiller_pe.to_csv("./data/processed/shiller_pe.csv")
