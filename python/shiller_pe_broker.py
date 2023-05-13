import pandas as pd
from python.util import download_file, DATA_FOLDER

raw_path = f"{DATA_FOLDER}/raw/shiller_pe.xls"
processed_path = f"{DATA_FOLDER}/processed/shiller_pe.csv"


def download_raw():
    url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
    download_file(url, raw_path)


def read_raw() -> pd.DataFrame:
    xls = pd.ExcelFile(raw_path)
    return pd.read_excel(xls, "Data", dtype={"Unnamed: 0": str})


def process_raw(df: pd.DataFrame, save_processed: bool = False) -> pd.DataFrame:
    df = df.iloc[8:len(df) - 1]

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
    df.columns = header
    df = df.drop(["empty1", "empty2"], axis=1)
    df["date"] = df["date"].str.replace(r'(\d{4})\.1$', r'\1.10').str.replace(".", "/")

    if save_processed:
        df.to_csv(processed_path, index=False)

    return df


def read_processed() -> pd.DataFrame:
    return pd.read_csv(processed_path)


if __name__ == "__main__":
    download_raw()
    raw_data = read_raw()
    process_raw(raw_data, save_processed=True)
