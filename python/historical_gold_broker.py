import pandas as pd
from python.util import download_file, PROJECT_FOLDER, extract_tables_from_pdf

raw_path = f"{PROJECT_FOLDER}/data/raw/historical_gold_prices.pdf"
processed_path = f"{PROJECT_FOLDER}/data/processed/historical_gold_prices.csv"


def download_raw():
    url = "https://nma.org/wp-content/uploads/2016/09/historic_gold_prices_1833_pres.pdf"
    download_file(url, raw_path)


def read_raw() -> pd.DataFrame:
    tables = extract_tables_from_pdf(raw_path)
    return pd.concat(tables, ignore_index=True)


def process_raw(df: pd.DataFrame, save_processed: bool = False) -> pd.DataFrame:
    df = df.iloc[1:len(df)]
    headers = ["year", "price"]
    df.columns = headers
    df["year"] = df["year"].astype(str).str.replace('*', '').astype(int)
    df['price'] = df['price'].astype(str).str.replace(',', '').astype(float)

    if save_processed:
        df.to_csv(processed_path, index=False)

    return df


def read_processed() -> pd.DataFrame:
    return pd.read_csv(processed_path)


if __name__ == "__main__":
    download_raw()
    raw_data = read_raw()
    process_raw(raw_data, save_processed=True)
