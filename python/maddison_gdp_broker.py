import pandas as pd
from python.util import download_file, PROJECT_FOLDER

raw_path = f"{PROJECT_FOLDER}/data/raw/maddison_gdp.xlsx"
processed_path = f"{PROJECT_FOLDER}/data/processed/maddison_gdp.csv"


def download_raw():
    url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx"
    download_file(url, raw_path)


def read_raw() -> pd.DataFrame:
    xls = pd.ExcelFile(raw_path)
    return pd.read_excel(xls, "Full data")


def process_raw(df: pd.DataFrame, save_processed: bool = False) -> pd.DataFrame:
    df["gdp"] = df["gdppc"] * df["pop"]

    if save_processed:
        df.to_csv(processed_path, index=False)

    return df


def read_processed() -> pd.DataFrame:
    return pd.read_csv(processed_path)


if __name__ == "__main__":
    download_raw()
    raw_data = read_raw()
    process_raw(raw_data, save_processed=True)
