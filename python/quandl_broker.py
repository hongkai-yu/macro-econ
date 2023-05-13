import pandas as pd
import quandl

from python.util import DATA_FOLDER

INDICATOR_CODE_MAP = {
    "gold": "LBMA/GOLD",
}


def raw_path(indicator: str):
    return f"{DATA_FOLDER}/raw/{indicator}.csv"


def processed_path(indicator: str):
    return f"{DATA_FOLDER}/processed/{indicator}.csv"


def indicator_to_code(indicator: str):
    return INDICATOR_CODE_MAP.get(indicator, indicator)


def download_raw(indicator: str):
    df = quandl.get(indicator_to_code(indicator))
    df.to_csv(raw_path(indicator))


def read_raw(indicator: str) -> pd.DataFrame:
    return pd.read_csv(raw_path(indicator))


def process_raw(df: pd.DataFrame, indicator: str, save_processed: bool = False) -> pd.DataFrame:
    if indicator == "gold":
        headers = ["date", "usd_am", "usd_pm", "gbp_am", "gbp_pm", "eur_am", "eur_pm"]
    else:
        headers = df.columns
    df.columns = headers
    df["date"] = df["date"].astype(str).str.replace('-', '/')

    if save_processed:
        df.to_csv(processed_path(indicator), index=False)

    return df


def read_processed(indicator: str) -> pd.DataFrame:
    return pd.read_csv(processed_path(indicator))


if __name__ == "__main__":
    for data_name in INDICATOR_CODE_MAP:
        download_raw(data_name)
        raw_data = read_raw(data_name)
        process_raw(raw_data, indicator=data_name, save_processed=True)
