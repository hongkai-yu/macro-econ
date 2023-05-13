import pandas as pd

from python.util import download_file, DATA_FOLDER

INDICATOR_CODE_MAP = {
    "gdp": "GDP",
    "real_gdp": "GDPC1",
    "cpi": "CPIAUCSL",
    "core_cpi": "CPILFESL",
    "m1": "M1SL",
    "m2": "M2SL",
    "federal_funds_rate": "DFF",
    "interest_rate_3month": "TB3MS",
    "interest_rate_5year": "GS5",
    "interest_rate_10year": "GS10",
    "total_public_debt": "GFDEBTN",
}
def raw_path(indicator: str):
    return f"{DATA_FOLDER}/raw/{indicator}.csv"

def processed_path(indicator: str):
    return f"{DATA_FOLDER}/processed/{indicator}.csv"

def indicator_to_code(indicator: str):
    return INDICATOR_CODE_MAP.get(indicator, indicator)


def download_raw(indicator: str):
    url = f"https://fred.stlouisfed.org/graph/fredgraph.csv?id={indicator_to_code(indicator)}"
    download_file(url, raw_path(indicator))


def read_raw(indicator: str) -> pd.DataFrame:
    return pd.read_csv(raw_path(indicator))


def process_raw(df: pd.DataFrame, indicator: str, save_processed: bool = False) -> pd.DataFrame:
    headers = ["date", indicator]
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
