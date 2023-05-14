import json
import time
from itertools import product

import pandas as pd
import requests

from python.util import DATA_FOLDER, ALPHA_VANTAGE_API_KEY

query_url = "https://www.alphavantage.co/query"
default_query_params = {
    "outputsize": "full",
    "datatype": "json",
    "apikey": ALPHA_VANTAGE_API_KEY,
}


def raw_path(symbol: str, function: str):
    return f"{DATA_FOLDER}/raw/{symbol}_{function}.json"


def processed_path(symbol: str, function: str):
    return f"{DATA_FOLDER}/processed/{symbol}_{function}.csv"


def download_raw(symbol: str, function: str = "TIME_SERIES_DAILY_ADJUSTED", **kwargs) -> dict:
    params = default_query_params.copy()
    params["function"] = function
    params["symbol"] = symbol
    params.update(kwargs)
    url = query_url + "?" + "&".join([f"{k}={v}" for k, v in params.items()])
    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        # save as json
        path = raw_path(symbol, function)
        with open(path, 'w') as file:
            json.dump(data, file)
        print(f"File downloaded to {path}.")
        return data
    else:
        print("Failed to query Alpha Vantage.")
        return {}


def read_raw(symbol: str, function: str = "TIME_SERIES_DAILY_ADJUSTED") -> dict:
    with open(raw_path(symbol, function), 'r') as file:
        return json.load(file)


def process_raw(data: dict, function: str = "TIME_SERIES_DAILY_ADJUSTED", save_processed: bool = False) -> pd.DataFrame:
    symbol = data["Meta Data"]["2. Symbol"]
    if function == "TIME_SERIES_DAILY_ADJUSTED":
        df = pd.DataFrame(data["Time Series (Daily)"]).transpose()
        headers = ["open", "high", "low", "close", "adjusted_close", "volume", "dividend_amount", "split_coefficient"]
        df.columns = headers
        df = df.astype(float)
    else:
        raise NotImplementedError(f"Function {function} not implemented.")

    if save_processed:
        df.to_csv(processed_path(symbol, function), index=True)

    return df


def read_processed(symbol: str, function: str = "TIME_SERIES_DAILY_ADJUSTED") -> pd.DataFrame:
    return pd.read_csv(processed_path(symbol, function))


if __name__ == "__main__":
    symbols = ["AAPL", "MSFT", "AMZN", "GOOG", "FB", "TSLA", "BRK.A", "NVDA", "JPM", "JNJ"]
    functions = ["TIME_SERIES_DAILY_ADJUSTED"]
    # functions = ["TIME_SERIES_DAILY_ADJUSTED", "OVERVIEW", "INCOME_STATEMENT", "BALANCE_SHEET", "CASH_FLOW"]
    query_counter = 0
    for s, f in product(symbols, functions):
        raw_data = download_raw(s, f)
        if raw_data:
            query_counter += 1
            process_raw(raw_data, f, save_processed=True)

        if query_counter % 5 == 0:
            print("Sleeping for 60 seconds to avoid API limit.")
            time.sleep(60)

        if query_counter > 500:
            print("Reached 500 queries, exiting.")
            break
