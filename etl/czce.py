# 郑州商品交易所
# http://www.czce.com.cn/cn/jysj/lshqxz/H770319index_1.htm

import pandas as pd
from etl.util import DATA_FOLDER


def read_year(year: int):
    file_path = f"{DATA_FOLDER}/raw/ALLFUTURES{year}.txt"
    # Read the file into a pandas DataFrame
    # Skipping the title row and using the second row as headers
    df = pd.read_csv(file_path, sep='|', header=1, thousands=',')
    column_names_eng = [
        "trade_date",
        "contract_code",
        "previous_settlement",
        "open",
        "high",
        "low",
        "close",
        "settlement",
        "change_1",
        "change_2",
        "volume_hands",
        "open_interest",
        "change_in_oi",
        "turnover_10k_cny",
        "delivery_settlement_price"
    ]
    df.columns = column_names_eng
    df = df.applymap(lambda x: x.strip() if isinstance(x, str) else x)  # clean up whitespace
    return df


if __name__ == "__main__":
    years = [2021, 2022, 2023]
    symbol = "UR"
    df = (pd.concat([read_year(year) for year in years])
          .query(f"contract_code.str.startswith('{symbol}')")
          .reset_index(drop=True)
          .to_csv(f"{DATA_FOLDER}/processed/{symbol}.csv", index=False))

