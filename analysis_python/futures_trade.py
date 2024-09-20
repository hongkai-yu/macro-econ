from etl.util import DATA_FOLDER

import pandas as pd
import matplotlib.pyplot as plt

# Read the CSV file into a pandas DataFrame
file_path = f"{DATA_FOLDER}/processed/UR.csv"
df = pd.read_csv(file_path)


def init_status():
    return {
        'position': 0,
        'entry_date': None,
        'entry_price': None,
        'exit_price': None,
    }


def long(row, status):
    status['position'] = 1
    status['entry_date'] = row.trade_date
    status['entry_price'] = row.open
    status['exit_price'] = row.ref_h_1


def short(row, status):
    status['position'] = -1
    status['entry_date'] = row.trade_date
    status['entry_price'] = row.open
    status['exit_price'] = row.ref_l_1


def exit_position(exit_price, row, status) -> dict:
    if status['position'] == 1:
        trade_type = 'long'
        profit = exit_price - status['entry_price']
    elif status['position'] == -1:
        trade_type = 'short'
        profit = status['entry_price'] - exit_price
    else:
        raise Exception('No position to exit')
    status['position'] = 0
    return {'entry_date': status['entry_date'], 'exit_date': row.trade_date, 'profit': profit, 'type': trade_type}


# Function to process trades for a single contract
def process_trades(contract_data):
    contract_data['ref_c_5'] = contract_data['close'].shift(5)
    contract_data['ref_c_4'] = contract_data['close'].shift(4)
    contract_data['ref_h_1'] = contract_data['high'].shift(1)
    contract_data['ref_l_1'] = contract_data['low'].shift(1)

    status = init_status()
    trades = []

    for row in contract_data.itertuples():
        # Get in
        if status['position'] == 0:
            if row.open > row.ref_c_5 and row.open > row.ref_c_4:
                long(row, status)
            elif row.open < row.ref_c_5 and row.open < row.ref_c_4:
                short(row, status)

        # reverse
        if status['position'] == 1 and row.open < row.ref_c_5 and row.open < row.ref_c_4:
            trades.append(exit_position(row.open, row, status))
            short(row, status)
        elif status['position'] == -1 and row.open > row.ref_c_5 and row.open > row.ref_c_4:
            trades.append(exit_position(row.open, row, status))
            long(row, status)

        # hit stop loss
        if (status['position'] == 1 and row.high > status['exit_price']) or (
                status['position'] == -1 and row.low < status['exit_price']):
            trades.append(exit_position(status['exit_price'], row, status))
            status = init_status()

    return pd.DataFrame(trades)


# Function to calculate overall profitability for a contract
def calculate_profitability(trades_df):
    return trades_df['profit'].sum()


# Function to calculate each trade's win/loss
def calculate_win_loss(trades_df):
    wins = trades_df[trades_df['profit'] > 0]['profit'].tolist()
    losses = trades_df[trades_df['profit'] < 0]['profit'].tolist()
    return wins, losses


# Function to plot cumulative net gain/loss for a contract
def plot_cumulative_profit(trades_df, contract_code):
    cumulative_profit = trades_df['profit'].cumsum()
    plt.figure(figsize=(10, 5))
    cumulative_profit.plot()
    plt.title(f'Cumulative Profit/Loss for {contract_code}')
    plt.xlabel('Trade Number')
    plt.ylabel('Cumulative Profit/Loss')
    plt.grid(True)
    plt.show()


# Script to apply the functions to each contract and output the results
contract_results = {}
for contract_code, contract_data in df.groupby('contract_code'):
    contract_trades_df = process_trades(contract_data)
    contract_profitability = calculate_profitability(contract_trades_df)
    contract_wins, contract_losses = calculate_win_loss(contract_trades_df)
    contract_results[contract_code] = {
        'Profitability': contract_profitability,
        'Wins': contract_wins,
        'Losses': contract_losses
    }
    # plot_cumulative_profit(contract_trades_df, contract_code)
for contract_code, contract_result in contract_results.items():
    print(f'{contract_code}: {contract_result["Profitability"]}')

contract_data = df[df['contract_code'] == 'UR401'].reset_index()
contract_trades_df = process_trades(contract_data)
# aa_res = contract_results['UR401']
