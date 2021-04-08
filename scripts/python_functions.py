# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 12:46:35 2021

@author: gusta
"""


import pandas as pd
from yahoofinancials import YahooFinancials


#ticker = 'AAPL'

#yahoo_financials = YahooFinancials(ticker)



def to_df(dat, sheet_type, ticker):
    dataframe_entries = list()
    for result in dat.get(sheet_type).get(ticker):
        extracted_date = list(result)[0]
        dataframe_row = list(result.values())[0]
        dataframe_row['date'] = extracted_date
        dataframe_entries.append(dataframe_row)
    
    df = pd.DataFrame(dataframe_entries).set_index('date')
    return df


def balance_sheet(ticker):
    yahoo_financials = YahooFinancials(ticker)
    dat = yahoo_financials.get_financial_stmts('annual', 'balance')
    df = to_df(dat, 'balanceSheetHistory', ticker)
    return df


def income_sheet(ticker):
    yahoo_financials = YahooFinancials(ticker)
    dat = yahoo_financials.get_financial_stmts('annual', 'income')
    df = to_df(dat, 'incomeStatementHistory', ticker)
    return df


def cash_sheet(ticker):
    yahoo_financials = YahooFinancials(ticker)
    dat = yahoo_financials.get_financial_stmts('annual', 'cash')
    df = to_df(dat, 'cashflowStatementHistory', ticker)
    return df


def earnings_quarters(ticker):
    yahoo_financials = YahooFinancials(ticker)
    dat = yahoo_financials.get_stock_earnings_data()
    dataframe_entries = list()
    for result in dat.get(ticker).get('earningsData').get('quarterly'):
        extracted_date = list(result.values())
        dataframe_entries.append(extracted_date)
    
    df = pd.DataFrame(dataframe_entries)
    df.columns = ['date', 'actual', 'estimate']
    return df
    





#balance = balance_sheet()
#income = income_sheet()
#cash = cash_sheet()
#earnings = earnings_quarters()


# 
# yahoo_financials = YahooFinancials('AAPL')
# dat = yahoo_financials.get_financial_stmts('annual', 'balance')
# df = to_df(dat, 'balanceSheetHistory')
# 
# 
# dataframe_entries = list()
# 
# for result in dat.get('balanceSheetHistory').get('AAPL'):
#     extracted_date = list(result)[0]
#     dataframe_row = list(result.values())[0]
#     dataframe_row['date'] = extracted_date
#     dataframe_entries.append(dataframe_row)
# 
# df = pd.DataFrame(dataframe_entries).set_index('date')
# 








