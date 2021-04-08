# --
# -- yourfunds
# --








## -- Libraries
#devtools::install_github("jestonblu/RobinHood")
library(RobinHood)
#library(tidyquant)
library(tidyverse)
library(BatchGetSymbols)
library(lubridate)

# -- Getting and using my credentials
source('C:/wandata/credentials/rhcred.R')
RH = RobinHood(username = USERNAME, password = PSSWD)




## -------------------
# -- Stats and info for specific stock
#get_fundamentals(RH, "AAPL", T)






# -------------------------------
## -- Visuals




## -- HOME VIZ
# Converting date time to the correct time zone
# attr(my.today$begins_at, "tzone") <- "us/mountain"

my.year <- get_portfolios(RH, interval = 'month', span = 'year') %>%
  select(begins_at, close_market_value) %>%  
  rbind(c(as.character(today), market.value)) %>% 
  mutate(begins_at = as.Date(begins_at),
         close_market_value = as.numeric(close_market_value),
         previous.close = as.numeric(lag(close_market_value))) %>% 
  mutate(growth = (close_market_value-previous.close)/previous.close) %>%
  mutate(growth = replace_na(growth, 0))
  


my.month <- get_portfolios(RH, interval = 'week', span = 'month') %>%
  select(begins_at, close_market_value) %>%  
  rbind(c(as.character(today), market.value)) %>% 
  mutate(begins_at = as.Date(begins_at),
         close_market_value = as.numeric(close_market_value),
         previous.close = as.numeric(lag(close_market_value))) %>% 
  mutate(growth = (close_market_value-previous.close)/previous.close) %>%
  mutate(growth = replace_na(growth, 0))


my.5days <- get_portfolios(RH, interval = 'day', span = 'week') %>%
  select(begins_at, close_market_value) %>%  
  rbind(c(as.character(today), market.value)) %>% 
  mutate(begins_at = as.Date(begins_at),
         close_market_value = as.numeric(close_market_value),
         previous.close = as.numeric(lag(close_market_value))) %>% 
  mutate(growth = (close_market_value-previous.close)/previous.close) %>%
  mutate(growth = replace_na(growth, 0))







## -- ALL TICKERS
all.tickers <- read_rds('./all_tickers.rds')



## -- WATCHLISTS TABLE

my.funds$last.close <- NA
i <- 1
for (ticker in my.tickers) {
  last.close <- get_historicals(RH, ticker, interval = 'day', span = 'week') %>% 
    filter(begins_at != today) %>% 
    filter(begins_at == max(begins_at)) %>% 
    pull(close_price)
  
  my.funds$last.close[i] <- last.close
  
  i <- i + 1
}

my.funds$last.close

# current.value = balance
# quantity = #shares
# last_trade_price = current price
# cost = how much I paid total
# revenue = balance - cost (how much is my revenue)
# growth = percent growth when I bought until now

my.funds$last.perc_change <- (my.funds$last_trade_price - my.funds$last.close) / my.funds$last.close
my.funds$compared.sp500 <- (my.funds$last.perc_change - sp500.last.return) * 100

my.hist.price <- my.hist$df.tickers %>% 
  select(Ticker = ticker, ref.date, price.adjusted) %>% 
  filter(ref.date >= week) %>% 
  group_by(Ticker) %>% 
  summarise(`5 days adjusted` = list(price.adjusted))

my.hist.change <- my.hist$df.tickers %>% 
  select(Ticker = ticker, ref.date, ret.adjusted.prices) %>% 
  filter(ref.date >= month) %>% 
  mutate(ret.adjusted.prices = round(ret.adjusted.prices * 100, 3)) %>% 
  group_by(Ticker) %>% 
  summarise(`30 days change` = list(ret.adjusted.prices))

my.info <- my.funds.info %>% 
  select(Ticker = symbol,
         `Market Cap.` = market_cap,
         `P/E Ratio` = pe_ratio)

home.table <- my.funds %>% 
  select(Ticker = symbol,
         Change = last.perc_change,
         Price = last_trade_price,
         Shares = quantity,
         Balance = current_value,
         Revenue = revenue,
         Return = growth,
         `Compared S&P500 (percent points)` = compared.sp500) %>% 
  left_join(my.hist.price, by = 'Ticker') %>% 
  left_join(my.hist.change, by = 'Ticker') %>% 
  left_join(my.info, by = 'Ticker')



# TABLE
library(reactable)
library(sparkline)

home.react <- reactable(home.table,
          highlight = T,
          columns = list(
            
            Ticker = colDef(
              style = FREEZE_STYLE,
              headerStyle = FREEZE_STYLE
            ),
            Change = colDef(
              format = colFormat(percent = T, digits = 2),
              style = function(value) {
                if (value > 0) {
                  color <- '#0080ff'
                }
                else if (value <0) {
                  color <- '#e00000'
                } else {
                  color <- '#808080'
                }
                list(color = color)
              }
            ),
            Price = colDef(
              format = colFormat(currency = 'USD', separator = T, locales = 'en-US')
            ),
            Shares = colDef(
              format = colFormat(digits = 3, separator = T)
            ),
            Balance = colDef(
              format = colFormat(currency = 'USD', separator = T, locales = 'en-US')
            ),
            Revenue = colDef(
              format = colFormat(currency = 'USD', separator = T, locales = 'en-US')
            ),
            Return = colDef(
              format = colFormat(percent = T, digits = 2)
            ),
            `Compared S&P500 (percent points)` = colDef(
              format = colFormat(digits = 4)
            ),
            `Market Cap.` = colDef(
              format = colFormat(currency = 'USD', separator = T, locales = 'en-US')
            ),
            `P/E Ratio` = colDef(
              format = colFormat(digits = 2)
            ),
            `5 days adjusted` = colDef(cell = function(value, index) {
              sparkline(home.table$`5 days adjusted`[[index]])
            }),
            `30 days change` = colDef(cell = function(value, index) {
              sparkline(home.table$`30 days change`[[index]], type = 'box')
            })
          ))









## PYTHON



library(reticulate)
#py_install("yahoofinancials", pip = TRUE)

source_python('scripts/python_functions.py')





##




## -- COMPARING STOCKS
# 
# bynd <- BatchGetSymbols(tickers = 'BYND', 
#                            first.date = year.5,
#                            last.date = today+1, 
#                            freq.data = 'daily',
#                            do.cache = F,
#                            thresh.bad.data = 0)
# 
# mu <- BatchGetSymbols(tickers = 'MU', 
#                            first.date = year.5-1,  #get the previous day of the initial day to show the percent change
#                            last.date = today+1, 
#                            freq.data = 'monthly',
#                            do.cache = F,
#                           thresh.bad.data = 0)
# 
# 



# search <- bind_rows(bynd$df.tickers,
#                     mu$df.tickers) %>% 
#   select(ticker, ref.date, ret.closing.prices) %>% 
#   filter(!is.na(ret.closing.prices))
# 
# 
# search %>% 
#   ggplot(aes(ref.date, ret.closing.prices, color = ticker)) +
#   geom_line(size = .8) +
#   scale_color_brewer(palette = 'Pastel2') +
#   # Convert y-axis to percentage
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(title = 'Monthly percent change') +
#   MY.THEME

















