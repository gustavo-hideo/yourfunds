## ------------------------------
# GLOBAL
#
# Runs all the time
#
## ------------------------------



## -- LIBRARIES
#devtools::install_github("jestonblu/RobinHood")
library(RobinHood)
#library(tidyquant)
library(tidyverse)
library(BatchGetSymbols)
library(lubridate)
library(TTR)
library(quantmod)
library(reactable)
library(sparkline)
library(reticulate)

# library(shinymanager)
# 
# 
# # -- CREDENTIALS
# 
# inactivity <- "function idleTimer() {
# var t = setTimeout(logout, 120000);
# window.onmousemove = resetTimer; // catches mouse movements
# window.onmousedown = resetTimer; // catches mouse movements
# window.onclick = resetTimer;     // catches mouse clicks
# window.onscroll = resetTimer;    // catches scrolling
# window.onkeypress = resetTimer;  //catches keyboard actions
# 
# function logout() {
# window.close();  //close the window
# }
# 
# function resetTimer() {
# clearTimeout(t);
# t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
# }
# }
# idleTimer();"
# 
# 
# # data.frame with credentials info
# credentials <- data.frame(
#     user = c("1", "fanny", "victor", "gustavo"),
#     password = c("1", "azerty", "12345", "bruna"),
#     # comment = c("alsace", "auvergne", "bretagne"), %>% 
#     stringsAsFactors = FALSE
# )


source('C:/wandata/credentials/rhcred.R')
RH = RobinHood(username = USERNAME, password = PSSWD)





# -- 
# GLOBAL VARS
# --

## -- COLORS
COLOR <- '#00c007'
COLOR2 <- '#808080'
COLOR3 <- '#f2f2f2'
RED <- '#CD5C5C'
ALPHA <-  .6
LINE.SIZE <- .8
POINT.SIZE <- 3


## -- TITLES
title <- 'yourfunds'
subtitle <- 'Money! Money!'


## -- THEME
MY.THEME <- theme(plot.background = element_blank(),
                  panel.background = element_blank(),
                  panel.grid = element_blank(),
                  #panel.grid.major.x = element_line(color = COLOR3),
                  #panel.grid.minor.x = element_line(color = COLOR3),
                  axis.ticks = element_blank(),
                  axis.text = element_text(color = COLOR2),
                  axis.title = element_blank(),
                  legend.key = element_blank(),
                  legend.position = 'top',
                  legend.justification = 'left',
                  legend.title = element_blank(),
                  legend.text = element_text(color = COLOR2),
                  plot.title = element_text(color = COLOR2,
                                            hjust = 0))

## -- DATES
today <- today()
week <- today %m-% weeks(1)
week.52 <- today %m-% weeks(52)
month <- today %m-% months(1)
month.2 <- today %m-% months(2)
month.6 <- today %m-% months(6)
year <- today %m-% years(1)
year.5 <- today %m-% years(5)
year.10 <- today %m-% years(10)



# -- ALL TICKERS
all.tickers <- read_rds("C:/wandata/yourfunds/all_tickers.rds")


# -- WATCHLISTS
my.watchlists <- watchlist(RH, action='get')

# -- Getting all watched stocks
my.watch.tickers <- c()
i <- 1
for (watch in my.watchlists) {
    for (ticker in watchlist(RH, action='get', watchlist = watch)) {
        my.watch.tickers[i] <- ticker
        i <- i + 1
    }
}


## -- FREEZE COLUMNS REACTABLE
FREEZE_STYLE <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                     borderRight = "1px solid #eee")






## --
# MARKET INDEX (S&P 500)
## --

sp500.raw.week <- BatchGetSymbols(tickers = '^GSPC', 
                                   first.date = week-1,
                                   last.date = today+1, 
                                   freq.data = 'daily',
                                   do.cache = F,
                                   thresh.bad.data = 0)

sp500.raw.month <- BatchGetSymbols(tickers = '^GSPC', 
                         first.date = floor_date(month-day(today()), 'week'),
                         last.date = today+1, 
                         freq.data = 'weekly',
                         do.cache = F,
                         thresh.bad.data = 0)

sp500.raw.year <- BatchGetSymbols(tickers = 'TSLA', 
                         first.date = floor_date(year-day(today()), 'month'),
                         last.date = today+1, 
                         freq.data = 'monthly',
                         do.cache = F,
                         thresh.bad.data = 0)

sp500.raw.year10 <- BatchGetSymbols(tickers = '^GSPC', 
                         first.date = floor_date(year.10-day(today()), 'month'),
                         last.date = today+1, 
                         freq.data = 'yearly',
                         do.cache = F,
                         thresh.bad.data = 0)




sp500.last.return <- sp500.raw.month$df.tickers %>% 
    filter(ref.date == max(ref.date)) %>% 
    pull(ret.adjusted.prices)

sp500.week <- sp500.raw.week$df.tickers %>% 
    select(ref.date,
           ret.adjusted.prices)

sp500.month <- sp500.raw.month$df.tickers %>%
    select(ref.date,
           ret.adjusted.prices)

sp500.6months <- sp500.raw.year$df.tickers %>% 
    filter(ref.date >= month.6) %>% 
    select(ref.date,
           ret.adjusted.prices)

sp500.year <- sp500.raw.year$df.tickers %>% 
    select(ref.date,
           ret.adjusted.prices)


sp500.5years <- sp500.raw.year10$df.tickers %>% 
    filter(ref.date >= year.5) %>% 
    select(ref.date,
           ret.adjusted.prices)

sp500.10years <- sp500.raw.year10$df.tickers %>% 
    select(ref.date,
           ret.adjusted.prices)






# -- SUMMARIES

# Equity
equity <- get_portfolios(RH)$equity

# Market value
market.value <- round(get_portfolios(RH)$market_value, 2)

# Buying power
buying.power <- get_portfolios(RH)$withdrawable_amount

# Transfers
deposits <- get_ach(RH, action = "transfers") %>% 
    select(direction, amount, state, fees) %>% 
    filter(state == 'completed',
           direction == 'deposit') %>% 
    mutate(amount = amount - fees) %>% 
    select(amount) %>% 
    summarise_all(.funs = sum) %>% 
    as.numeric()

# Revenue
revenue <- round(equity - deposits, 2)

# Percent growth
percent_growth <- round((revenue / deposits) * 100, 1)


# -- MY FUNDS
my.funds <- get_positions(RH) %>% 
    mutate(revenue = current_value - cost) %>% 
    mutate(growth = revenue/cost)

# Only tickers
my.tickers <- as.vector(my.funds$symbol)

# Watchlists
my.watchlists <- watchlist(RH, action='get')

# Companies info
my.funds.info <- get_fundamentals(RH, my.tickers)





# -- 
# HISTORICAL DATA
# --
# 
# # solving issue of GetSymbols with google as data source
invisible(Sys.setlocale("LC_MESSAGES", "C"))
invisible(Sys.setlocale("LC_TIME", "C"))

# -- MY HISTORICAL FUNDS
my.hist <- BatchGetSymbols(tickers = my.tickers,
                          first.date = month,
                          last.date = today+1,
                          freq.data = 'daily',
                          do.cache = F,
                          thresh.bad.data = 0)
# 
# 
# 
# 
# 
# # --
# # WATCHLIST 52 WEEKS HIGH
# # --
# 
# watch.52.high <- F.WATCH.52.HIGH(my.tickers,
#                                  week.52,
#                                  today+1,
#                                  'daily',
#                                  T,
#                                  './BGS_Cache_Watch',
#                                  thresh.bad.data = 0)
# 
# 









