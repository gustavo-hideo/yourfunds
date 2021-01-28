# --
# -- yourfunds
# --



# ----------------------------
## -- Notes

# -- List of all NASDAQ stocks
# http://ftp.nasdaqtrader.com/Trader.aspx?id=symbollookup

# -- Investopedia
# https://www.investopedia.com/


# -- Test issue
# https://www.investopedia.com/terms/t/test.asp#:~:text=What%20Is%20a%20Test%3F,level%20set%20by%20the%20market.&text=However%2C%20if%20the%20stock%20price,new%20highs%2C%20the%20test%20fails.


# -- todo
# How RobinHood calculate percent change (increase/decrease). Based on when? R: closing price last day
# How to use 52 weeks averages/lows/highs
# Why 52 weeks


# -- Data dictionary
# http://www.nasdaqtrader.com/trader.aspx?id=symboldirdefs

# ----------------------------





## -- Libraries
#devtools::install_github("jestonblu/RobinHood")
library(RobinHood)
#library(tidyquant)
library(tidyverse)
library(BatchGetSymbols)
library(lubridate)

# -- Getting and using my credentials
source('C:\wandata\credentials\rhcred.R')
RH = RobinHood(username = USERNAME, password = PSSWD)






## -- Getting stocks symbols

# -- From NASDAQ
nasdaq.listed <- read_delim('http://ftp.nasdaqtrader.com/dynamic/SymDir/nasdaqlisted.txt',
                           delim='|') %>% 
  filter(!is.na(`Security Name`))

# -- From others
other.listed <- read_delim('http://ftp.nasdaqtrader.com/dynamic/SymDir/otherlisted.txt',
                           delim='|') %>% 
  filter(!is.na(`Security Name`))

# -- Merging all symbols
all.symbols <- append(nasdaq.listed$Symbol, other.listed$`NASDAQ Symbol`)

# -- ALL TICKERS AVAILABLE IN ROBINHOOD
all.tickers <- get_tickers(RH)



## -- Vars
today <- today()
week <- today %m-% weeks(1)
week.52 <- today %m-% weeks(52)
month <- today %m-% months(1)
year <- today %m-% years(1)
year.5 <- today %m-% years(5)
year.10 <- today %m-% years(10)




## -- Summaries
# Market value: Total amount I have invested
# Withdrawable amount: Buying power
# Equity: Total amount I have in the account (market value + withdrawable amount)

# -- Equity
equity <- get_portfolios(RH)$equity

# -- Transfers
# Getting completed deposits (deposit/withdraw) and amount ($)
deposits <- get_ach(RH, action = "transfers") %>% 
  select(direction, amount, state, fees) %>% 
  filter(state == 'completed',
         direction == 'deposit') %>% 
  mutate(amount = amount - fees) %>% 
  select(amount) %>% 
  summarise_all(.funs = sum) %>% 
  as.numeric()

# -- Revenue
revenue <- equity - deposits

# -- Percent growth
percent_growth <- round((revenue / deposits) * 100, 1)



## -- My funds
# Cost: How much I paid total
# Current value: Current value of all shares
# Last trade price: last price of 1 share
my.funds <- get_positions(RH) %>% 
  mutate(revenue = current_value - cost) %>% 
  mutate(growth = round((revenue/cost) * 100, 1))

## - Only symbols
my.symbols <- as.vector(my.funds$symbol)

## -- Watchlists
# This command is not returning the watchlists
# Reach out to community or creator
my.watchlists <- watchlist(RH, action='get')

## -- Company details
# average_volume_2_weeks
# average_volme
# high_52_weeks
# low_52_weeks
# dividend_yield (get at tidyquant using tq_get('symbol', get = "dividends"))
# market_cap
# more more more
get_fundamentals(RH, my.symbols)



## --  Historical data

# -- solving issue of GetSymbols with google as data source
invisible(Sys.setlocale("LC_MESSAGES", "C"))
invisible(Sys.setlocale("LC_TIME", "C"))

# -- My funds
search <- BatchGetSymbols(tickers = my.symbols, 
                first.date = year,
                last.date = today+1, 
                freq.data = 'daily',
                do.cache = F)





## -- 52 WEEKS HIGH








## ------------------------------------
# FUNCTION TO RUN EVERY TIME WATCH 52 WEEKS IS OPENED


##
# 52 weeks high for companies in watchlists
##

#start.time <- Sys.time()
watch.52.curr <- BatchGetSymbols(tickers = my.symbols, 
                                   first.date = week.52,
                                   last.date = today+1, 
                                   freq.data = 'daily',
                                   do.cache = T,
                                   cache.folder = './BGS_Cache_Watch')
#end.time <- Sys.time()
#(duration <- end.time - start.time)  #

watch.files <- list.files(path="./BGS_Cache_Watch", pattern="*.rds", full.names=TRUE, recursive=FALSE)


# watch.52 with only current values
watch.52.curr <- watch.52.curr$df.tickers %>% 
  filter(ref.date == today) %>% 
  select(ticker,
         price.high.curr = price.high,
         price.close.curr = price.close)

watch.highs <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(watch.highs) <- c('ticker, price.high')

for (i in 1:length(watch.files)){
  
  # need to compile all files in one big data.frame
  # then when the data frame is completed with all symbols and highs over the last 52 weeks, it will
  # merge with the watch.52.curr and flag those symbols where the price.high.curr beat the highest in 52 weeks
  dat <- readRDS(watch.files[i]) %>% 
    select(ticker,
           #ref.date,
           price.high) %>% 
    group_by(ticker) %>% 
    filter(price.high == max(price.high))
  
  watch.highs <- bind_rows(dat, watch.highs)

  }
  
dat.and.watch <- watch.highs %>% 
  left_join(watch.52.curr, by = 'ticker') %>% 
  select(ticker, price.high, price.high.curr) %>% 
  filter(price.high <= price.high.curr)

rm(watch.52.curr,
   watch.files,
   dat,
   watch.highs)

# Deleting cache to update prices when running again
unlink("./BGS_Cache_Watch", recursive = T)

## ------------------------------------










## ------------------------------------
# FUNCTION TO RUN EVERY NIGHT TO GET ALL STOCKS THAT REACHED 52 WEEKS HIGH IN THE PREVIOUS CLOSING


###
# 52 weeks high for all companies
###

start.time <- Sys.time()
all.52 <- BatchGetSymbols(tickers = all.symbols, 
                          first.date = today,
                          last.date = today+1, 
                          freq.data = 'daily',
                          do.cache = T,
                          cache.folder = './BGS_Cache_curr')
end.time <- Sys.time()
(duration <- end.time - start.time)  # 2.12 hrs for 52 weeks of all.symbols

files <- list.files(path="./BGS_Cache", pattern="*.rds", full.names=TRUE, recursive=FALSE)


  
highs <- c()

for (i in 1:length(files)){
  
  dat <- readRDS(files[i])
  max.high <- max(dat$price.high)
  curr <- dat$price.high[dat$ref.date == today-3]
  
  if (length(curr) == 1){
      if (curr >= max.high){
      highs <- c(highs, unique(dat$ticker))
      }
  }
  
}
  










# -------------------------------

## -- Viz



## -- COMPARING STOCKS























