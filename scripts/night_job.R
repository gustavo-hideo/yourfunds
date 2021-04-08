## ------------------------------
# NIGHT JOB
#
# Script to run every night
#
## ------------------------------


## -- Libraries
#devtools::install_github("jestonblu/RobinHood")
library(RobinHood)
library(tidyverse)
library(BatchGetSymbols)
library(lubridate)


# -- Getting and using my credentials
source('C:/wandata/credentials/rhcred.R')
RH = RobinHood(username = USERNAME, password = PSSWD)


## --
# Get all tickers
## --
all.tickers <- get_tickers(RH)$symbol
write_rds(all.tickers, here::here("./all_tickers.rds"))


start.time <- Sys.time()
test <- F.ALL.52.HIGH(all.tickers,
                      week.52,
                      today+1,
                      'daily',
                      T,
                      './BGS_Cache')
end.time <- Sys.time()
(duration <- end.time - start.time)  # 2.12 hrs for 52 weeks of all.symbols



































