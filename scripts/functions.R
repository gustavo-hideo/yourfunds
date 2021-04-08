## ------------------------------
# FUNCTIONS
#
# Functions to be used in other scripts
#
## ------------------------------



## -- LIBRARIES
#devtools::install_github("jestonblu/RobinHood")
library(RobinHood)
#library(tidyquant)
library(tidyverse)
library(BatchGetSymbols)
library(lubridate)


# -- CREDENTIALS
source('C:/wandata/credentials/rhcred.R')
RH = RobinHood(username = USERNAME, password = PSSWD)



#
# -- 
# FUNCTIONS
# --
#



# --
# GRAPHS
# --


# Home graph for return with s&p500
HOME.VIZ.GROWTH <- function(data = my.year, sp500 = sp500.year, y.axis.acc = 1) {
    
    data %>% 
        ggplot(aes(begins_at, growth)) +
        geom_rect(aes(ymax = 0,
                      ymin = -Inf,
                      xmin = as.Date(min(begins_at), format="%Y-%m-%d"),
                      xmax = as.Date(max(begins_at), format="%Y-%m-%d")),
                  fill = 'firebrick',
                  alpha = 0.003) +
        geom_hline(yintercept = 0,
                   linetype = 'dotted',
                   color = 'firebrick',
                   alpha = 0.2) +
        geom_line(data = sp500, aes(ref.date, ret.adjusted.prices),
                  color = COLOR2, linetype = 'dashed',
                  alpha = ALPHA,
                  size = LINE.SIZE) +
        geom_line(color = COLOR,
                  size = LINE.SIZE) +
        scale_y_continuous(labels = scales::percent_format(accuracy = y.axis.acc)) +
        MY.THEME
    
}



# Search graph for adjusted closing price
SEARCH.VIZ.ADJ <- function(data) {
    
    data %>% 
        ggplot(aes(ref.date, price.adjusted, group = 1)) +
        geom_line(color = COLOR,
                  size = LINE.SIZE) +
        scale_x_date(date_labels = "%b/%y") +
        MY.THEME
    
}






# Search graph for return with s&p500
SEARCH.VIZ.GROWTH <- function(data, sp500, y.axis.acc = 1) {
    
    data %>% 
        ggplot(aes(ref.date, ret.adjusted.prices)) +
        geom_rect(aes(ymax = 0,
                      ymin = -Inf,
                      xmin = as.Date(min(ref.date), format="%Y-%m-%d"),
                      xmax = as.Date(max(ref.date), format="%Y-%m-%d")),
                  fill = 'firebrick',
                  alpha = 0.003) +
        geom_hline(yintercept = 0,
                   linetype = 'dotted',
                   color = 'firebrick',
                   alpha = 0.3) +
        geom_line(data = sp500, aes(ref.date, ret.adjusted.prices),
                  color = COLOR2, linetype = 'dashed',
                  alpha = ALPHA,
                  size = LINE.SIZE) +
        geom_line(color = COLOR,
                  size = LINE.SIZE) +
        scale_y_continuous(labels = scales::percent_format(accuracy = y.axis.acc)) +
        MY.THEME
    
}





# Search graph for balance sheet: assets vs liability
library(ggtext)


# BALANCE SHEET
SEARCH.BALANCE.VIZ <- function(data){
    
    balance.ratio <- data %>% 
        filter(date == max(date)) %>% 
        mutate(ratio = round(totalCurrentAssets / totalCurrentLiabilities, 1)) %>% 
        pull(ratio)
    
    data %>% 
        select(date, totalCurrentLiabilities, totalCurrentAssets) %>% 
        mutate(date = format(as.Date(date), "%Y-%m")) %>% 
        pivot_longer(cols = c(totalCurrentLiabilities, totalCurrentAssets),
                     names_to = "name", values_to = "value") %>% 
        ggplot(aes(date, value, group = name, color = name)) +
        geom_line(size = LINE.SIZE) +
        scale_color_manual(values = c(COLOR, RED)) +
        scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
        labs(title = paste("<span style='font-size:16pt'>Balance Sheet <br>
             <span style='color:#00c007;'>Current Assets</span>
             vs.
             <span style='color:#CD5C5C;'Current >Liability</span></span>
             <br>
             <span style='font-size:14pt'> Liabilities to assets ratio: ", balance.ratio, "</span>"
        )) +
        MY.THEME +
        theme(legend.position = 'none',
              plot.title = element_markdown())
    
}


# INCOME SHEET
SEARCH.INCOME.VIZ <- function(data){
    
    operating.margin <- data %>% 
        filter(date == max(date)) %>% 
        mutate(margin = round((operatingIncome / totalRevenue) * 100, 1)) %>% 
        pull(margin)
    
    data %>% 
        select(date, grossProfit, totalOperatingExpenses) %>% 
        mutate(date = format(as.Date(date), "%Y-%m")) %>% 
        pivot_longer(cols = c(grossProfit, totalOperatingExpenses),
                     names_to = "name", values_to = "value") %>% 
        ggplot(aes(date, value, group = name, color = name)) +
        geom_line(size = LINE.SIZE) +
        scale_color_manual(values = c(RED, COLOR)) +
        scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
        labs(title = paste("<span style='font-size:16pt'>Income Sheet <br>
             <span style='color:#00c007;'>Gross Profit</span>
             vs.
             <span style='color:#CD5C5C;'>Operating Expenses</span></span>
             <br>
             <span style='font-size:14pt'> Operating margin: ", operating.margin, "%</span>"
                           )) +
        MY.THEME +
        theme(legend.position = 'none',
              plot.title = element_markdown())
    
}


# EARNINGS
SEARCH.EARNINGS.VIZ <- function(data){
    
    data %>%
        ggplot(aes(date, estimate)) +
        geom_point(size = 5, color = COLOR, alpha = .3) +
        geom_point(aes(date, actual), color = COLOR, size = 5) +
        scale_color_manual(values = c(RED, COLOR)) +
        labs(title = paste("<span style='font-size:16pt'>Earnings
             <span style='color:#00c007;'>Actual</span>
             vs.
             <span style='color:#66ff6b;'>Estimate</span></span>")) +
        MY.THEME +
        theme(legend.position = 'none',
              plot.title = element_markdown())
    
}




# data <- income_sheet('AAPL')
# data <- tibble::rownames_to_column(data, 'date')
# 
# dat2 <- balance_sheet('AAPL')
# dat2 <- tibble::rownames_to_column(dat2, 'date')
# 
 data <- earnings_quarters('AAPL')






# --
# WATCH 52 WEEKS HIGH
# --

F.WATCH.52.HIGH <- function(tickers, f.date, l.date, freq, cache, folder, thresh.bad.data) {
    
    dat <- BatchGetSymbols(tickers = tickers, 
                           first.date = f.date,
                           last.date = l.date, 
                           freq.data = freq,
                           do.cache = cache,
                           cache.folder = folder,
                           thresh.bad.data = thresh.bad.data)
    
    files <- list.files(path=folder, pattern="*.rds", full.names=TRUE, recursive=FALSE)
    
    # watch.52 with only current values
    dat <- dat$df.tickers %>% 
        filter(ref.date == today) %>% 
        select(ticker,
               price.high.curr = price.high,
               price.close.curr = price.close)
    
    dat.highs <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(dat.highs) <- c('ticker, price.high')
    
    for (i in 1:length(files)){
        dat.files <- readRDS(files[i]) %>% 
            select(ticker,
                   #ref.date,
                   price.high) %>% 
            group_by(ticker) %>% 
            filter(price.high == max(price.high))
        
        dat.highs <- bind_rows(dat.files, dat.highs)
    }
    
    watch.52.highs <- dat.highs %>% 
        left_join(dat, by = 'ticker') %>% 
        select(ticker, price.high, price.high.curr) %>% 
        filter(price.high <= price.high.curr)
    
    unlink("./BGS_Cache_Watch", recursive = T)

    
    return(watch.52.highs)
    
}





# --
# ALL TICKERS 52 WEEKS HIGH
# --

F.ALL.52.HIGH <- function(tickers, f.date, l.date, freq, cache, folder) {
    
    dat <- BatchGetSymbols(tickers = tickers, 
                           first.date = f.date,
                           last.date = l.date, 
                           freq.data = freq,
                           do.cache = cache,
                           cache.folder = folder)
    
    files <- list.files(path=folder, pattern="*.rds", full.names=TRUE, recursive=FALSE)
    
    highs <- c()
    
    for (i in 1:length(files)) {
        dat <- readRDS(files[i])
        max.high <- max(dat$price.high)
        curr <- dat$price.high[dat$ref.date == today]
        
        if (length(curr) == 1){
            if (curr >= max.high){
                highs <- c(highs, unique(dat$ticker))
            }
        }
    }
    

    
    return(highs)
    
}














