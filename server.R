


library(shiny)
library(shinyWidgets)
library(shinycssloaders)



server <- function(input, output) {
  

  
  ###################################################
  
  # # create userbase
  # user_base_basic_tbl <- tibble(
  #   user_name = "u",
  #   password  = "p"
  # )
  
  # check credentials vs tibble
  validate_password_basic <- eventReactive(input$ab_login_button_basic, {
    
    validate <- FALSE
    
    if (input$ti_user_name_basic == USERNAME &&
        input$ti_password_basic == PSSWD )
    {validate <- TRUE}
    
   # login.try <- RobinHood(username = input$ti_user_name_basic,
   #            password = input$ti_password_basic)
   # 
   #  if(is.list(login.try)) {
   #    validate <- T
   #  }
   
    
    validate
  })
  
  output$login <- reactive({
    if (validate_password_basic()) {
      TRUE
    }
  })
  
  outputOptions(output, "login", suspendWhenHidden = FALSE)
  
  
  # hide form 
  observeEvent(validate_password_basic(), {
    
    shinyjs::hide(id = "login_basic")
  })
  
  
  
  
  
  
  ############################
    
    
    # --
    # ---------------
    # HOME PLOT
    # ---------------
    # --
    
    output$plot.home<- renderPlot({
      
      req(validate_password_basic())
        
        if (input$home.interval == 3) {
            plot <- HOME.VIZ.GROWTH(my.year, sp500.year, 1)
        }
        
        if (input$home.interval == 2) {
            plot <- HOME.VIZ.GROWTH(my.month, sp500.month, 1)
        }
        
        if (input$home.interval == 1) {
            plot <- HOME.VIZ.GROWTH(my.5days, sp500.week, 0.1)
        }
        
        plot
    })
    
    
    output$home.react <- renderReactable({
      
      req(validate_password_basic())
        
        home.react
        
    })
    
    
    
    
    
    # ------------------------------------------------
    
    
    
    
    
    
    
    
    
    
    
    
    # --
    # ---------------
    # SEARCH PAGE
    # ---------------
    # --
    
    
    #
    # GET DATA
    #
    
    dat.search <- reactive({
      
      if(input$search.plot.interval == 1) {
        interval <- week-1
        freq <- 'daily'
      } else if(input$search.plot.interval == 2) {
        interval <- floor_date(month-1, 'month')
        freq <- 'weekly'
      } else if(input$search.plot.interval == 3) {
        interval <- month.6-1
        freq <- 'monthly'
      } else if(input$search.plot.interval == 4) {
        interval <- floor_date(year-day(today()), 'month')
        freq <- 'monthly'
      } else if(input$search.plot.interval == 5) {
        interval <- year.5-1
        freq <- 'yearly'
      } else if(input$search.plot.interval == 6) {
        interval <- year.10-1
        freq <- 'yearly'
      }
      
      dat <- BatchGetSymbols(tickers = input$search.ticker,
                             first.date = interval,
                             last.date = today+1,
                             freq.data = freq,
                             do.cache = F,
                             thresh.bad.data = 0)
      
      
      dat$df.tickers
      
      
    })
    
    
    search.sp500 <- reactive ({
      
      if(input$search.plot.interval == 1) {
        search.sp500 <- sp500.week
      } else if(input$search.plot.interval == 2) {
        search.sp500 <- sp500.month
      } else if(input$search.plot.interval == 3) {
        search.sp500 <- sp500.6months
      } else if(input$search.plot.interval == 4) {
        search.sp500 <- sp500.year
      } else if(input$search.plot.interval == 5) {
        search.sp500 <- sp500.5years
      } else if(input$search.plot.interval == 6) {
        search.sp500 <- sp500.10years
      }
      
      search.sp500
      
    })
    
    
    
    output$search.plot <- renderPlot({
      
      req(validate_password_basic())
      
      if (input$search.plot.type == 'Market value'){
        SEARCH.VIZ.ADJ(dat.search())
      } else {
        SEARCH.VIZ.GROWTH(dat.search(), search.sp500())
      }
      
      
    })
    
    
    
    
    
    #
    # FINANCIAL STATEMENTS
    #
    
    balance.r <- reactive({
      dat <- balance_sheet(input$search.ticker)
      dat <- tibble::rownames_to_column(dat, 'date')
    })
    
    income.r <- reactive({
      dat <- income_sheet(input$search.ticker)
      dat <- tibble::rownames_to_column(dat, 'date')
    })
    
    earnings.r <- reactive({
      dat <- earnings_quarters(input$search.ticker)
      #dat <- tibble::rownames_to_column(dat, 'date')
    })
    
    

    # -- BALANCE SHEET
    output$balance.p <- renderPlot({
      req(input$search.ticker)
      
      SEARCH.BALANCE.VIZ(balance.r())
    })
    
    
    # -- INCOME SHEET
    output$income.p <- renderPlot({
      req(input$search.ticker)
      
      SEARCH.INCOME.VIZ(income.r())
    })
    
    
    
    # -- EARNINGS
    output$earnings.p <- renderPlot({
        req(input$search.ticker)
        
        SEARCH.EARNINGS.VIZ(earnings.r())
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    # ------------------------------------------------
    
    
    
    
    
    
    
    
    
    
    # --
    # ---------------
    # RECOMMENDATIONS
    # ---------------
    # --
    
    
    # -- GET DATA
    
    dat.plot <- reactive({
        
        dat <- getSymbols(input$plot.ticker,
                          auto.assign = F,
                          from = input$daterange[1],
                          to = input$daterange[2])
        
        #macd <- myMACD(Cl(dat), 12, 26,9)
        
        dat
        
    })
    
    
    
    # -- PLOT 1
    
    
    output$plot1 <- renderPlot({
      
      req(validate_password_basic())
        
        validate(
            need(length(my.tickers) >= 1, '')
        )
        
        chartSeries(dat.plot(),
                    subset=paste0(input$daterange[1],'::',input$daterange[2]),
                    theme=chartTheme('white'))
        addMACD(fast=12,slow=26,signal=9,type="EMA")
        
    })
    
    
    
    
    # -- PLOT 2
    
    output$plot2 <- renderPlot({
      
      req(validate_password_basic())
        
        validate(
            need(length(my.tickers) >= 1, '')
        )
        
        chartSeries(dat.plot(),
                    subset=paste0(input$daterange[1],'::',input$daterange[2]),
                    theme=chartTheme('white'))
        addROC(n=7)
        
    })
    
    
    
    
    
    
    
    ## -- WATCHLISTS
    
    
    
    # -- DYNAMIC SELECTINPUTS
    
    output$selectInput <- renderUI({
      
      req(validate_password_basic())
        
        selectInput('w.plot.ticker',
                    'Ticker:',
                    choices = watchlist(RH, action='get', watchlist = input$watchlist),
                    selected = watchlist(RH, action='get', watchlist = input$watchlist)[1]
        )
        
    })
    
    
    
    # -- GETTING DATA
    dat.w.plot <- reactive({
        
        dat <- getSymbols(input$w.plot.ticker,
                          auto.assign = F,
                          from = input$daterange[1],
                          to = input$daterange[2])
        
        #macd <- myMACD(Cl(dat), 12, 26,9)
        
        dat
        
    })
    
    
    
    
    
    
    # -- WATCHLIST PLOT 1
    
    output$w.plot1 <- renderPlot({
      
      req(validate_password_basic())
        
        validate(
            need(length(watchlist(RH, action='get', watchlist = input$watchlist)) >= 1, '')
        )
        
        chartSeries(dat.w.plot(),
                    subset=paste0(input$daterange[1],'::',input$daterange[2]),
                    theme=chartTheme('white'))
        addMACD(fast=12,slow=26,signal=9,type="EMA")
        
    })
    
    
    
    
    
    
    
    
    # -- WATCHLIST PLOT 2
    
    
    output$w.plot2 <- renderPlot({
      
      req(validate_password_basic())
        
        validate(
            need(length(watchlist(RH, action='get', watchlist = input$watchlist)) >= 2, '')
        )
        
        chartSeries(dat.w.plot(),
                    subset=paste0(input$daterange[1],'::',input$daterange[2]),
                    theme=chartTheme('white'))
        addROC(n=7)
        
    })
    
    
    
    # --
    # REC. SEARCH PLOTS
    # --
    
    # -- GET DATA
    
    dat.s.plot <- reactive({
        
        dat <- getSymbols(input$plot.s.ticker,
                          auto.assign = F,
                          from = input$daterange[1],
                          to = input$daterange[2])
        
        #macd <- myMACD(Cl(dat), 12, 26,9)
        
        dat
        
    })
    
    
    
    # -- PLOT 1
    
    
    output$s.plot1 <- renderPlot({
      
      req(validate_password_basic())
        
        validate(
            need(length(all.tickers) >= 1, '')
        )
        
        chartSeries(dat.s.plot(),
                    subset=paste0(input$daterange[1],'::',input$daterange[2]),
                    theme=chartTheme('white'))
        addMACD(fast=12,slow=26,signal=9,type="EMA")
        
    })
    
    
    
    
    # -- PLOT 2
    
    output$s.plot2 <- renderPlot({
      
      req(validate_password_basic())
        
        validate(
            need(length(all.tickers) >= 1, '')
        )
        
        chartSeries(dat.s.plot(),
                    subset=paste0(input$daterange[1],'::',input$daterange[2]),
                    theme=chartTheme('white'))
        addROC(n=7)
        
    })
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
}



