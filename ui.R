#
# 
#

library(shiny)
library(shinyjs)
library(shinythemes) 
library(shinyWidgets)
library(shinycssloaders)



ui <- #secure_app(head_auth = tags$script(inactivity),
                 
    fluidPage(
    
        theme = shinythemes::shinytheme("paper"),
        
        titlePanel("", windowTitle = "yourfunds"),
        
        
        
        mainPanel(
            width = 12,
            
            
            ## LOGIN
            
            
            shinyjs::useShinyjs(),
            
            uiOutput(outputId = "display_content_basic"),
            
            div(
                id = "login_basic", 
                style = "width: 500px; max-width: 100%; margin: 0 auto;",
                
                div(
                    class = "well",
                    h4(class = "text-center", "Please login"),
                    p(class = "text-center", 
                      tags$small("Robinhood")
                    ),
                    
                    textInput(
                        inputId     = "ti_user_name_basic", 
                        label       = tagList(icon("user"), 
                                              "User Name"),
                        placeholder = "Enter user name"
                    ),
                    
                    passwordInput(
                        inputId     = "ti_password_basic", 
                        label       = tagList(icon("unlock-alt"), 
                                              "Password"), 
                        placeholder = "Enter password"
                    ), 
                    
                    div(
                        class = "text-center",
                        actionButton(
                            inputId = "ab_login_button_basic", 
                            label = "Log in",
                            class = "btn-primary"
                        )
                    )
                )
            ),
            
            
            
            

            
            #####
            
            #----------------------------------------
            
            conditionalPanel(
                
                condition = "output.login",
                
                
                    
                
                fluidRow(
        
                    h1(HTML(title)),
        
                    #h5(HTML(subtitle)),
        
                    br()
        
                ),
                

            
                tabsetPanel(type = "tabs",
                            
                            tabPanel("Home",
                                     width = 12,
                                     fluidRow(
                                         column(5,
                                                #HTML(paste0('<font size=25pt>$', market.value, '</font size>')),
                                                h1(HTML(paste0('$', market.value))),
                                                h6(HTML(paste0('$', buying.power, ' of buying power'))),
                                                h6(HTML(paste0('$', deposits, ' invested'))),
                                                h6(HTML(paste0('$', revenue, ' (',percent_growth, '%)', ' of return'))),
                                                # # Home plot
                                                br(),
                                                HTML('<font size=3pt>Market value return</font size></font color>'),
                                                br(),
                                                HTML(paste0('<font size=1pt;font color=',COLOR,'> Yours  ',
                                                               '<font color=',COLOR2,'> S&P500', '</font size></font color>')),
                                                plotOutput('plot.home'),
                                                radioButtons('home.interval',
                                                             '',
                                                             inline = T,
                                                             choices = list(
                                                                 '5 days' = 1,
                                                                 'Month' = 2,
                                                                 'Year' = 3),
                                                                 selected = 3
                                                             )
                                         ),
                                         
                                         column(7,
                                                reactableOutput('home.react')
                                         )
                                     
                                    ) # fluidRow
                            ), # tabPanel Home
                            
                            
                                        
                            tabPanel("Search",
                                 
                                 width = 12,
                                 
                                 fluidRow(
                                     
                                     column(4,
                                            selectInput('search.ticker',
                                                        'Ticker:',
                                                        choices = all.tickers,
                                                        selected = my.tickers[1])
                                         ),
                                     column(4,
                                            selectInput('search.plot.type',
                                                        'Chart type:',
                                                        choices = c('Market value','Market return'),
                                                        selected = 'Market return'
                                            ))
                                 ),
                                 
                                 
                                 fluidRow(
                                     column(8,
                                         radioButtons('search.plot.interval',
                                                      '',
                                                      inline = T,
                                                      choices = list(
                                                          '5 days' = 1,
                                                          '1 month' = 2,
                                                          '6 months' = 3,
                                                          '1 year' = 4,
                                                          '5 years' = 5,
                                                          '10 years' = 6),
                                                      selected = 1
                                         ))
                                     ),
                                 fluidRow(
                                     column(8,
                                     plotOutput('search.plot')
                                    ),
                                    column(4, 
                                           plotOutput("earnings.p"))
                                 ),
                                 
                                 br(),
                                 br(),
                                 
                                 fluidRow(
                                     column(6,
                                            plotOutput("balance.p")),
                                     column(6,
                                            plotOutput("income.p"))
                                 
                                 
                                )
                            ),
                        
                        
        
                            # tabPanel("Compare",
                            # 
                            #      width = 12,
                            # 
                            #      h1(HTML('COMPARE'))
                            # 
                            # ),
                            # 
                            # 
                            # 
                            # tabPanel("52 High",
                            # 
                            #      width = 12,
                            # 
                            #      h1(HTML('52 HIGH'))
                            # 
                            # ),
                            # 
                            # 
                            # 
                            # tabPanel("52 Low",
                            # 
                            #      width = 12,
                            # 
                            #      h1(HTML('52 LOW'))
                            # 
                            #  ),
        
        
        
                            tabPanel("Recommendations",
        
                                 width = 12,
                                 
                                 fluidRow(
                                     
                                     br(),
                                     
                                     dateRangeInput("daterange", "Date range:",
                                                    min = "2007-01-01",
                                                    start = year,
                                                    end   = today+1)
                                     
                                 ),
        
        
                                 tabsetPanel(type = "tabs",
        
        
                                     tabPanel("Yours",
        
                                         fluidRow(
                                             selectInput('plot.ticker',
                                                         'Ticker:',
                                                         choices = my.tickers,
                                                         selected = my.tickers[1])
                                         ),
                                         
                                         HTML('For information on how to read these graphs, click 
                                              <a href=https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/ttr-package.html>
                                              here</a>.'),
        
        
                                         fluidRow(
                                             column(6,
                                                    HTML("MACD: Moving average convergence/divergence"),
                                                    plotOutput(outputId = "plot1", width = "100%") %>%
                                                        withSpinner(color=COLOR)
                                             ),
                                             column(6,
                                                    HTML("ROC: Rate of Change"),
                                                    plotOutput(outputId = "plot2", width = "100%")%>%
                                                        withSpinner(color=COLOR)
                                             )
                                         )
        
                                     ),   # tabPanel Yours
        
        
        
        
                                tabPanel("Watchlists",
        
                                         width = 12,
        
                                         fluidRow(
        
                                             selectInput('watchlist',
                                                         'Select a list:',
                                                         choices = my.watchlists,
                                                         selected = my.watchlists[1])
                                         ),
        
                                         fluidRow(
                                             htmlOutput("selectInput")
                                         ),
        
                                         fluidRow(
                                             column(6,
                                                    plotOutput(outputId = "w.plot1", width = "100%") %>%
                                                        withSpinner(color=COLOR)
                                             ),
                                             column(6,
                                                    plotOutput(outputId = "w.plot2", width = "100%")%>%
                                                        withSpinner(color=COLOR)
                                             )
                                         ),
        
                                ),  # tabPanel Watchlists
        
        
                                tabPanel("Search",
        
                                         width = 12,
        
        
                                         fluidRow(
                                             selectInput('plot.s.ticker',
                                                         'Ticker:',
                                                         choices = all.tickers,
                                                         selected = all.tickers[1],
                                                         selectize = T)
                                         ),
        
        
                                         fluidRow(
                                             column(6,
                                                    plotOutput(outputId = "s.plot1", width = "100%") %>%
                                                        withSpinner(color=COLOR)
                                             ),
                                             column(6,
                                                    plotOutput(outputId = "s.plot2", width = "100%")%>%
                                                        withSpinner(color=COLOR)
                                             )
                                         )
                                )  # tabPanel Search
        
        
        
                         ) # tabsetPanel under recommendations
        
        
                        ) # tabPanel Recommendations
        
                ),  # tabsetPanel
                
                
                br(),
                
                h6(HTML('Data source: <a href="https://finance.yahoo.com/">YahooFinance</a>
                and <a href="https:/robinhood.com/">Robinhood</a>'))
                
            )  # conditionalPanel
    
    
    
            
        )   # mainPanel
            
        
    )  # fluidPage
  # secure auth





