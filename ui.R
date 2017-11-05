library(shiny)

shinyUI(fluidPage(theme = "decred.css",
  fluidRow(column(width=3,
           tags$img(src="decred.png", width="200px")
                 ),
          column(width=6, offset = 3,
                 titlePanel("PoS Master")
          )),
  fluidRow(width=12,
           checkboxInput(inputId = "stakePool",
                         label = "Using stake pool?",
                         value = TRUE)
           ),
  #Network Information
  tabsetPanel(type = "tabs",
    tabPanel("Network Information", fluidRow(
      column(12, h1("Decred Exchange Rate")),
      column(3, wellPanel(h5("DCR/USD"), tags$b(h4(textOutput("USD"))))),
      column(3, wellPanel(h5("DCR/BTC"), tags$b(h4(textOutput("BTC"))))),
      column(3, wellPanel(h5("24hr Low"), tags$b(h4(textOutput("low"))))),
      column(3, wellPanel(h5("24hr High"), tags$b(h4(textOutput("high"))))),
      column(12, wellPanel(h4("OHLC Plot"), plotOutput("OHLC")))
    ),
    fluidRow(
      column(12, h1("PoS Mining")),
      column(3, wellPanel(h5("Ticket Price"), tags$b(h4(textOutput("price"))))),
      column(3, wellPanel(h5("Voting Pool Size"), tags$b(h4(textOutput("pool"))))),
      column(3, wellPanel(h5("Mempool Size"), tags$b(h4(textOutput("mem"))))),
      column(3, wellPanel(h5("Avg Mempool Fee"), tags$b(h4(textOutput("avgfee")))))
    ),
    fluidRow(
      column(4, wellPanel(fluidRow(
        column(12, h5("Upcoming Ticket Price"))),
        fluidRow(
          column(3, tags$b(tags$span(style="color:green", "Current Estimate:"))),
          column(3, tags$b(h4(textOutput("estExp"))))),
        fluidRow(
          column(3, tags$b(tags$span(style="color:blue", "Min:"))),
          column(9, tags$b(h4(textOutput("estMin"))))),
        fluidRow(
          column(3, tags$b(tags$span(style="color:red", "Max:"))),
          column(9, tags$b(h4(textOutput("estMax")))))
      )),
      column(4, wellPanel(fluidRow(
        column(12, h5("Ticket Price Progress"))),
        fluidRow(
          column(12, tags$b(h4(textOutput("changeBlocks")))))
       ,
      fluidRow(
      column(12, tags$b(h4(textOutput("changeTime")))))
      ))
    ),
    fluidRow(
      column(12, h1("Current Mempool Fee Data Graphs")),
      column(12, tabsetPanel(
        tabPanel("Fee Chart", plotOutput("feeBar")),
        tabPanel("Ticket Split", plotOutput("ticketSplit"))))
    ),
    fluidRow(
      column(12, h1("Network Details")),
      column(3, wellPanel(h5("Block Height"), tags$b(h4(textOutput("height"))))),
      column(3, wellPanel(h5("Average Block Time (last 4k blocks)"), tags$b(h4(textOutput("time"))))),
      column(3, wellPanel(h5("Network Hash Rate"), tags$b(h4(textOutput("hash"))))),
      column(3, wellPanel(h5("Network Difficulty"), tags$b(h4(textOutput("diff")))))
    )    
  ),
  # Voting Stats
  tabPanel("Voting Stats",
           fluidRow(
              column(12, plotOutput("voteLine", height = "100px"))),
           fluidRow(
             column(12, "At least 4032 non-abstain votes are required for a vote to succeed. For more information on Decred voting, ", tags$a(href = "https://docs.decred.org/getting-started/user-guides/agenda-voting", "click here"))),
           fluidRow(
             column(12, plotOutput("quromProgress"))),
           fluidRow(
             column(12, tableOutput("agendaItems"))),
           fluidRow(
             column(12, plotOutput("agendaPlots")))),
  #Ticket Returns
  tabPanel("Ticket Returns", fluidRow(width=12,
           sliderInput(inputId = "priceSlider",
                       label = "Select ticket price range to display",
                       min = 20,
                       max = 200,
                       value = c(max(20,round(poolData$stakediff, 0) - 10), min(200, round(poolData$stakediff, 0) + 10)),
                       step = 1,
                       width = "100%"),
           sliderInput(inputId = "feeSlider",
                       label = "Select ticket fee range to display",
                       min = 0.001,
                       max = 2,
                       value = c(max(0.001,round(poolData$feeinfo.mean, 0) - 0.1), min(2, round(poolData$feeinfo.mean, 0) + 0.1)),
                       step = 0.01,
                       width = "100%")
           ),
  fluidRow(width=12,
           DT::dataTableOutput("priceGrid")
          )),
  #Investment Returns
  tabPanel("Your Investment Returns", fluidRow(width=12,
    sidebarLayout(
      # Sidebar with a slider input for number of bins
    sidebarPanel(
      numericInput(inputId = "invest",
                label = "Amount of DCR to invest.", value = "1000"),
      numericInput(inputId = "priceNow",
                label = "Current ticket price", value = round(poolData$stakediff, 2), min = 20, max = 200),
      numericInput(inputId = "feesNow",
                label = "Current average fee", value = round(poolData$feeinfo.mean, 0),
                min = max(0.01,round(poolData$feeinfo.mean, 0) - 0.1),
                max = min(2, round(poolData$feeinfo.mean, 0) + 0.1)
                ),
      numericInput(inputId = "nextPrice",
                label = "Expected price of next window", value = round(stakeData$estimates.expected))
      ),
      mainPanel(
        textOutput("curPriceInfo")
      )
    )
   ),
   
   fluidRow(width=12,
            tags$p("The following tables lists alternative ticket price and fee combinations that
                   would give a similar return.")
            ),
   fluidRow(width=12,
            DT::dataTableOutput("similarReturns")
            )
  )),
  fluidRow(width=12,
           tags$p("")
  ),
  fluidRow(width=12,
              tags$p("Donations to help with server costs are welcome at DsYTFNfkEj3mQB3owMS6KKqncDS8CAjHUma")
  ),
  fluidRow(width=12,
           tags$span("For support or suggestions, contact @Shadowlance in"), tags$a(href="decred.slack.com", "decred.slack.com"), 
           tags$span("or on the "), tags$a(href="https://forum.decred.org/threads/presenting-pos-master.5115/", "forums")
           )
))
