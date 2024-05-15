library(shiny)
library(shinydashboard)
library(quantmod)
library(plotly)
library(zoo)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Dividend Stocks Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Comparison", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Dividends & Hedging", tabName = "dividends_inflation", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Dashboard Header */
      .skin-blue .main-header .navbar {
        background-color: #004080; /* Darker blue color */
      }
      .skin-blue .main-header .logo {
        background-color: #004080; /* Darker blue color */
        color: white;
        border-bottom: 0 solid transparent;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #004080; /* Darker blue color */
      }
      .skin-blue .main-header .navbar .sidebar-toggle {
        color: white;
      }

      /* Dashboard Sidebar */
      .skin-blue .main-sidebar {
        background-color: #cce5ff; /* Lighter blue color */
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #218838; /* Green color */
        color: white;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        color: #000;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #b3d9ff; /* Adjusted lighter blue color */
        color: #000;
      }

      /* Dashboard Body */
      .content-wrapper, .right-side {
        background-color: white;
      }

      /* Boxes */
      .box.box-solid.box-primary>.box-header {
        color: #fff;
        background: #004080; /* Darker blue color */
      }
      .box.box-solid.box-primary {
        border-bottom-color: #004080; /* Darker blue color */
        border-left-color: #004080; /* Darker blue color */
        border-right-color: #004080; /* Darker blue color */
        border-top-color: #004080; /* Darker blue color */
      }

      /* Custom Section Header Text */
      .green-header {
        color: #218838; /* Green color */
      }

      /* Increase font size for body paragraphs */
      .content-wrapper p {
        font-size: 16px; /* Increase font size */
        line-height: 1.6; /* Increase line height for better readability */
      }
      .content-wrapper ul li {
        font-size: 16px; /* Increase font size for list items */
        line-height: 1.6; /* Increase line height for better readability */
      }

     
      "))
    ),
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              h2("Overview of Dividend Stocks", class = "green-header"),
              p("Dividend stocks are shares of companies that pay regular dividends to their shareholders. 
          They are considered a good investment because they provide a steady income stream and potential for capital appreciation."),
              p("Advantages of dividend stocks:"),
              tags$ul(
                tags$li("Regular income: Dividends provide a steady stream of income."),
                tags$li("Potential for capital appreciation: Stock prices can increase over time."),
                tags$li("Lower volatility: Dividend stocks are generally less volatile than growth stocks."),
                tags$li("Hedge against market downturns: Dividends can provide a source of income even when stock prices fall.")
              ),
              h3("Dividend Stocks vs. Non-Dividend Stocks", class = "green-header"),
              p("Non-dividend stocks, also known as growth stocks, reinvest their earnings into the company rather than paying dividends. 
          While these stocks can provide high returns through capital appreciation, they are often more volatile and offer no income during market downturns. 
          Dividend stocks, on the other hand, provide a steady income through dividends, which can help offset losses during bear markets."),
              fluidRow(
                box(
                  title = "Dividend Fundamentals", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("Basics of Dividends",
                             h4("What are Dividends?", class = "green-header"),
                             p("Dividends are payments made by a corporation to its shareholders, usually in the form of cash or additional shares. They are typically paid quarterly and represent a portion of the company's earnings. There are different types of dividends, including cash dividends, which are the most common, and stock dividends, where additional shares are issued instead of cash."),
                             h4("Key Terminologies", class = "green-header"),
                             p("Dividend Yield is the annual dividend payment divided by the stock's current price. It shows how much income you can expect relative to the stock's price."),
                             p("The Dividend Payout Ratio is the percentage of earnings paid out as dividends. A lower payout ratio can indicate that the company retains earnings for growth, while a higher ratio means more earnings are paid out as dividends."),
                             p("The Ex-Dividend Date is the cutoff date for receiving the next dividend payment. If you buy a stock on or after this date, you won't receive the upcoming dividend.")
                    ),
                    tabPanel("Benefits of Dividend Investing",
                             h4("Steady Income Stream", class = "green-header"),
                             p("One of the primary benefits of dividend investing is the steady income stream it provides. Dividends are paid regularly, which can be especially beneficial during retirement or for supplementing your regular income."),
                             h4("Potential for Capital Appreciation", class = "green-header"),
                             p("In addition to receiving dividends, the value of your shares can also increase over time, providing capital appreciation. This means you can benefit from both income and growth."),
                             h4("Lower Volatility", class = "green-header"),
                             p("Dividend-paying stocks are generally less volatile than growth stocks. Companies that pay dividends often have stable earnings and strong financials, making them less prone to drastic price swings."),
                             h4("Hedge Against Market Downturns", class = "green-header"),
                             p("During market downturns, dividend payments can provide a source of income, helping to offset potential losses in stock value. This makes dividend stocks a more resilient investment during tough economic times.")
                    ),
                    tabPanel("Risks and Challenges",
                             h4("Market Risks", class = "green-header"),
                             p("Like all investments, dividend stocks are subject to market risks. The value of your investment can go down as well as up, and there's no guarantee of dividends."),
                             h4("Dividend Cuts or Suspensions", class = "green-header"),
                             p("Companies can reduce or stop paying dividends if they face financial difficulties. It's important to choose companies with a strong track record of maintaining or increasing dividends.")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Investment Report Links", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  div(class = "link-box", tags$a(href = "https://finance.yahoo.com/quote/AAPL/", target = "_blank", "Yahoo Finance: AAPL")),
                  div(class = "link-box", tags$a(href = "https://investor.apple.com/dividend-history/default.aspx", target = "_blank", "Apple Dividend History"))
                )
              )
      ),
      # Analysis tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Select Stock for Analysis", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  selectInput("analysisStock", "Choose a stock:", 
                              choices = c("AAPL", "MSFT", "T", "VZ", "JNJ", "KO", "PFE", "XOM", "CVX", "MCD"))
                )
              ),
              fluidRow(
                box(
                  title = "Select Date Range", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  dateRangeInput("dates", "Date range:",
                                 start = Sys.Date() - 1825, 
                                 end = Sys.Date())
                )
              ),
              fluidRow(
                box(
                  title = span("Dividend Yield Analysis", class = "plot-title"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  plotOutput("yieldPlot", height = "600px")
                )
              )
      ),
      # Comparison tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Select Stocks for Comparison", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  selectInput("dividendStock", "Choose a dividend stock:", 
                              choices = c("AAPL", "MSFT", "T", "VZ", "JNJ", "KO", "PFE", "XOM", "CVX", "MCD")),
                  selectInput("growthStock", "Choose a non-dividend stock:", 
                              choices = c("AMZN", "GOOGL", "FB", "TSLA", "NFLX", "NVDA", "CRM", "ADBE", "PYPL", "SQ"))
                )
              ),
              fluidRow(
                box(
                  title = span("Stock Price Comparison & Dividend Stability", class = "plot-title"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  plotOutput("comparisonPlot", height = "600px")
                )
              )
      ),
      # Dividends & Inflation tab
      tabItem(tabName = "dividends_inflation",
              h2("Dividends as a Hedge Against Inflation", class = "green-header"),
              p("Dividends can serve as a hedge against inflation because they provide a steady income stream that can increase over time, 
          helping to maintain purchasing power in the face of rising prices. Companies that pay dividends often have strong financials 
          and the ability to pass on costs to consumers, making them more resilient during inflationary periods."),
              fluidRow(
                box(
                  title = span("Inflation vs. Dividend Growth", class = "plot-title"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                  plotlyOutput("inflationPlot")
                )
              ),
              h2("Bear Market Hedge Example Scenario", class = "green-header"),
              p("Imagine you own two stocks, one that pays dividends and one that doesn’t, both worth $10,000 each. A market downturn causes both stocks to lose 20% of their value. Once you have a grasp, feel free to mess around with your own numbers!"),
              fluidRow(
                column(width = 6,
                       h4("Non-Dividend Stock", class = "green-header"),
                       numericInput("initial_value_non_dividend", "Initial Value ($):", value = 10000),
                       verbatimTextOutput("loss_value_non_dividend"),
                       numericInput("income_non_dividend", "Income ($):", value = 0),
                       h4("Total Value After One Year", class = "green-header"),
                       verbatimTextOutput("total_value_non_dividend")
                ),
                column(width = 6,
                       h4("Dividend Stock", class = "green-header"),
                       numericInput("initial_value_dividend", "Initial Value ($):", value = 10000),
                       verbatimTextOutput("loss_value_dividend"),
                       numericInput("dividend_rate", "Annual Dividend Rate (%):", value = 3),
                       h4("Total Value After One Year", class = "green-header"),
                       verbatimTextOutput("total_value_dividend")
                )
              ),
              p("In this default example, the dividend stock provides a $240 income, partially offsetting the $2,000 loss in stock value, while the non-dividend stock offers no income to cushion the loss."),
              p("This shows how dividend stocks can provide financial resilience during bear markets by offering a steady income stream that helps to mitigate overall portfolio losses.")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  analysisStockData <- reactive({
    getSymbols(input$analysisStock, src = "yahoo", auto.assign = FALSE, from = input$dates[1], to = input$dates[2])
  })
  
  output$yieldPlot <- renderPlot({
    stock <- analysisStockData()
    
    # Calculate the dividend yield
    closing_prices <- Cl(stock)
    sma_prices <- SMA(closing_prices, n = 12)
    dividendYield <- closing_prices / as.numeric(sma_prices)
    
    # Get the dividend data
    dividends <- getDividends(input$analysisStock, src = "yahoo", from = input$dates[1], to = input$dates[2])
    
    # Merge the dividend data with the stock price data to align the frequencies
    merged_data <- merge(dividendYield, dividends, all = TRUE)
    merged_data <- na.approx(merged_data, na.rm = FALSE)
    
    # Plot the data
    par(mar = c(5, 4, 4, 5) + 0.3, bg = "lightyellow")
    plot(index(merged_data), merged_data[, 1], type = "l", col = "blue", 
         main = "Dividend Yield Analysis", xlab = "Date", ylab = "Dividend Yield (%)")
    par(new = TRUE)
    plot(index(merged_data), merged_data[, 2], type = "l", col = "darkgreen", 
         axes = FALSE, xlab = "", ylab = "")
    axis(side = 4)
    mtext("Dividends ($)", side = 4, line = 3)
    legend("topright", legend = c("Dividend Yield", "Dividends ($)"), 
           col = c("blue", "darkgreen"), lty = c(1, 1))
  })
  
  output$comparisonPlot <- renderPlot({
    divStock <- getSymbols(input$dividendStock, src = "yahoo", auto.assign = FALSE)
    growthStock <- getSymbols(input$growthStock, src = "yahoo", auto.assign = FALSE)
    
    divStockAdj <- Ad(divStock)
    growthStockAdj <- Ad(growthStock)
    
    dividends <- getDividends(input$dividendStock, src = "yahoo", from = "2000-01-01", to = Sys.Date())
    
    par(mar = c(5, 4, 4, 5) + 0.3, bg = "lightyellow")  # Set margin to allow space for a secondary y-axis
    plot(index(divStockAdj), divStockAdj, type = "l", col = "blue", 
         xlab = "Date", ylab = "Adjusted Price ($)", main = "Stock Price Comparison & Dividend Stability")
    lines(index(growthStockAdj), growthStockAdj, col = "red")
    par(new = TRUE)
    plot(index(dividends), dividends, type = "l", col = "darkgreen", 
         axes = FALSE, xlab = "", ylab = "")
    axis(side = 4)
    mtext("Dividends ($)", side = 4, line = 3)
    legend("topright", legend = c(input$dividendStock, input$growthStock, paste(input$dividendStock, "Dividends")), 
           col = c("blue", "red", "darkgreen"), lty = c(1, 1, 1))
  })
  
  output$inflationPlot <- renderPlotly({
    # Dummy data for inflation and dividend growth
    years <- seq(2010, 2020, by = 1)
    inflationRate <- c(1.5, 2.1, 2.3, 2.4, 2.1, 1.8, 2.5, 2.7, 2.9, 2.2, 2.3)
    dividendGrowth <- c(2.0, 2.3, 2.5, 2.7, 2.4, 2.0, 2.8, 3.0, 3.2, 2.5, 2.6)
    
    inflationData <- data.frame(years, inflationRate, dividendGrowth)
    
    plot_ly(inflationData, x = ~years) %>%
      add_lines(y = ~inflationRate, name = "Inflation Rate", line = list(color = 'red')) %>%
      add_lines(y = ~dividendGrowth, name = "Dummy Stock Dividend Growth", line = list(color = 'blue')) %>%
      layout(title = "Inflation vs. Dividend Growth",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Rate (%)", showgrid = TRUE, gridcolor = 'lightyellow'),
             yaxis2 = list(title = "Growth (%)", overlaying = "y", side = "right", showgrid = TRUE, gridcolor = 'lightyellow'),
             plot_bgcolor='lightyellow',
             legend = list(x = 0.1, y = 0.9))
  })
  
  # Calculate loss value for non-dividend stock
  output$loss_value_non_dividend <- renderText({
    loss_value <- input$initial_value_non_dividend * 0.8
    paste("Value after 20% Loss: $", loss_value)
  })
  
  # Calculate loss value for dividend stock
  output$loss_value_dividend <- renderText({
    loss_value <- input$initial_value_dividend * 0.8
    paste("Value after 20% Loss: $", loss_value)
  })
  
  output$total_value_non_dividend <- renderText({
    initial_value <- input$initial_value_non_dividend
    loss_value <- initial_value * 0.8
    income <- input$income_non_dividend
    total_value <- loss_value + income
    paste("Total Value: $", total_value)
  })
  
  output$total_value_dividend <- renderText({
    initial_value <- input$initial_value_dividend
    loss_value <- initial_value * 0.8
    dividend_rate <- input$dividend_rate / 100
    dividend_income <- loss_value * dividend_rate
    total_value <- loss_value + dividend_income
    paste("Total Value: $", total_value, " ($", loss_value, " + $", round(dividend_income, 2), " dividend)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
