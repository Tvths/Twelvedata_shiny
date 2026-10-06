# Shiny app for the twelvedataR package
#
# Install the package first:
#   remotes::install_github("Tvths/twelvedataR")
# and put your key in .Renviron:
#   TWELVEDATA_API_KEY=your_key_here
#
# Run from GitHub:
#   shiny::runGitHub("twelvedataShiny", "Tvths")

library(shiny)
library(twelvedataR)

intervals <- c("1min", "5min", "15min", "30min", "45min",
               "1h", "2h", "4h", "1day", "1week", "1month")

ui <- fluidPage(
  titlePanel("Twelve Data explorer"),

  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Symbol", value = "AAPL",
                placeholder = "e.g. AAPL, MSFT, EUR/USD, BTC/USD"),

      selectInput("interval", "Interval", choices = intervals,
                  selected = "1day"),

      radioButtons("period", "Period",
                   choices = c("Last N observations" = "n",
                               "Date range" = "range"),
                   selected = "n"),

      conditionalPanel(
        condition = "input.period == 'n'",
        sliderInput("outputsize", "Number of observations",
                    min = 10, max = 5000, value = 100, step = 10)
      ),

      conditionalPanel(
        condition = "input.period == 'range'",
        dateRangeInput("dates", "Date range",
                       start = Sys.Date() - 365, end = Sys.Date(),
                       max = Sys.Date())
      ),

      checkboxInput("show_ma", "Show 20-period moving average", value = TRUE),

      # A button instead of reacting to every keystroke:
      # the free API plan only allows 8 requests per minute.
      actionButton("go", "Get data", class = "btn-primary"),

      hr(),
      helpText("Data from the Twelve Data API via the twelvedataR package.",
               "Requires TWELVEDATA_API_KEY in your .Renviron.")
    ),

    mainPanel(
      uiOutput("key_warning"),
      tabsetPanel(
        tabPanel("Price chart",
                 plotOutput("price_plot", height = "400px"),
                 tableOutput("summary_table")),
        tabPanel("Daily returns",
                 plotOutput("return_hist", height = "400px")),
        tabPanel("Latest quote",
                 tableOutput("quote_table")),
        tabPanel("Data",
                 downloadButton("download", "Download CSV"),
                 br(), br(),
                 tableOutput("data_table"))
      )
    )
  )
)

server <- function(input, output, session) {

  output$key_warning <- renderUI({
    if (!nzchar(Sys.getenv("TWELVEDATA_API_KEY"))) {
      div(class = "alert alert-danger",
          "No TWELVEDATA_API_KEY found. Add it to your .Renviron and restart R.")
    }
  })

  # Download only when the button is pressed
  prices <- eventReactive(input$go, {
    symbol <- toupper(trimws(input$symbol))

    args <- list(symbol = symbol, interval = input$interval)
    if (input$period == "n") {
      args$outputsize <- input$outputsize
    } else {
      args$start_date <- input$dates[1]
      args$end_date   <- input$dates[2]
      args$outputsize <- 5000
    }

    df <- tryCatch(
      do.call(get_time_series, args),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    validate(need(!is.null(df), "Could not download data. See the error message."))
    list(symbol = symbol, data = df)
  }, ignoreNULL = TRUE)

  quote <- eventReactive(input$go, {
    tryCatch(
      get_quote(toupper(trimws(input$symbol))),
      error = function(e) NULL
    )
  })

  output$price_plot <- renderPlot({
    req(prices())
    df <- prices()$data
    plot(df$datetime, df$close, type = "l", lwd = 1.5, col = "steelblue",
         main = paste(prices()$symbol, "- closing price"),
         xlab = "", ylab = "Close")
    if (isTRUE(input$show_ma) && nrow(df) >= 20) {
      ma <- stats::filter(df$close, rep(1 / 20, 20), sides = 1)
      lines(df$datetime, ma, col = "darkorange", lwd = 2)
      legend("topleft", legend = c("Close", "MA(20)"),
             col = c("steelblue", "darkorange"), lwd = 2, bty = "n")
    }
  })

  output$summary_table <- renderTable({
    req(prices())
    df <- prices()$data
    data.frame(
      From         = format(min(df$datetime)),
      To           = format(max(df$datetime)),
      Observations = nrow(df),
      Min          = min(df$close),
      Max          = max(df$close),
      Last         = df$close[nrow(df)],
      Change_pct   = round(100 * (df$close[nrow(df)] / df$close[1] - 1), 2)
    )
  })

  output$return_hist <- renderPlot({
    req(prices())
    df <- prices()$data
    validate(need(nrow(df) > 2, "Need more observations to compute returns."))
    r <- diff(df$close) / head(df$close, -1)
    hist(r, breaks = 40, col = "grey80", border = "white",
         main = paste(prices()$symbol, "- returns per", input$interval),
         xlab = "Return")
    abline(v = mean(r), col = "red", lwd = 2)
  })

  output$quote_table <- renderTable({
    q <- quote()
    validate(need(!is.null(q), "Press 'Get data' to load the latest quote."))
    data.frame(Field = names(q), Value = vapply(q, as.character, ""))
  })

  output$data_table <- renderTable({
    req(prices())
    df <- prices()$data
    df$datetime <- format(df$datetime)
    df <- df[rev(seq_len(nrow(df))), ]   # newest first
    head(df, 200)
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(gsub("/", "-", prices()$symbol), "_", input$interval, ".csv")
    },
    content = function(file) {
      utils::write.csv(prices()$data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
