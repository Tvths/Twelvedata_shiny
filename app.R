# Shiny app for the twelvedataR package
#
# Install the package first:
#   remotes::install_github("Tvths/twelvedataR")
# and put your key in .Renviron:
#   TWELVEDATA_API_KEY=your_key_here
#
# Run from GitHub:
#   shiny::runGitHub("Twelvedata_shiny", "Tvths")

library(shiny)
library(twelvedataR)

intervals <- c("1min", "5min", "15min", "30min", "45min",
               "1h", "2h", "4h", "1day", "1week", "1month")

# ---------------------------------------------------------------------------
# Animated candlestick widget shown under the sidebar.
# Runs entirely in the browser (no extra API calls). Before any data is
# loaded it shows a random-walk simulation; after "Get data" it replays the
# real closing prices of the downloaded symbol in a loop.
# ---------------------------------------------------------------------------

market_css <- r"---(
.mkt-card {
  margin-top: 15px;
  border-radius: 6px;
  background: linear-gradient(160deg, #0f1b2d 0%, #15304d 100%);
  padding: 10px 12px 8px 12px;
  color: #d6e7ff;
  box-shadow: 0 2px 10px rgba(0, 0, 0, .18);
}
.mkt-head {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  margin-bottom: 6px;
}
.mkt-dot {
  width: 8px; height: 8px; border-radius: 50%;
  background: #26d07c;
  animation: mkt-pulse 1.6s infinite;
}
@keyframes mkt-pulse {
  0%   { box-shadow: 0 0 0 0   rgba(38, 208, 124, .6); }
  70%  { box-shadow: 0 0 0 8px rgba(38, 208, 124, 0);  }
  100% { box-shadow: 0 0 0 0   rgba(38, 208, 124, 0);  }
}
.mkt-price {
  margin-left: auto;
  font-weight: 600;
  font-variant-numeric: tabular-nums;
}
#mkt-canvas { display: block; width: 100%; }
.mkt-foot { font-size: 11px; color: #86a3c6; margin-top: 4px; }
@media (prefers-reduced-motion: reduce) { .mkt-dot { animation: none; } }
.mkt-card { border: 1px solid rgba(140, 200, 255, .15); }
)---"

# Dark navy theme for the whole page, matching the animation card
theme_css <- r"---(
body {
  background: radial-gradient(1200px 700px at 15% -10%, #1b3556 0%, #0b1524 65%) fixed;
  background-color: #0b1524;
  color: #d6e7ff;
}
h1, h2, h3 { color: #eef5ff; }

/* Sidebar box */
.well {
  background: rgba(255, 255, 255, .04);
  border: 1px solid rgba(140, 200, 255, .15);
  box-shadow: none;
}
label, .control-label, .radio label, .checkbox label { color: #cfe0f7; }
.help-block { color: #86a3c6; }
hr { border-top-color: rgba(140, 200, 255, .15); }
input[type=checkbox], input[type=radio] { accent-color: #3a8ee6; }

/* Text, select and date inputs */
.form-control, .selectize-input, .selectize-input.full,
.selectize-dropdown, .selectize-input input {
  background: #0f1b2d !important;
  color: #e6f0ff !important;
  border-color: #2a4466 !important;
  box-shadow: none !important;
}
.form-control:focus, .selectize-input.focus { border-color: #3a8ee6 !important; }
.selectize-dropdown .option { color: #d6e7ff; }
.selectize-dropdown .active { background: #1d3a5f !important; color: #fff !important; }
.input-group-addon { background: #15304d; color: #cfe0f7; border-color: #2a4466; }

/* Slider */
.irs--shiny .irs-line { background: #1d3a5f; border-color: #2a4466; }
.irs--shiny .irs-bar  { background: #3a8ee6; border-color: #3a8ee6; }
.irs--shiny .irs-single { background: #3a8ee6; }
.irs--shiny .irs-min, .irs--shiny .irs-max { background: #1d3a5f; color: #cfe0f7; }
.irs--shiny .irs-grid-text { color: #86a3c6; }
.irs--shiny .irs-grid-pol { background: #4a6a90; }
.irs--shiny .irs-handle { background: #d6e7ff; border-color: #3a8ee6; }

/* Buttons */
.btn-primary { background: #2f7fd8; border-color: #2f7fd8; }
.btn-primary:hover, .btn-primary:focus { background: #3a8ee6; border-color: #3a8ee6; }
.btn-default { background: #15304d; color: #d6e7ff; border-color: #2a4466; }
.btn-default:hover, .btn-default:focus { background: #1d3a5f; color: #fff; border-color: #3a8ee6; }

/* Tabs */
.nav-tabs { border-bottom-color: #2a4466; }
.nav-tabs > li > a { color: #8cc8ff; }
.nav-tabs > li > a:hover { background: #15304d; border-color: transparent; }
.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
  background: #15304d; color: #fff;
  border-color: #2a4466 #2a4466 transparent;
}

/* Tables */
.table { color: #d6e7ff; }
.table > thead > tr > th { border-bottom-color: #2a4466; color: #eef5ff; }
.table > tbody > tr > td { border-top-color: rgba(140, 200, 255, .1); }
.table-striped > tbody > tr:nth-of-type(odd) { background: rgba(255, 255, 255, .03); }
)---"

# Base-graphics settings so the plots blend into the dark page
dark_par <- function() {
  par(bg = NA, fg = "#5d7a9e", col.axis = "#9db6d6",
      col.lab = "#cfe0f7", col.main = "#eef5ff")
}

market_js <- r"---(
$(function () {
  var canvas = document.getElementById('mkt-canvas');
  if (!canvas) return;
  var ctx = canvas.getContext('2d');

  var W = 300, H = 190;   // canvas size in CSS pixels
  var N = 36;             // candles visible at once
  var STEP = 600;         // ms per new candle
  var RIGHT_PAD = 58;     // room for the price label

  var candles = [], real = null, ri = 0, last = 100;
  var lastPush = 0, lo = null, hi = null;
  var reduce = window.matchMedia &&
               window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  var labelEl = document.getElementById('mkt-label');
  var priceEl = document.getElementById('mkt-price');
  var footEl  = document.getElementById('mkt-foot');

  function resize() {
    var d = window.devicePixelRatio || 1;
    W = canvas.clientWidth || 300;
    canvas.width  = Math.round(W * d);
    canvas.height = Math.round(H * d);
    canvas.style.height = H + 'px';
    ctx.setTransform(d, 0, 0, d, 0, 0);
  }

  function makeCandle() {
    var o, c;
    if (real) {
      o = real[ri];
      ri = (ri + 1) % real.length;
      c = real[ri];
      if (ri === 0) o = c;          // loop back to the start without a jump
    } else {
      o = last;
      c = o * (1 + (Math.random() - 0.48) * 0.025);
    }
    last = c;
    var s = Math.abs(c - o) + Math.abs(o) * 0.004;
    return { o: o, c: c,
             h: Math.max(o, c) + Math.random() * s * 0.6,
             l: Math.min(o, c) - Math.random() * s * 0.6 };
  }

  function seed(k) { for (var i = 0; i < k; i++) candles.push(makeCandle()); }

  function draw(now) {
    if (!lastPush) lastPush = now;
    var p = (now - lastPush) / STEP;
    if (p >= 1) {
      candles.push(makeCandle());
      if (candles.length > N + 1) candles.shift();
      lastPush = now; p = 0;
    }
    if (reduce) p = 1;
    var e = p < 0.5 ? 2 * p * p : 1 - Math.pow(-2 * p + 2, 2) / 2;  // ease in-out

    // Smoothly adapting vertical scale
    var mn = Infinity, mx = -Infinity;
    candles.forEach(function (k) { mn = Math.min(mn, k.l); mx = Math.max(mx, k.h); });
    if (lo === null) { lo = mn; hi = mx; }
    lo += (mn - lo) * 0.08;  hi += (mx - hi) * 0.08;
    var pad = (hi - lo) * 0.12 || 1, top = hi + pad, bot = lo - pad;
    function y(v) { return 10 + (top - v) / (top - bot) * (H - 20); }

    var R = W - RIGHT_PAD, sp = R / N, n = candles.length;
    function x(i) { return R - (n - 1 - i + e) * sp; }

    ctx.clearRect(0, 0, W, H);

    // Grid
    ctx.strokeStyle = 'rgba(255,255,255,0.06)';
    ctx.lineWidth = 1;
    for (var g = 1; g < 4; g++) {
      var gy = Math.round(H * g / 4) + 0.5;
      ctx.beginPath(); ctx.moveTo(0, gy); ctx.lineTo(W, gy); ctx.stroke();
    }

    // Close prices of all candles (newest one grows towards its close)
    var closes = candles.map(function (k, i) {
      return i === n - 1 ? k.o + (k.c - k.o) * e : k.c;
    });

    // Candles
    for (var i = 0; i < n; i++) {
      var k = candles[i], cx = x(i);
      if (cx < -sp) continue;
      var c = closes[i], up = c >= k.o;
      var col = up ? '#26d07c' : '#ff5c6c';
      var hh = i === n - 1 ? Math.max(k.o, c) + (k.h - Math.max(k.o, k.c)) * e : k.h;
      var ll = i === n - 1 ? Math.min(k.o, c) - (Math.min(k.o, k.c) - k.l) * e : k.l;
      ctx.globalAlpha = 0.55;
      ctx.strokeStyle = col;
      ctx.beginPath(); ctx.moveTo(cx, y(hh)); ctx.lineTo(cx, y(ll)); ctx.stroke();
      ctx.fillStyle = col;
      var by = y(Math.max(k.o, c)), bh = Math.max(1, y(Math.min(k.o, c)) - by);
      ctx.fillRect(cx - sp * 0.3, by, sp * 0.6, bh);
    }
    ctx.globalAlpha = 1;

    // Area under the close line
    var grad = ctx.createLinearGradient(0, 0, 0, H);
    grad.addColorStop(0, 'rgba(110,180,255,0.25)');
    grad.addColorStop(1, 'rgba(110,180,255,0)');
    ctx.beginPath();
    ctx.moveTo(x(0), H);
    for (i = 0; i < n; i++) ctx.lineTo(x(i), y(closes[i]));
    ctx.lineTo(x(n - 1), H);
    ctx.closePath();
    ctx.fillStyle = grad; ctx.fill();

    // Glowing close line
    ctx.save();
    ctx.shadowColor = 'rgba(120,190,255,0.9)';
    ctx.shadowBlur = 8;
    ctx.strokeStyle = '#8cc8ff';
    ctx.lineWidth = 1.8;
    ctx.beginPath();
    for (i = 0; i < n; i++) {
      if (i === 0) ctx.moveTo(x(i), y(closes[i])); else ctx.lineTo(x(i), y(closes[i]));
    }
    ctx.stroke();
    ctx.restore();

    // Current price: dashed line, pulsing dot and label
    var cur = closes[n - 1], cy = y(cur), lx = x(n - 1);
    ctx.setLineDash([3, 4]);
    ctx.strokeStyle = 'rgba(255,255,255,0.35)';
    ctx.beginPath(); ctx.moveTo(0, cy); ctx.lineTo(R + 4, cy); ctx.stroke();
    ctx.setLineDash([]);

    var pr = 3 + 1.5 * Math.sin(now / 200);
    ctx.fillStyle = 'rgba(140,200,255,0.3)';
    ctx.beginPath(); ctx.arc(lx, cy, pr + 4, 0, 2 * Math.PI); ctx.fill();
    ctx.fillStyle = '#ffffff';
    ctx.beginPath(); ctx.arc(lx, cy, 2.5, 0, 2 * Math.PI); ctx.fill();

    var up = cur >= closes[0];
    ctx.fillStyle = up ? '#26d07c' : '#ff5c6c';
    var ly = Math.min(Math.max(cy - 9, 0), H - 18);
    ctx.fillRect(R + 6, ly, RIGHT_PAD - 8, 18);
    ctx.fillStyle = '#0f1b2d';
    ctx.font = '600 11px sans-serif';
    ctx.textBaseline = 'middle';
    ctx.fillText(cur.toFixed(2), R + 10, ly + 9);

    // Header: price and change over the visible window
    var chg = 100 * (cur / closes[0] - 1);
    priceEl.textContent = cur.toFixed(2) + '  ' + (chg >= 0 ? '▲ +' : '▼ ') + chg.toFixed(2) + '%';
    priceEl.style.color = chg >= 0 ? '#26d07c' : '#ff5c6c';

    requestAnimationFrame(draw);
  }

  // Real data from the server
  Shiny.addCustomMessageHandler('market_anim', function (m) {
    var cl = [].concat(m.close).map(Number).filter(isFinite);
    if (cl.length < 2) return;
    real = cl; ri = 0; last = cl[0];
    candles = []; lo = null;
    seed(Math.min(6, cl.length - 1));
    labelEl.textContent = m.symbol + ' · ' + m.interval;
    footEl.textContent = 'Replaying ' + cl.length + ' real closing prices in a loop';
  });

  resize();
  window.addEventListener('resize', resize);
  seed(N);
  requestAnimationFrame(draw);
});
)---"

market_widget <- div(
  class = "mkt-card",
  div(class = "mkt-head",
      span(class = "mkt-dot"),
      span(id = "mkt-label", "Market simulation"),
      span(id = "mkt-price", class = "mkt-price", "")),
  tags$canvas(id = "mkt-canvas"),
  div(id = "mkt-foot", class = "mkt-foot",
      "Random walk for show. Press 'Get data' to replay real prices.")
)

# ---------------------------------------------------------------------------

sidebar <- sidebarPanel(
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
)

# Put the animation directly under the grey sidebar box, in the same column
sidebar <- tagAppendChild(sidebar, market_widget)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(market_css)),
    tags$style(HTML(theme_css)),
    tags$script(HTML(market_js))
  ),

  titlePanel("Twelve Data explorer"),

  sidebarLayout(
    sidebar,

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
    list(symbol = symbol, interval = input$interval, data = df)
  }, ignoreNULL = TRUE)

  # Send the downloaded closes to the animation (last 300 points at most)
  observeEvent(input$go, {
    p <- tryCatch(prices(), error = function(e) NULL)
    req(p)
    session$sendCustomMessage("market_anim", list(
      symbol   = p$symbol,
      interval = p$interval,
      close    = as.numeric(utils::tail(p$data$close, 300))
    ))
  })

  quote <- eventReactive(input$go, {
    tryCatch(
      get_quote(toupper(trimws(input$symbol))),
      error = function(e) NULL
    )
  })

  output$price_plot <- renderPlot({
    req(prices())
    df <- prices()$data
    dark_par()
    plot(df$datetime, df$close, type = "n", bty = "n",
         main = paste(prices()$symbol, "- closing price"),
         xlab = "", ylab = "Close")
    grid(col = "#FFFFFF14", lty = 1)
    lines(df$datetime, df$close, lwd = 1.8, col = "#8cc8ff")
    if (isTRUE(input$show_ma) && nrow(df) >= 20) {
      ma <- stats::filter(df$close, rep(1 / 20, 20), sides = 1)
      lines(df$datetime, ma, col = "#ffa94d", lwd = 2)
      legend("topleft", legend = c("Close", "MA(20)"),
             col = c("#8cc8ff", "#ffa94d"), lwd = 2, bty = "n",
             text.col = "#cfe0f7")
    }
  }, bg = "transparent")

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
    dark_par()
    hist(r, breaks = 40, col = "#3a6ea5", border = "#0b1524",
         main = paste(prices()$symbol, "- returns per", input$interval),
         xlab = "Return")
    abline(v = mean(r), col = "#ff5c6c", lwd = 2)
  }, bg = "transparent")

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
