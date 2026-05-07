# ============================================================
#   INVESTMENT PORTFOLIO LAB — R FINANCE COURSE
#   Warren Buffett's Top 10 Berkshire Hathaway Holdings
#   Tickers: AAPL AXP BAC KO CVX OXY MCO KHC CB DVA
# ============================================================
#
#  INSTALL PACKAGES ONCE before running (copy & paste):
#
#  install.packages(c("shiny", "plotly",
#    "tidyquant","PerformanceAnalytics",
#    "quadprog", "ggrepel", "glmnet", "ranger",
#    "xgboost", "TTR", "moments", "xts"
#  ))
# install.packages("remotes")
# remotes::install_version("GenSA", version = "1.1.7")
# ============================================================

library(shiny)
library(plotly)
library(dplyr)
library(tidyquant)
library(tidyverse)
library(PerformanceAnalytics)
library(quadprog)
library(ggrepel)
library(glmnet)
library(ranger)
library(xgboost)
library(TTR)
library(moments)
library(xts)

setwd("~/Documents/Cursos-R/Laboratorio-6")

# ════════════════════════════════════════════════════════════
# MODULE 1 — DATA COLLECTION FROM YAHOO FINANCE 
# DESCRIPTIVE STATISTICS
# ════════════════════════════════════════════════════════════
# Obtenemos los tickers de nuestro portafolio
tickers <- c(
  "MCD", "NVDA", "TSLA", "AMZN", "GOOG", "XOM", "PEP", "ATER",
  "APP", "AXP", "BAC", "KO", "CVX", "OXY", "MCO", "KHC", "CB", "DVA"
)

start_date <- "2019-01-01"
end_date   <- Sys.Date()

cat("\n▶ MODULE 1 — Downloading price data from Yahoo Finance...\n")
prices_raw <- tq_get(
  tickers,
  from = start_date,
  to   = end_date,
  get  = "stock.prices"
)

str(prices_raw)
prices_raw$date <- as.Date(prices_raw$date)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Candlestick Portfolio Viewer"),
  
  fluidRow(
    column(width = 4,
           selectInput("symbol", "Stock:",
                       choices  = unique(prices_raw$symbol),
                       selected = unique(prices_raw$symbol)[1]
           )
    ),
    column(width = 4,
           dateInput("start_date", "Start Date:",
                     value = min(prices_raw$date),
                     min   = min(prices_raw$date),
                     max   = max(prices_raw$date)
           )
    ),
    column(width = 4,
           dateInput("end_date", "End Date:",
                     value = max(prices_raw$date),
                     min   = min(prices_raw$date),
                     max   = max(prices_raw$date)
           )
    )
  ),
  
  fluidRow(
    column(width = 12,
           plotlyOutput("candlestick_plot", height = "750px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$symbol, input$start_date, input$end_date)
    req(input$start_date <= input$end_date)
    
    prices_raw %>%
      filter(
        symbol == input$symbol,
        date   >= as.Date(input$start_date),
        date   <= as.Date(input$end_date)
      ) %>%
      arrange(date) %>%
      # ── Compute SMAs on the FULL filtered window ──────────────
      mutate(
        sma20 = zoo::rollmean(close, k = 20, fill = NA, align = "right"),
        sma50 = zoo::rollmean(close, k = 50, fill = NA, align = "right"),
        vol_color = ifelse(close >= open,
                           "rgba(0,200,100,0.6)",
                           "rgba(220,50,50,0.6)")
      )
  })
  
  output$candlestick_plot <- renderPlotly({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # ── 1. Candlestick trace ──────────────────────────────────────
    p_candle <- plot_ly(
      data = df,
      x    = ~date,
      type = "candlestick",
      open  = ~open,
      high  = ~high,
      low   = ~low,
      close = ~close,
      name  = input$symbol,
      increasing = list(line = list(color = "#00c864")),
      decreasing = list(line = list(color = "#dc3232"))
    ) %>%
      
      # ── SMA 20 (magenta) ────────────────────────────────────────
      add_lines(
        data       = df,
        x          = ~date,
        y          = ~sma20,
        name       = "SMA 20",
        inherit    = FALSE,
        line       = list(color = "magenta", width = 1.5, dash = "solid"),
        connectgaps = FALSE
      ) %>%
      
      # ── SMA 50 (dodger blue) ────────────────────────────────────
      add_lines(
        data       = df,
        x          = ~date,
        y          = ~sma50,
        name       = "SMA 50",
        inherit    = FALSE,
        line       = list(color = "dodgerblue", width = 1.5, dash = "solid"),
        connectgaps = FALSE
      ) %>%
      
      layout(
        yaxis = list(title = "Price (USD)"),
        xaxis = list(rangeslider = list(visible = FALSE))
      )
    
    # ── 2. Volume bar trace ───────────────────────────────────────
    p_volume <- plot_ly(
      data   = df,
      x      = ~date,
      y      = ~volume,
      type   = "bar",
      marker = list(color = ~vol_color),
      name   = "Volume",
      showlegend = FALSE
    ) %>%
      layout(
        yaxis = list(
          title      = "Volume",
          tickformat = ".2s"
        ),
        xaxis = list(
          rangeslider = list(visible = TRUE)
        )
      )
    
    # ── 3. Combine with subplot (75 % price / 25 % volume) ────────
    subplot(
      p_candle,
      p_volume,
      nrows   = 2,
      shareX  = TRUE,
      heights = c(0.75, 0.25),
      titleY  = TRUE
    ) %>%
      layout(
        title = list(
          text = paste("Candlestick Chart:", input$symbol),
          font = list(size = 18)
        ),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = 1.05),
        margin    = list(t = 60)
      )
  })
}

# ---- RUN APP ----
shinyApp(ui, server)

### COMPARISON PLOTS ####

# ── Tickers ───────────────────────────────────────────────────────────────────
portfolio_tickers <- sort(c(
  "MCD", "NVDA", "TSLA", "AMZN", "GOOG", "XOM", "PEP", "ATER",
  "APP", "AXP", "BAC", "KO",  "CVX", "OXY", "MCO", "KHC", "CB", "DVA"
))

index_tickers <- c("^GSPC", "^DJI", "^IXIC", "^RUT", "^VIX")
index_labels  <- c(
  "^GSPC" = "S&P 500",
  "^DJI"  = "Dow Jones",
  "^IXIC" = "NASDAQ",
  "^RUT"  = "Russell 2000",
  "^VIX"  = "VIX"
)

start_date <- "2019-01-01"
end_date   <- Sys.Date()

# ── Download ──────────────────────────────────────────────────────────────────
cat("▶ Downloading portfolio prices...\n")
prices_portfolio <- tq_get(portfolio_tickers, from = start_date, to = end_date,
                           get = "stock.prices") %>%
  mutate(date = as.Date(date), type = "Portfolio")

cat("▶ Downloading index prices...\n")
prices_index <- tq_get(index_tickers, from = start_date, to = end_date,
                       get = "stock.prices") %>%
  mutate(date   = as.Date(date),
         type   = "Index",
         symbol = recode(symbol, !!!index_labels))

all_prices <- bind_rows(prices_portfolio, prices_index)

# ── Helpers ───────────────────────────────────────────────────────────────────
normalize_prices <- function(df) {
  df %>%
    group_by(symbol) %>% arrange(date) %>%
    mutate(y_val = 100 * close / first(na.omit(close))) %>%
    ungroup()
}

index_colors <- c(
  "S&P 500"      = "#FFD700",
  "Dow Jones"    = "#FF8C00",
  "NASDAQ"       = "#00BFFF",
  "Russell 2000" = "#FF69B4",
  "VIX"          = "#FF4500"
)

stock_palette <- setNames(
  scales::hue_pal()(length(portfolio_tickers)),
  portfolio_tickers
)

get_color <- function(sym) {
  if (sym %in% names(index_colors)) index_colors[[sym]]
  else if (sym %in% names(stock_palette)) stock_palette[[sym]]
  else "#aaaaaa"
}

dark <- list(
  bg     = "#0d1117",
  panel  = "#161b22",
  border = "#21262d",
  text   = "#c9d1d9",
  muted  = "#8b949e",
  accent = "#58a6ff"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(paste0("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600&display=swap');

    *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

    body {
      background: ", dark$bg, ";
      color: ", dark$text, ";
      font-family: 'IBM Plex Sans', sans-serif;
      font-size: 13px;
    }

    /* ── Top bar ── */
    .top-bar {
      display: flex;
      align-items: flex-end;
      gap: 16px;
      padding: 14px 20px 10px;
      background: ", dark$panel, ";
      border-bottom: 1px solid ", dark$border, ";
      flex-wrap: wrap;
    }

    .ctrl-group {
      display: flex;
      flex-direction: column;
      gap: 4px;
    }

    .ctrl-label {
      font-size: 10px;
      font-weight: 600;
      letter-spacing: .08em;
      text-transform: uppercase;
      color: ", dark$muted, ";
    }

    /* selectize overrides */
    .selectize-input {
      background: ", dark$bg, " !important;
      border: 1px solid ", dark$border, " !important;
      border-radius: 6px !important;
      color: ", dark$text, " !important;
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 12px !important;
      min-width: 200px;
      max-width: 340px;
      padding: 5px 8px !important;
    }
    .selectize-input.focus { border-color: ", dark$accent, " !important; box-shadow: 0 0 0 2px rgba(88,166,255,.15) !important; }
    .selectize-dropdown {
      background: ", dark$panel, " !important;
      border: 1px solid ", dark$border, " !important;
      border-radius: 6px !important;
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 12px !important;
      color: ", dark$text, " !important;
    }
    .selectize-dropdown .option { padding: 6px 10px; }
    .selectize-dropdown .option:hover,
    .selectize-dropdown .option.active { background: #1f2937 !important; color: ", dark$accent, " !important; }
    .selectize-dropdown .selected { background: rgba(88,166,255,.15) !important; }

    /* item pills */
    .selectize-input .item {
      background: rgba(88,166,255,.15) !important;
      border: 1px solid rgba(88,166,255,.3) !important;
      border-radius: 4px !important;
      color: ", dark$accent, " !important;
      font-size: 11px !important;
      padding: 1px 6px !important;
    }

    /* date inputs */
    .date-wrap input[type='text'] {
      background: ", dark$bg, " !important;
      border: 1px solid ", dark$border, " !important;
      border-radius: 6px !important;
      color: ", dark$text, " !important;
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 12px !important;
      padding: 5px 8px !important;
      width: 130px !important;
    }
    .date-wrap .input-group-btn button {
      background: ", dark$panel, " !important;
      border: 1px solid ", dark$border, " !important;
      color: ", dark$muted, " !important;
    }

    /* radio toggle */
    .radio-toggle label { color: ", dark$text, " !important; font-size: 12px; }
    .radio-toggle .radio { display: inline-flex; align-items: center; gap: 5px; margin-right: 12px; }

    /* tabs */
    .nav-tabs {
      border-bottom: 1px solid ", dark$border, ";
      padding: 0 20px;
      background: ", dark$panel, ";
      margin: 0;
    }
    .nav-tabs > li > a {
      color: ", dark$muted, " !important;
      background: transparent !important;
      border: none !important;
      border-bottom: 2px solid transparent !important;
      border-radius: 0 !important;
      font-size: 12px;
      font-weight: 600;
      letter-spacing: .05em;
      padding: 10px 16px;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li > a:hover {
      color: ", dark$accent, " !important;
      border-bottom: 2px solid ", dark$accent, " !important;
      background: transparent !important;
    }
    .tab-content { padding: 0; }

    /* plot area */
    .plot-wrap { padding: 12px 20px 16px; }
    .shiny-plot-output, .plotly { width: 100% !important; }

    /* title */
    .app-title {
      font-family: 'IBM Plex Mono', monospace;
      font-size: 15px;
      font-weight: 600;
      color: ", dark$accent, ";
      letter-spacing: .04em;
      white-space: nowrap;
      margin-right: 8px;
    }

    /* separator */
    .sep { width: 1px; height: 36px; background: ", dark$border, "; margin: 0 4px; }

    /* hide default shiny chrome */
    .shiny-input-container > label { display: none; }
    h2.shiny-app-title { display: none; }
    .container-fluid { padding: 0 !important; }
  ")))),
  
  # ── TOP CONTROL BAR ──────────────────────────────────────────────────────
  tags$div(class = "top-bar",
           
           tags$span(class = "app-title", "◈ MARKET LAB"),
           tags$div(class = "sep"),
           
           # Holdings multi-select
           tags$div(class = "ctrl-group",
                    tags$span(class = "ctrl-label", "Holdings"),
                    selectizeInput("selected_stocks", label = NULL,
                                   choices  = portfolio_tickers,
                                   selected = c("NVDA", "TSLA", "AMZN"),
                                   multiple = TRUE,
                                   options  = list(placeholder = "Select stocks…", plugins = list("remove_button"))
                    )
           ),
           
           # Indexes multi-select
           tags$div(class = "ctrl-group",
                    tags$span(class = "ctrl-label", "Indexes"),
                    selectizeInput("selected_indexes", label = NULL,
                                   choices  = unname(index_labels),
                                   selected = c("S&P 500", "NASDAQ"),
                                   multiple = TRUE,
                                   options  = list(placeholder = "Select indexes…", plugins = list("remove_button"))
                    )
           ),
           
           tags$div(class = "sep"),
           
           # Date range
           tags$div(class = "ctrl-group date-wrap",
                    tags$span(class = "ctrl-label", "From"),
                    dateInput("start_date", label = NULL,
                              value = as.Date("2022-01-01"),
                              min   = min(all_prices$date),
                              max   = max(all_prices$date)
                    )
           ),
           tags$div(class = "ctrl-group date-wrap",
                    tags$span(class = "ctrl-label", "To"),
                    dateInput("end_date", label = NULL,
                              value = max(all_prices$date),
                              min   = min(all_prices$date),
                              max   = max(all_prices$date)
                    )
           ),
           
           tags$div(class = "sep"),
           
           # Display mode toggle
           tags$div(class = "ctrl-group radio-toggle",
                    tags$span(class = "ctrl-label", "Scale"),
                    radioButtons("display_mode", label = NULL,
                                 choices  = c("Normalized" = "normalized", "Raw Price" = "raw"),
                                 selected = "normalized",
                                 inline   = TRUE
                    )
           )
  ),
  
  # ── TABS + PLOTS ──────────────────────────────────────────────────────────
  tabsetPanel(id = "tabs",
              
              tabPanel("Price Comparison",
                       tags$div(class = "plot-wrap",
                                plotlyOutput("comparison_plot", height = "calc(100vh - 140px)")
                       )
              ),
              
              tabPanel("Return Distribution",
                       tags$div(class = "plot-wrap",
                                plotlyOutput("dist_plot", height = "calc(100vh - 140px)")
                       )
              )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  combined_data <- reactive({
    req(input$start_date, input$end_date)
    req(input$start_date <= input$end_date)
    selected <- c(input$selected_stocks, input$selected_indexes)
    req(length(selected) > 0)
    
    df <- all_prices %>%
      filter(symbol %in% selected,
             date   >= as.Date(input$start_date),
             date   <= as.Date(input$end_date)) %>%
      arrange(symbol, date)
    
    if (input$display_mode == "normalized") {
      df <- normalize_prices(df)
    } else {
      df$y_val <- df$close
    }
    df
  })
  
  # ── Price Comparison ────────────────────────────────────────────────────────
  output$comparison_plot <- renderPlotly({
    df <- combined_data()
    req(nrow(df) > 0)
    
    syms <- unique(df$symbol)
    p    <- plot_ly()
    
    for (s in syms) {
      d     <- filter(df, symbol == s)
      col   <- get_color(s)
      is_idx <- s %in% unname(index_labels)
      
      p <- add_lines(p,
                     data = d, x = ~date, y = ~y_val, name = s,
                     line = list(
                       color = col,
                       width = if (is_idx) 2.2 else 1.4,
                       dash  = if (is_idx) "dash" else "solid"
                     )
      )
    }
    
    y_title <- if (input$display_mode == "normalized") "Indexed (base 100)" else "Price (USD)"
    
    p %>% layout(
      paper_bgcolor = dark$bg,
      plot_bgcolor  = dark$bg,
      font   = list(family = "IBM Plex Mono, monospace", color = dark$text, size = 11),
      margin = list(t = 30, r = 20, b = 50, l = 60),
      xaxis  = list(title = "", gridcolor = dark$border, zerolinecolor = dark$border,
                    tickfont = list(size = 10)),
      yaxis  = list(title = y_title, gridcolor = dark$border, zerolinecolor = dark$border,
                    tickfont = list(size = 10)),
      legend = list(
        orientation = "v", x = 1.01, y = 1,
        font = list(size = 10), bgcolor = "rgba(0,0,0,0)",
        bordercolor = dark$border
      ),
      hovermode = "x unified"
    )
  })
  
  # ── Return Distribution ──────────────────────────────────────────────────────
  output$dist_plot <- renderPlotly({
    df <- combined_data()
    req(nrow(df) > 0)
    
    ret_df <- df %>%
      group_by(symbol) %>% arrange(date) %>%
      mutate(daily_ret = c(NA, diff(log(close))) * 100) %>%
      drop_na(daily_ret) %>%
      ungroup()
    
    syms <- unique(ret_df$symbol)
    p    <- plot_ly()
    
    for (s in syms) {
      d   <- filter(ret_df, symbol == s)
      col <- get_color(s)
      
      p <- add_trace(p,
                     type        = "violin",
                     x           = d$daily_ret,
                     name        = s,
                     orientation = "h",
                     box         = list(visible = TRUE),
                     meanline    = list(visible = TRUE),
                     line        = list(color = col),
                     fillcolor   = paste0(substr(col, 1, 7), "33"),
                     points      = FALSE
      )
    }
    
    p %>% layout(
      paper_bgcolor = dark$bg,
      plot_bgcolor  = dark$bg,
      font   = list(family = "IBM Plex Mono, monospace", color = dark$text, size = 11),
      margin = list(t = 30, r = 20, b = 60, l = 80),
      xaxis  = list(title = "Daily Log-Return (%)", gridcolor = dark$border,
                    zerolinecolor = "#444", zeroline = TRUE, tickfont = list(size = 10)),
      yaxis  = list(gridcolor = dark$border, tickfont = list(size = 10)),
      showlegend = FALSE,
      hovermode  = "closest"
    )
  })
}

shinyApp(ui, server)

# Convertimos a long
prices <- prices_raw %>%
  select(date, symbol, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date)

cat("✅ Prices downloaded:", nrow(prices), "trading days\n")
cat("   Date range:", format(min(prices$date)), "→", format(max(prices$date)), "\n")

# Now create a shiny for returns
# ── Compute daily log returns ────────────────────────────────
returns <- prices_raw %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    type       = "log",
    col_rename = "log_return"
  ) %>%
  ungroup()

# ---- UI ----
ui <- fluidPage(
  
  titlePanel("Portfolio Returns Viewer"),
  
  # ---- TOP CONTROLS ----
  fluidRow(
    column(
      width = 4,
      selectInput(
        "symbol",
        "Stock:",
        choices = unique(returns$symbol),
        selected = unique(returns$symbol)[1]
      )
    ),
    
    column(
      width = 4,
      dateInput(
        "start_date",
        "Start Date:",
        value = min(returns$date),
        min = min(returns$date),
        max = max(returns$date)
      )
    ),
    
    column(
      width = 4,
      dateInput(
        "end_date",
        "End Date:",
        value = max(returns$date),
        min = min(returns$date),
        max = max(returns$date)
      )
    )
  ),
  
  # ---- MAIN PLOT ----
  fluidRow(
    column(
      width = 12,
      plotlyOutput("returns_plot", height = "650px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    
    req(input$symbol, input$start_date, input$end_date)
    req(input$start_date <= input$end_date)
    
    returns %>%
      filter(
        symbol == input$symbol,
        date >= as.Date(input$start_date),
        date <= as.Date(input$end_date)
      )
  })
  
  output$returns_plot <- renderPlotly({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    plot_ly(
      data = df,
      x = ~date,
      y = ~log_return,
      type = "scatter",
      mode = "lines",
      name = input$symbol
    ) %>%
      layout(
        title = paste("Daily Log Returns:", input$symbol),
        xaxis = list(
          title = "Date",
          rangeslider = list(visible = TRUE)
        ),
        yaxis = list(title = "Log Return")
      )
  })
}

# ---- RUN ----
shinyApp(ui, server)


# ── Descriptive statistics ───────────────────────────────────
desc_stats <- returns %>%
  group_by(symbol) %>%
  summarise(
    Mean_Daily  = mean(log_return,  na.rm = TRUE),
    SD_Daily    = sd(log_return,    na.rm = TRUE),
    Mean_Annual = Mean_Daily * 252,
    SD_Annual   = SD_Daily   * sqrt(252),
    Skewness    = skewness(log_return, na.rm = TRUE),
    Kurtosis    = kurtosis(log_return, na.rm = TRUE),
    Min         = min(log_return,   na.rm = TRUE),
    Max         = max(log_return,   na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   DESCRIPTIVE STATISTICS (LOG RETURNS)\n")
cat("═══════════════════════════════════════════════════════════\n")
print(desc_stats)

# ── Plot: rebased price performance ─────────────────────────
prices_rebased <- prices_raw %>%
  group_by(symbol) %>%
  mutate(rebased = 100 * adjusted / first(adjusted)) %>%
  ungroup()

p1 <- ggplot(prices_rebased, aes(x = date, y = rebased, color = symbol)) +
  geom_line(linewidth = 0.6) +
  labs(
    title    = "Buffett Portfolio — Rebased Price Performance (Base = 100)",
    subtitle = paste(start_date, "to", end_date),
    x = NULL, y = "Indexed Price", color = "Ticker"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p1)

# ════════════════════════════════════════════════════════════
# MODULE 2 — PORTFOLIO OPTIMIZATION & EFFICIENT FRONTIER
# ════════════════════════════════════════════════════════════

cat("\n▶ MODULE 2 — Portfolio Optimization...\n")

ret_matrix <- returns_wide %>% select(-date) %>% as.matrix()
n      <- length(tickers)
mu     <- colMeans(ret_matrix, na.rm = TRUE)
Sigma  <- cov(ret_matrix, use = "complete.obs")

rf_annual <- 0.045
rf_daily  <- rf_annual / 252

# ── QP Solver: min variance given optional return target ─────
min_var_portfolio <- function(Sigma, mu, target_ret = NULL,
                              w_min = 0.02, w_max = 0.40) {
  n    <- length(mu)
  Dmat <- 2 * Sigma
  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), diag(n), -diag(n))
  bvec <- c(1, rep(w_min, n), rep(-w_max, n))
  meq  <- 1
  if (!is.null(target_ret)) {
    Amat <- cbind(Amat, mu)
    bvec <- c(bvec, target_ret)
  }
  sol <- tryCatch(
    solve.QP(Dmat, dvec, Amat, bvec, meq = meq),
    error = function(e) NULL
  )
  if (is.null(sol)) return(NULL)
  w <- pmax(sol$solution, 0)
  w / sum(w)
}

# ── Minimum Variance Portfolio ───────────────────────────────
weights_minvar        <- min_var_portfolio(Sigma, mu)
names(weights_minvar) <- tickers

# ── Efficient Frontier + Max Sharpe (grid search) ────────────
target_rets <- seq(min(mu) * 1.01, max(mu) * 0.99, length.out = 100)

frontier_df <- map_dfr(target_rets, function(tr) {
  w <- min_var_portfolio(Sigma, mu, target_ret = tr)
  if (is.null(w)) return(NULL)
  port_ret <- sum(w * mu)
  port_vol <- sqrt(t(w) %*% Sigma %*% w)[1]
  tibble(
    Return     = port_ret * 252,
    Volatility = port_vol * sqrt(252),
    Sharpe     = (port_ret - rf_daily) / port_vol * sqrt(252),
    weights    = list(w)
  )
})

best_idx              <- which.max(frontier_df$Sharpe)
weights_sharpe        <- frontier_df$weights[[best_idx]]
names(weights_sharpe) <- tickers

# ── Equal-Weight Portfolio ───────────────────────────────────
weights_equal        <- rep(1 / n, n)
names(weights_equal) <- tickers

# ── Summary table ────────────────────────────────────────────
port_stats <- function(w, label) {
  ann_ret <- sum(w * mu) * 252
  ann_vol <- sqrt(t(w) %*% Sigma %*% w)[1] * sqrt(252)
  tibble(
    Portfolio         = label,
    Annual_Return     = round(ann_ret, 4),
    Annual_Volatility = round(ann_vol, 4),
    Sharpe_Ratio      = round((ann_ret - rf_annual) / ann_vol, 4)
  )
}

summary_table <- bind_rows(
  port_stats(weights_equal,  "Equal-Weight"),
  port_stats(weights_minvar, "Min Variance"),
  port_stats(weights_sharpe, "Max Sharpe")
)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   PORTFOLIO COMPARISON SUMMARY\n")
cat("═══════════════════════════════════════════════════════════\n")
print(summary_table)

# ── Efficient Frontier Plot ──────────────────────────────────
set.seed(42)
sim_results <- map_dfr(seq_len(3000), function(i) {
  w <- runif(n); w <- w / sum(w)
  tibble(
    Return     = sum(w * mu) * 252,
    Volatility = sqrt(t(w) %*% Sigma %*% w)[1] * sqrt(252),
    Sharpe     = (sum(w * mu) * 252 - rf_annual) /
      (sqrt(t(w) %*% Sigma %*% w)[1] * sqrt(252))
  )
})

key_portfolios <- tibble(
  Label      = c("Equal-Weight", "Min Variance", "Max Sharpe"),
  Return     = summary_table$Annual_Return,
  Volatility = summary_table$Annual_Volatility
)

asset_stats <- tibble(
  Label      = tickers,
  Return     = as.numeric(mu) * 252,
  Volatility = sqrt(diag(Sigma)) * sqrt(252)
)

p2 <- ggplot() +
  geom_point(data = sim_results,
             aes(x = Volatility, y = Return, color = Sharpe),
             alpha = 0.3, size = 0.8) +
  geom_line(data = frontier_df,
            aes(x = Volatility, y = Return),
            color = "white", linewidth = 1.2, linetype = "dashed") +
  scale_color_viridis_c(name = "Sharpe", option = "plasma") +
  geom_point(data = key_portfolios,
             aes(x = Volatility, y = Return),
             shape = 23, size = 5, fill = "gold", color = "black") +
  geom_label_repel(data = key_portfolios,
                   aes(x = Volatility, y = Return, label = Label), size = 3) +
  geom_point(data = asset_stats,
             aes(x = Volatility, y = Return),
             color = "cyan", size = 3, shape = 17) +
  geom_text_repel(data = asset_stats,
                  aes(x = Volatility, y = Return, label = Label),
                  size = 2.5, color = "cyan") +
  labs(
    title    = "Efficient Frontier — Buffett Portfolio Universe",
    subtitle = "Dashed = analytical frontier | Dots = 3,000 random portfolios",
    x = "Annual Volatility (σ)", y = "Annual Return (μ)"
  ) +
  theme_dark(base_size = 12)

print(p2)

# ── Weight Comparison Chart ──────────────────────────────────
weight_df <- tibble(
  Ticker         = tickers,
  `Equal-Weight` = weights_equal,
  `Min Variance` = weights_minvar,
  `Max Sharpe`   = weights_sharpe
) %>%
  pivot_longer(-Ticker, names_to = "Strategy", values_to = "Weight")

p3 <- ggplot(weight_df, aes(x = Ticker, y = Weight, fill = Strategy)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Portfolio Weights by Strategy",
       x = NULL, y = "Allocation", fill = "Strategy") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p3)

# Build xts for downstream modules
returns_xts <- returns_wide %>%
  select(-date) %>%
  xts(order.by = returns_wide$date)

# ════════════════════════════════════════════════════════════
# MODULE 3 — RISK METRICS
# VaR, CVaR, Sharpe, Sortino, Beta, Drawdowns
# ════════════════════════════════════════════════════════════

cat("\n▶ MODULE 3 — Risk Metrics...\n")

# ── Individual asset risk metrics ───────────────────────────
var_95  <- sapply(tickers, function(tk) as.numeric(VaR(returns_xts[, tk],  p = 0.95, method = "historical")))
cvar_95 <- sapply(tickers, function(tk) as.numeric(CVaR(returns_xts[, tk], p = 0.95, method = "historical")))
sharpe  <- sapply(tickers, function(tk) as.numeric(SharpeRatio.annualized(returns_xts[, tk], Rf = rf_daily, scale = 252)))
sortino <- sapply(tickers, function(tk) as.numeric(SortinoRatio(returns_xts[, tk], MAR = rf_daily)[1]))

risk_table <- tibble(
  Ticker  = tickers,
  VaR_95  = round(as.numeric(var_95),  4),
  CVaR_95 = round(as.numeric(cvar_95), 4),
  Sharpe  = round(as.numeric(sharpe),  4),
  Sortino = round(as.numeric(sortino), 4)
)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   INDIVIDUAL ASSET RISK METRICS (Daily, 95% CI)\n")
cat("═══════════════════════════════════════════════════════════\n")
print(risk_table)

# ── Beta & Alpha vs S&P 500 ──────────────────────────────────
spy_raw <- tq_get("SPY",
                  from = index(returns_xts)[1],
                  to   = index(returns_xts)[length(index(returns_xts))]) %>%
  tq_transmute(adjusted, periodReturn, period = "daily",
               type = "log", col_rename = "SPY")

spy_xts      <- xts(spy_raw$SPY, order.by = spy_raw$date)
common_dates <- as.Date(intersect(as.character(index(returns_xts)),
                                  as.character(index(spy_xts))))
ret_aligned  <- returns_xts[common_dates, ]
spy_aligned  <- spy_xts[common_dates]

betas  <- map_dbl(tickers, ~ CAPM.beta(Ra = ret_aligned[, .x], Rb = spy_aligned))
alphas <- map_dbl(tickers, ~ CAPM.alpha(Ra = ret_aligned[, .x],
                                        Rb = spy_aligned, Rf = rf_daily))

capm_table <- tibble(
  Ticker = tickers,
  Beta   = round(betas,         3),
  Alpha  = round(alphas * 252,  4)
) %>%
  mutate(Interpretation = case_when(
    Beta > 1.2 ~ "High systematic risk",
    Beta > 0.8 ~ "Market-like",
    Beta > 0   ~ "Defensive",
    TRUE       ~ "Counter-cyclical"
  ))

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   CAPM BETAS & ANNUALISED ALPHAS vs S&P 500 (SPY)\n")
cat("═══════════════════════════════════════════════════════════\n")
print(capm_table)

# ── Portfolio-level risk ─────────────────────────────────────
port_equal_r  <- Return.portfolio(ret_aligned, weights = weights_equal)
port_minvar_r <- Return.portfolio(ret_aligned, weights = weights_minvar)
port_sharpe_r <- Return.portfolio(ret_aligned, weights = weights_sharpe)

port_risk_summary <- function(port_ret, label) {
  tibble(
    Portfolio     = label,
    VaR_95_daily  = round(VaR(port_ret,  p = 0.95, method = "historical"), 4),
    CVaR_95_daily = round(CVaR(port_ret, p = 0.95, method = "historical"), 4),
    Max_Drawdown  = round(maxDrawdown(port_ret), 4),
    Calmar_Ratio  = round(
      as.numeric(Return.annualized(port_ret, 252)) / maxDrawdown(port_ret), 3)
  )
}

port_risk_table <- bind_rows(
  port_risk_summary(port_equal_r,  "Equal-Weight"),
  port_risk_summary(port_minvar_r, "Min Variance"),
  port_risk_summary(port_sharpe_r, "Max Sharpe")
)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   PORTFOLIO RISK SUMMARY\n")
cat("═══════════════════════════════════════════════════════════\n")
print(port_risk_table)

# ── Drawdown chart ───────────────────────────────────────────
all_port_ret <- merge(port_equal_r, port_minvar_r, port_sharpe_r)
colnames(all_port_ret) <- c("Equal-Weight", "Min Variance", "Max Sharpe")

chart.Drawdown(all_port_ret,
               main = "Portfolio Drawdowns Comparison",
               legend.loc = "bottomleft",
               colorset = c("#E74C3C", "#3498DB", "#2ECC71"))

# ── Rolling 6-month Sharpe ───────────────────────────────────
window_days <- 126

roll_fn <- function(port_ret) {
  rollapply(port_ret, window_days,
            FUN = function(x) SharpeRatio.annualized(x, Rf = rf_daily * window_days),
            fill = NA, align = "right")
}

roll_all <- merge(roll_fn(port_equal_r),
                  roll_fn(port_minvar_r),
                  roll_fn(port_sharpe_r))
colnames(roll_all) <- c("Equal-Weight", "Min Variance", "Max Sharpe")

chart.TimeSeries(roll_all,
                 main = "Rolling 6-Month Sharpe Ratio (Annualised)",
                 ylab = "Sharpe Ratio", legend.loc = "topleft",
                 colorset = c("#E74C3C", "#3498DB", "#2ECC71"))

# ── Correlation heatmap ──────────────────────────────────────
cor_matrix <- cor(returns_xts, use = "complete.obs")

cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column("Asset1") %>%
  pivot_longer(-Asset1, names_to = "Asset2", values_to = "Correlation")

p4 <- ggplot(cor_long, aes(x = Asset1, y = Asset2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "#3498DB", mid = "white", high = "#E74C3C",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Asset Return Correlation Matrix", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# ════════════════════════════════════════════════════════════
# MODULE 4 — MACHINE LEARNING: PREDICTING STOCK RETURNS
# Features: Technical Indicators + Lagged Returns
# Models: LASSO, Random Forest, XGBoost
# ════════════════════════════════════════════════════════════

cat("\n▶ MODULE 4 — Machine Learning Models...\n")

# ── Feature Engineering ──────────────────────────────────────
engineer_features <- function(ticker_sym, prices_raw, n_ahead = 5) {
  df <- prices_raw %>%
    filter(symbol == ticker_sym) %>%
    arrange(date) %>%
    select(date, adjusted, volume)
  
  lag_ret <- function(x, k) log(x / lag(x, k))
  
  df %>%
    mutate(
      target      = lead(lag_ret(adjusted, 1), n_ahead - 1),
      ret_1d      = lag_ret(adjusted, 1),
      ret_5d      = lag_ret(adjusted, 5),
      ret_10d     = lag_ret(adjusted, 10),
      ret_21d     = lag_ret(adjusted, 21),
      ma5         = SMA(adjusted, 5),
      ma20        = SMA(adjusted, 20),
      ma_ratio    = SMA(adjusted, 5) / SMA(adjusted, 20) - 1,
      momentum_10 = adjusted / lag(adjusted, 10) - 1,
      momentum_21 = adjusted / lag(adjusted, 21) - 1,
      vol_21      = runSD(lag_ret(adjusted, 1), 21),
      rsi_14      = RSI(adjusted, 14),
      macd_line   = MACD(adjusted, 12, 26, 9)[, "macd"],
      macd_signal = MACD(adjusted, 12, 26, 9)[, "signal"],
      macd_hist   = MACD(adjusted, 12, 26, 9)[, "macd"] -
        MACD(adjusted, 12, 26, 9)[, "signal"],
      bb_upper    = BBands(adjusted, 20)[, "up"],
      bb_lower    = BBands(adjusted, 20)[, "dn"],
      bb_pct      = (adjusted - BBands(adjusted, 20)[, "dn"]) /
        (BBands(adjusted, 20)[, "up"] -
           BBands(adjusted, 20)[, "dn"]),
      vol_ratio   = volume / SMA(volume, 20),
      ticker      = ticker_sym
    ) %>%
    select(-adjusted, -volume, -ma5, -ma20, -bb_upper, -bb_lower) %>%
    drop_na()
}

cat("   Engineering features for all tickers...\n")
all_features <- map_dfr(tickers, engineer_features, prices_raw = prices_raw)
cat("✅  Feature matrix:", nrow(all_features), "rows ×",
    ncol(all_features) - 3, "features\n")

# ── Walk-Forward Train/Test Split (AAPL as example) ──────────
ticker_data  <- filter(all_features, ticker == "AAPL") %>% arrange(date)
split_idx    <- floor(0.70 * nrow(ticker_data))
train_data   <- ticker_data[1:split_idx, ]
test_data    <- ticker_data[(split_idx + 1):nrow(ticker_data), ]
feature_cols <- setdiff(names(ticker_data), c("date", "target", "ticker"))

X_train <- as.matrix(train_data[, feature_cols])
y_train <- train_data$target
X_test  <- as.matrix(test_data[,  feature_cols])
y_test  <- test_data$target

cat("   Training period:", format(min(train_data$date)), "→",
    format(max(train_data$date)), "\n")
cat("   Testing period: ", format(min(test_data$date)),  "→",
    format(max(test_data$date)),  "\n\n")

# ── Model 1: LASSO ───────────────────────────────────────────
cat("   Training LASSO...\n")
set.seed(42)
cv_lasso   <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
lasso_pred <- as.numeric(predict(cv_lasso, newx = X_test,
                                 s = cv_lasso$lambda.1se))  # less aggressive than lambda.min

rmse_lasso <- sqrt(mean((lasso_pred - y_test)^2))
cor_lasso  <- ifelse(sd(lasso_pred) == 0, 0, cor(lasso_pred, y_test))

cat("   LASSO   — RMSE:", round(rmse_lasso, 5),
    "| Corr:", round(cor_lasso, 4), "\n")

# ── Model 2: Random Forest ───────────────────────────────────
cat("   Training Random Forest...\n")
set.seed(42)
rf_model <- ranger(
  x             = as.data.frame(X_train),
  y             = y_train,
  num.trees     = 500,
  mtry          = floor(sqrt(length(feature_cols))),
  importance    = "permutation",
  min.node.size = 10
)
rf_pred  <- predict(rf_model, data = as.data.frame(X_test))$predictions
rmse_rf  <- sqrt(mean((rf_pred - y_test)^2))
cor_rf   <- cor(rf_pred, y_test)
cat("   RF      — RMSE:", round(rmse_rf,  5),
    "| Corr:", round(cor_rf,  4), "\n")

# ── Model 3: XGBoost ─────────────────────────────────────────
cat("   Training XGBoost...\n")
dtrain <- xgb.DMatrix(X_train, label = y_train)
dtest  <- xgb.DMatrix(X_test,  label = y_test)
set.seed(42)
xgb_model <- xgb.train(
  params    = list(objective = "reg:squarederror",
                   eta = 0.05, max_depth = 4,
                   subsample = 0.8, colsample_bytree = 0.8),
  data      = dtrain,
  nrounds   = 300,
  watchlist = list(test = dtest),
  verbose   = 0,
  early_stopping_rounds = 20
)
xgb_pred <- predict(xgb_model, dtest)
rmse_xgb <- sqrt(mean((xgb_pred - y_test)^2))
cor_xgb  <- cor(xgb_pred, y_test)
cat("   XGBoost — RMSE:", round(rmse_xgb, 5),
    "| Corr:", round(cor_xgb, 4), "\n")

# ── Naive Benchmark ──────────────────────────────────────────
naive_pred  <- lag(y_test)[-1]
rmse_naive  <- sqrt(mean((naive_pred - y_test[-1])^2, na.rm = TRUE))

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   MODEL PERFORMANCE COMPARISON (AAPL, out-of-sample)\n")
cat("═══════════════════════════════════════════════════════════\n")
ml_results <- tibble(
  Model       = c("Naive (Random Walk)", "LASSO", "Random Forest", "XGBoost"),
  RMSE        = round(c(rmse_naive, rmse_lasso, rmse_rf, rmse_xgb), 6),
  Correlation = round(c(NA, cor_lasso, cor_rf, cor_xgb), 4)
)
print(ml_results)

# ── Feature Importance Plot (XGBoost) ────────────────────────
xgb_imp <- xgb.importance(model = xgb_model) %>% head(10)

p5 <- ggplot(xgb_imp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#E67E22") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance (Top 10) — AAPL",
       x = "Feature", y = "Gain") +
  theme_minimal(base_size = 12)

print(p5)

# ── Predicted vs Actual Plot ─────────────────────────────────
pred_df <- tibble(
  Date    = test_data$date,
  Actual  = y_test,
  LASSO   = lasso_pred,
  RF      = rf_pred,
  XGBoost = xgb_pred
) %>%
  pivot_longer(-Date, names_to = "Model", values_to = "Return")

p6 <- ggplot(pred_df, aes(x = Date, y = Return, color = Model)) +
  geom_line(alpha = 0.8, linewidth = 0.5) +
  labs(title = "Predicted vs Actual 5-Day Forward Returns — AAPL (Test Set)",
       x = NULL, y = "5-Day Log Return", color = "Model") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p6)

# ════════════════════════════════════════════════════════════
# MODULE 5 — BACKTESTING
# ML Signal Strategy vs Passive Benchmarks
# ════════════════════════════════════════════════════════════

cat("\n▶ MODULE 5 — Backtesting...\n")

# ── Generate signals for all tickers on the test period ──────
all_dates  <- sort(unique(all_features$date))
split_date <- all_dates[floor(0.70 * length(all_dates))]

test_features <- all_features %>%
  filter(date > split_date) %>%
  arrange(ticker, date)

X_all_test <- as.matrix(test_features[, feature_cols])
test_features$xgb_pred <- predict(xgb_model, xgb.DMatrix(X_all_test))

# ── Each day: long top-5 tickers by predicted return ─────────
signal_weights <- test_features %>%
  group_by(date) %>%
  arrange(date, desc(xgb_pred)) %>%
  mutate(rank = row_number(),
         weight = if_else(rank <= 5, 1/5, 0)) %>%
  ungroup() %>%
  select(date, ticker, weight)

daily_actual_returns <- returns %>%
  filter(date > split_date) %>%
  rename(ticker = symbol)

ml_portfolio_ret <- signal_weights %>%
  left_join(daily_actual_returns, by = c("date", "ticker")) %>%
  group_by(date) %>%
  summarise(port_return = sum(weight * log_return, na.rm = TRUE)) %>%
  ungroup()

ml_ret_xts <- xts(ml_portfolio_ret$port_return,
                  order.by = ml_portfolio_ret$date)

# ── Benchmark portfolios ──────────────────────────────────────
test_dates     <- ml_portfolio_ret$date
ret_test_xts   <- returns_xts[test_dates]
equal_ret_xts  <- Return.portfolio(ret_test_xts, weights = weights_equal)
minvar_ret_xts <- Return.portfolio(ret_test_xts, weights = weights_minvar)

spy_bt <- tq_get("SPY", from = min(test_dates), to = max(test_dates)) %>%
  tq_transmute(adjusted, periodReturn, period = "daily",
               type = "log", col_rename = "spy_ret")
spy_xts_bt <- xts(spy_bt$spy_ret, order.by = spy_bt$date)

common_test <- Reduce(intersect,
                      list(index(ml_ret_xts), index(equal_ret_xts),
                           index(minvar_ret_xts), index(spy_xts_bt)))

all_strategies <- merge(
  ml_ret_xts,
  equal_ret_xts,
  minvar_ret_xts,
  spy_xts_bt,
  join = "inner"   # keeps only common dates automatically
)

colnames(all_strategies) <- c("ML Signal", "Equal-Weight",
                              "Min Variance", "SPY Benchmark")
# ── Performance Table ─────────────────────────────────────────
cat("\n═══════════════════════════════════════════════════════════\n")
cat("   ANNUALISED PERFORMANCE — OUT-OF-SAMPLE BACKTEST\n")
cat("═══════════════════════════════════════════════════════════\n")
print(round(table.AnnualizedReturns(all_strategies,
                                    Rf = rf_daily, scale = 252), 4))

perf_extra <- tibble(
  Strategy     = colnames(all_strategies),
  Max_Drawdown = map_dbl(seq_len(ncol(all_strategies)),
                         ~ round(maxDrawdown(all_strategies[, .x]), 4)),
  Calmar       = map_dbl(seq_len(ncol(all_strategies)), function(i) {
    round(as.numeric(Return.annualized(all_strategies[, i], 252)) /
            maxDrawdown(all_strategies[, i]), 3)
  }),
  Win_Rate     = map_dbl(seq_len(ncol(all_strategies)),
                         ~ round(mean(all_strategies[, .x] > 0, na.rm = TRUE), 4))
)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   ADDITIONAL RISK METRICS\n")
cat("═══════════════════════════════════════════════════════════\n")
print(perf_extra)

# ── Cumulative Returns Chart ──────────────────────────────────
chart.CumReturns(
  all_strategies,
  main         = "Cumulative Returns — Out-of-Sample Backtest",
  legend.loc   = "topleft",
  colorset     = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
  wealth.index = TRUE,
  begin        = "first"
)

# ── Full Performance Summary ─────────────────────────────────
charts.PerformanceSummary(
  all_strategies,
  main       = "Performance Summary — All Strategies",
  colorset   = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
  legend.loc = "topleft"
)

# ── Transaction Cost Sensitivity ─────────────────────────────
ml_net_ret  <- ml_ret_xts - 0.001 / 252
net_compare <- merge(ml_ret_xts, ml_net_ret, join = "inner")
colnames(net_compare) <- c("ML Gross", "ML Net (10bps)")

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   TRANSACTION COST SENSITIVITY — ML STRATEGY\n")
cat("═══════════════════════════════════════════════════════════\n")
print(round(table.AnnualizedReturns(net_compare,
                                    Rf = rf_daily, scale = 252), 4))

# ════════════════════════════════════════════════════════════
# DONE
# ════════════════════════════════════════════════════════════

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║                  LAB COMPLETE!                           ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

cat("💡 Discussion Questions:\n")
cat("  1. Which portfolio strategy had the best risk-adjusted return?\n")
cat("  2. Does the ML strategy outperform after transaction costs?\n")
cat("  3. Is the XGBoost outperformance consistent across all years?\n")
cat("  4. Which features drive predictions the most, and why?\n")
cat("  5. What are the risks of overfitting in financial ML models?\n\n")
