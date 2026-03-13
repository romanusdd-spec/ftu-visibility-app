# app.R — FTU Visibility Portal (Commercial + Ops)
# Commercial behavior preserved (only CSS sizing/spacing tweaks)
# Ops: Utilization + Compliance + Revenue actualization (single-page 2x2)

library(shiny)
library(dplyr)
library(lubridate)
library(readr)

has_arrow  <- requireNamespace("arrow", quietly = TRUE)
has_plotly <- requireNamespace("plotly", quietly = TRUE)

if (!has_plotly) stop("Package 'plotly' is required. Install: install.packages('plotly')")

library(plotly)
library(tidyr)

# -----------------------------
# Paths (bundle-safe, production-safe)
# -----------------------------
# -----------------------------
# Paths (bundle-safe, production-safe)
# -----------------------------

# Prefer Shiny's app directory when available (runApp / shinyapps.io)
app_dir <- shiny::getShinyOption("appdir")

# If running interactively in RStudio, use the currently opened app.R location
if (is.null(app_dir) || !nzchar(app_dir) || !dir.exists(app_dir)) {
  app_dir <- tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      p <- rstudioapi::getActiveDocumentContext()$path
      if (nzchar(p)) dirname(p) else NULL
    } else NULL
  }, error = function(e) NULL)
}

# Final fallback
if (is.null(app_dir) || !nzchar(app_dir) || !dir.exists(app_dir)) {
  app_dir <- getwd()
}

app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)

# Force relative paths to resolve from bundle root
setwd(app_dir)

commercial_dir <- file.path(app_dir, "data", "commercial")
ops_dir        <- file.path(app_dir, "data", "ops")

if (!dir.exists(commercial_dir)) {
  stop("Bundle data folder not found. Expected ./data/commercial. ",
       "Make sure you deployed the publish_bundle folder.")
}

# Commercial mart paths
path_detail_parquet <- file.path(commercial_dir, "fact_wo_submission_detail.parquet")
path_detail_csv     <- file.path(commercial_dir, "fact_wo_submission_detail.csv")

# Ops mart paths
path_ops_daily   <- file.path(ops_dir, "fact_truck_utilization_summary_daily.csv")
path_ops_by_type <- file.path(ops_dir, "fact_truck_utilization_summary_daily_by_type.csv")

path_kpi_loc_compliance_mtd       <- file.path(ops_dir, "kpi_loc_compliance_mtd.csv")
path_loc_compliance_trend_mtd_cum <- file.path(ops_dir, "fact_loc_compliance_trend_mtd_cumulative.csv")

# -----------------------------
# Helpers
# -----------------------------
normalize_customer <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

fmt_num <- function(x, digits = 0) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_bn <- function(x_idr) {
  # IDR -> Bn IDR
  if (is.na(x_idr) || length(x_idr) == 0) return("-")
  sprintf("%.2f Bn", as.numeric(x_idr) / 1e9)
}

read_mart_detail <- function() {
  if (has_arrow && file.exists(path_detail_parquet)) {
    df <- arrow::read_parquet(path_detail_parquet)
  } else if (file.exists(path_detail_csv)) {
    df <- read_csv(path_detail_csv, show_col_types = FALSE)
  } else {
    stop(
      paste0(
        "Cannot find fact_wo_submission_detail mart (csv/parquet).\n",
        "Expected file: ", path_detail_parquet, " or ", path_detail_csv, "\n",
        "Working dir: ", app_dir, "\n",
        "Looking in: ", commercial_dir
      )
    )
  }
  
  df %>%
    mutate(
      OrderDate       = as.Date(OrderDate),
      OrderOffice     = as.character(OrderOffice),
      Customer        = normalize_customer(Customer),
      customer_key    = toupper(normalize_customer(Customer)),
      wo_month        = format(OrderDate, "%Y-%m"),
      est_rate_per_kg = as.numeric(est_rate_per_kg),
      est_revenue     = as.numeric(est_revenue),
      GrossWeight     = as.numeric(GrossWeight)
    )
}

soft_palette <- function(n) {
  base <- c(
    "#475569", "#64748b", "#94a3b8", "#334155", "#6b7280",
    "#9ca3af", "#1f2937", "#a1a1aa", "#7c8796", "#5b6472"
  )
  if (n <= length(base)) return(base[seq_len(n)])
  rep(base, length.out = n)
}

ops_read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing ops mart: ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

# ============================================================
# COMMERCIAL UI (Approved behavior; only sizing/spacing CSS tweaks)
# ============================================================
commercial_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f8fafc; }

    /* Title spacing (push higher) */
    .titleWrap { margin: 1px 0 6px; }
    .titleMain { font-size: 22px; font-weight: 800; letter-spacing: -0.3px; margin-bottom: 1px; }
    .titleSub  { color:#6b7280; margin-bottom: 1px; }

    /* KPI cards (same look, smaller height + smaller value font) */
    .kpiCard {
      background:#fff;
      border-radius:16px;
      padding:8px 14px 10px 14px;
      box-shadow: 0 8px 30px rgba(15, 23, 42, 0.06);
      border: 1px solid rgba(148,163,184,0.35);
      height: 75px;
      margin-bottom: 12px;
      padding:8px 14px 10px 14px;
    }
    .kpiTitle { font-size: 11px; color:#6b7280; margin-bottom: 3px; margin-top: 2px; }
    .kpiValue { font-size: 20px; font-weight: 800; color:#111827; line-height: 1; }
    .kpiNote  { font-size: 10px; color:#6b7280; margin-top: 6px; }
    
    /* ===== Commercial KPI row (custom width allocation) ===== */
    .kpiRowComm {
      display:flex;
      gap:12px;
      flex-wrap:nowrap;
      margin-bottom: 8px;
    }
    .kpiCard.kpiComm { min-width:0; }
    
    /* 12-grid: 3,3,1.5,1.5,3 => 23.5%,23.5%,12.5%,12.5%,23.5% */
    .kpiComm:nth-child(1) { flex: 0 0 23.5%; }
    .kpiComm:nth-child(2) { flex: 0 0 23.5%; }
    .kpiComm:nth-child(3) { flex: 0 0 12.5%; }
    .kpiComm:nth-child(4) { flex: 0 0 12.5%; }
    .kpiComm:nth-child(5) { flex: 0 0 23.5%; }
    
    /* ===== Small site badge ===== */
    .badgeSite{
      display:inline-block;
      padding:2px 8px;
      border-radius:999px;
      font-size:12px;
      font-weight:700;
      color:#fff;
      margin-left:8px;
      vertical-align:middle;
      line-height:1.2;
    }
    .badgeCLG{ background:#1f77b4; }  /* blue */
    .badgeSBY{ background:#ff7f0e; }  /* orange */
    
    .panelDaily .tinyNote{
      margin-top: 2px !important;
      padding-top: 0 !important;
      line-height: 1.1;
    }
    
    /* Panels */
    .panel {
      background:#fff;
      border-radius:16px;
      padding:16px 18px;
      box-shadow: 0 8px 30px rgba(15, 23, 42, 0.06);
      border: 1px solid rgba(148,163,184,0.35);
      margin-bottom: 14px;
    }
    .panelTitle { font-weight: 800; margin-bottom: 10px; }
    .tinyNote { color:#6b7280; font-size: 11px; margin-top: 8px; }
  "))),
  
  div(class="titleWrap",
      div(class="titleMain", "Commercial Submission — Potential Revenue"),
      div(class="titleSub", "Snapshot-based rate • MTD = latest month in data • NA-safe"),
      uiOutput("fatal_ui")
  ),
  
  div(class="kpiRowComm",
      div(class="kpiCard kpiComm",
          div(class="kpiTitle","Cilegon — MTD Submitted Potential Revenue"),
          div(class="kpiValue", textOutput("kpi_mtd_rev_clg")),
          div(class="kpiNote",  textOutput("kpi_mtd_note"))
      ),
      div(class="kpiCard kpiComm",
          div(class="kpiTitle","Surabaya — MTD Submitted Potential Revenue"),
          div(class="kpiValue", textOutput("kpi_mtd_rev_sby")),
          div(class="kpiNote",  textOutput("kpi_mtd_month"))
      ),
      div(class="kpiCard kpiComm",
          div(class="kpiTitle","Cilegon — MTD WO"),
          div(class="kpiValue", textOutput("kpi_mtd_wo_clg")),
          div(class="kpiNote",  HTML("&nbsp;"))
      ),
      div(class="kpiCard kpiComm",
          div(class="kpiTitle","Surabaya — MTD WO"),
          div(class="kpiValue", textOutput("kpi_mtd_wo_sby")),
          div(class="kpiNote",  HTML("&nbsp;"))
      ),
      div(class="kpiCard kpiComm",
          div(class="kpiTitle","MTD Coverage (WO with rate) — Global"),
          div(class="kpiValue", textOutput("kpi_cov")),
          div(class="kpiNote", "Coverage indicates monetization readiness")
      )
  ),
  
  fluidRow(
    column(
      width = 6,
      div(class="panel panelDaily",
          div(class="panelTitle",
              HTML('Daily Submitted Potential Revenue <span class="badgeSite badgeCLG">CLG</span> (IDR Mn)')
          ),
          plotlyOutput("plot_daily_clg", height = 170),
          div(class="tinyNote", "Click a bar to filter day • Double-click to clear day.")
      ),
      div(style="height:0px;"),
      div(class="panel panelDaily",
          div(class="panelTitle",
              HTML('Daily Submitted Potential Revenue <span class="badgeSite badgeSBY">SBY</span> (IDR Mn)')
          ),
          plotlyOutput("plot_daily_sby", height = 170),
          div(class="tinyNote", "Click a bar to filter day • Double-click to clear day.")
      )
    ),
    column(
      width = 6,
      div(class="panel",
          div(class="panelTitle", "Customer Contribution (MTD) — Global"),
          plotlyOutput("plot_treemap", height = 420),
          div(class="tinyNote", "Click a tile to filter customer • Double-click to clear customer.")
      )
    )
  ),
)

# ============================================================
# OPS UI (single-page 2x2; equal-width KPI row; smaller title)
# ============================================================
ops_ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f8fafc; }

    /* ===== Title: match Commercial format ===== */
    .titleWrap { margin: 1px 0 6px; }
    .titleMain { font-size: 22px; font-weight: 800; letter-spacing: -0.3px; margin-bottom: 1px; }
    .titleSub  { color:#6b7280; margin-bottom: 1px; }

    /* ===== KPI row: keep your Ops width allocation ===== */
    .kpiRow {
      display:flex;
      gap:12px;
      flex-wrap:nowrap;
      margin-bottom: 8px;
    }

    .kpiCard.kpiMini{
      height: 75px !important;
      padding: 14px 16px !important;
      margin-bottom: 10px !important;
      min-width: 0;
      display:flex;
      flex-direction:column;
      justify-content:center;
      background:#fff;
      border-radius:16px;
      box-shadow: 0 8px 30px rgba(15, 23, 42, 0.06);
      border: 1px solid rgba(148,163,184,0.35);
    }

    /* 5 KPI cards: 2,2,3,3,2 (sum=12) => 16.5%,16.5%,25%,25%,17% */
    .kpiMini:nth-child(1) { flex: 0 0 16.5%; }
    .kpiMini:nth-child(2) { flex: 0 0 16.5%; }
    .kpiMini:nth-child(3) { flex: 0 0 25.0%; }
    .kpiMini:nth-child(4) { flex: 0 0 25.0%; }
    .kpiMini:nth-child(5) { flex: 1 1 17.0%; }

    .kpiMini .kpiTitle { font-size: 11px; margin-bottom: 6px; color:#6b7280; }
    .kpiMini .kpiValue { font-size: 20px; font-weight: 800; line-height: 1.0; color:#111827; }
    .kpiMini .kpiNote  { font-size: 10px; margin-top: 6px; color:#6b7280; }
    
    .kpiMini .form-group{ margin:0; }
    .kpiMini .form-control{
      height: 34px;
      padding: 6px 10px;
      border-radius: 10px;
      border: 1px solid rgba(148,163,184,0.6);
      font-size: 12px;
    }
    
    .opsFilterInner{
      display:flex;
      align-items:center;
      gap:8px;
    }
    .opsFilterLabel{
      font-weight:700;
      color:#374151;
      font-size:12px;
    }
    .opsFilterInner .form-group{ margin:0; }
    .opsFilterInner .form-control{
      height: 34px;
      padding: 6px 10px;
      border-radius: 10px;
      border: 1px solid rgba(148,163,184,0.6);
      box-shadow: 0 4px 14px rgba(15,23,42,0.06);
      font-size: 12px;
    }
    
    /* Site filter KPI card: tighter vertical layout */
    .kpiFilterCard{
      padding-top: 10px;
      padding-bottom: 10px;
    }
    
    .kpiFilterCard .kpiTitle{
      margin-bottom: 6px;
    }
    
    .kpiFilterWrap{
      margin-top: 0;
    }
    
    /* Tighten Shiny select spacing inside KPI */
    .kpiFilterCard .form-group{ 
      margin: 0 !important;
    }
    .kpiFilterCard .form-control{
      height: 32px;
      padding: 4px 10px;
      border-radius: 10px;
      border: 1px solid rgba(148,163,184,0.6);
      font-size: 12px;
      line-height: 1.1;
    }
    
    /* ===== Panels: match Commercial format ===== */
    .panel {
      background:#fff;
      border-radius:16px;
      padding:16px 18px;
      box-shadow: 0 8px 30px rgba(15, 23, 42, 0.06);
      border: 1px solid rgba(148,163,184,0.35);
      margin-bottom: 14px;
    }
    .panelTitle { font-weight: 800; margin-bottom: 10px; }
    .tinyNote { color:#6b7280; font-size: 11px; margin-top: 8px; }
  "))),
  
  div(class="titleWrap",
      div(class="titleMain", "Operation — Performance and Compliance"),
      div(class="titleSub", "Utilization • Compliance • Revenue actualization"),
      uiOutput("ops_fatal_ui")
  ),
  
  
  div(class="kpiRow",
      div(class="kpiCard kpiMini",
          div(class="kpiTitle","MTD Utilization"),
          div(class="kpiValue", textOutput("ops_kpi_mtd_util")),
          div(class="kpiNote",  textOutput("ops_kpi_mtd_note"))
      ),
      
      div(class="kpiCard kpiMini",
          div(class="kpiTitle","Fleet Size (Latest)"),
          div(class="kpiValue", textOutput("ops_kpi_fleet_latest")),
          div(class="kpiNote",  textOutput("ops_kpi_fleet_note"))
      ),
      
      div(class="kpiCard kpiMini",
          div(class="kpiTitle","Ops Compliance Rate (MTD)"),
          div(class="kpiValue", textOutput("ops_kpi_compliance_rate")),
          div(class="kpiNote",  textOutput("ops_kpi_compliance_note"))
      ),
      
      div(class="kpiCard kpiMini",
          div(class="kpiTitle","Revenue Fulfillment (LOC Rev / Potential Rev (WO))"),
          div(class="kpiValue", textOutput("ops_kpi_revenue_fulfill")),
          div(class="kpiNote",  textOutput("ops_kpi_revenue_note"))
      ),
      
      div(class="kpiCard kpiMini kpiFilterCard",
          div(class="kpiTitle","Site Filter"),
          div(class="kpiFilterWrap",
              selectInput(
                inputId = "ops_site",
                label = NULL,
                choices = c("GLOBAL", "FKSCLG", "FKSSBY"),
                selected = "GLOBAL",
                width = "100%"
              )
          )
      )
  ),
  
  fluidRow(
    column(
      width = 6,
      div(class="panel",
          div(class="panelTitle", "Daily Utilization (MTD)"),
          plotlyOutput("ops_plot_daily", height = 200),
          div(class="tinyNote", "Continuous daily axis. Hover for details.")
      )
    ),
    column(
      width = 6,
      div(class="panel",
          div(class="panelTitle", "Compliance Trend (MTD cumulative)"),
          plotlyOutput("ops_plot_compliance_trend", height = 200),
          div(class="tinyNote", "Cumulative LOC with WO vs without WO.")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 6,
      div(class="panel",
          div(class="panelTitle", textOutput("ops_type_title")),
          plotlyOutput("ops_plot_type_latest", height = 200),
          div(class="tinyNote", "Latest date only • Sorted by utilization (lowest first).")
      )
    ),
    column(
      width = 6,
      div(class="panel",
          div(class="panelTitle", "Revenue Split — With WO vs Without WO (MTD)"),
          plotlyOutput("ops_plot_revenue_pie", height = 200),
          div(class="tinyNote", textOutput("ops_revenue_pie_note"))
      )
    )
  )
)

# ============================================================
# MAIN UI (tabs)
# ============================================================
ui_main <- navbarPage(
  title = "FTU Visibility Portal",
  tabPanel("Commercial", commercial_ui),
  tabPanel("Operation", ops_ui),
  
  tags$head(tags$style(HTML("
  /* --- GLOBAL: reduce gap below navbar (applies to all tabs) --- */
  .navbar { margin-bottom: 1px !important; }
  .container-fluid { padding-top: 1px !important; }
")))
  
)

# ============================================================
# SERVER (Commercial preserved + Ops added)
# ============================================================
server_main <- function(input, output, session) {
  
  # -----------------------------
  # Commercial (preserve behavior)
  # -----------------------------
  marts <- reactiveValues(detail = NULL, err = NULL)
  
  selected_day <- reactiveVal(NULL)           # Date
  selected_customer_key <- reactiveVal(NULL)  # UPPER string key
  
  load_marts <- function() {
    out <- tryCatch(read_mart_detail(), error = function(e) e)
    if (inherits(out, "error")) {
      marts$detail <- NULL
      marts$err <- out$message
    } else {
      marts$detail <- out
      marts$err <- NULL
    }
  }
  
  observeEvent(TRUE, { load_marts() }, once = TRUE)
  
  output$fatal_ui <- renderUI({
    if (is.null(marts$err)) return(NULL)
    div(
      style = "margin-top:10px; padding:12px 14px; border-radius:12px;
               background:#fff; border:1px solid rgba(239,68,68,0.35);",
      strong("Data load error"),
      div(style="color:#ef4444; margin-top:6px;", marts$err)
    )
  })
  
  # STATIC month (latest month in data)
  mtd_month <- reactive({
    req(marts$detail)
    max(marts$detail$wo_month, na.rm = TRUE)
  })
  
  # STATIC base (for KPI)
  detail_mtd_base <- reactive({
    req(marts$detail)
    marts$detail %>% filter(wo_month == mtd_month())
  })
  
  detail_mtd_by_office <- function(office_code) {
    detail_mtd_base() %>% filter(OrderOffice == office_code)
  }
  
  # Potential Revenue (WO) from Commercial mart (same basis as Commercial tab)
  potential_revenue_wo_idr <- reactive({
    req(marts$detail)
    
    # Prefer using ops month if it exists in commercial data; otherwise fall back to commercial month
    ops_m <- tryCatch(ops_mtd_month(), error = function(e) NA_character_)
    if (!is.na(ops_m) && ops_m %in% marts$detail$wo_month) {
      df <- marts$detail %>% filter(wo_month == ops_m)
    } else {
      df <- marts$detail %>% filter(wo_month == mtd_month())
    }
    
    sum(as.numeric(df$est_revenue), na.rm = TRUE)
  })
  
  potential_revenue_wo_idr_by_site <- reactive({
    req(marts$detail)
    
    site <- ops_site_sel()  # uses dropdown
    ops_m <- tryCatch(ops_mtd_month(), error = function(e) NA_character_)
    
    if (!is.na(ops_m) && ops_m %in% marts$detail$wo_month) {
      df <- marts$detail %>% filter(wo_month == ops_m)
    } else {
      df <- marts$detail %>% filter(wo_month == mtd_month())
    }
    
    if (site != "GLOBAL") {
      df <- df %>% filter(OrderOffice == site)
    }
    
    sum(as.numeric(df$est_revenue), na.rm = TRUE)
  })
  
  # DYNAMIC view (for charts)
  detail_view <- reactive({
    df <- detail_mtd_base()
    
    if (!is.null(selected_day())) {
      df <- df %>% filter(OrderDate == selected_day())
    }
    if (!is.null(selected_customer_key())) {
      df <- df %>% filter(customer_key == selected_customer_key())
    }
    df
  })
  
  # KPI (STATIC)
  output$kpi_mtd_rev_clg <- renderText({
    df <- detail_mtd_by_office("FKSCLG")
    fmt_num(sum(df$est_revenue, na.rm = TRUE), 0)
  })
  
  output$kpi_mtd_rev_sby <- renderText({
    df <- detail_mtd_by_office("FKSSBY")
    fmt_num(sum(df$est_revenue, na.rm = TRUE), 0)
  })
  
  output$kpi_mtd_wo_clg <- renderText({
    df <- detail_mtd_by_office("FKSCLG")
    n_distinct(df$FileNumber)
  })
  
  output$kpi_mtd_wo_sby <- renderText({
    df <- detail_mtd_by_office("FKSSBY")
    n_distinct(df$FileNumber)
  })
  
  output$kpi_cov <- renderText({
    df <- detail_mtd_base()
    wo_count <- n_distinct(paste0(df$OrderOffice, "|", df$FileNumber))
    wo_with_rate <- sum(!is.na(df$est_rate_per_kg))
    pct <- if (wo_count > 0) wo_with_rate / wo_count else NA_real_
    paste0(wo_with_rate, "/", wo_count, " (", fmt_num(pct * 100, 1), "%)")
  })
  
  output$kpi_mtd_note <- renderText({
    df <- detail_mtd_base()
    if (nrow(df) == 0) return("No data.")
    latest_day <- max(df$OrderDate, na.rm = TRUE)
    paste0("Latest WO date: ", format(latest_day, "%Y-%m-%d"))
  })
  
  output$kpi_mtd_month <- renderText({
    paste0("Month: ", mtd_month())
  })
  
  # Daily bar (plotly native)
  daily_df_office <- function(office_code) {
    reactive({
      df <- detail_view() %>% filter(OrderOffice == office_code)
      
      df %>%
        group_by(OrderDate) %>%
        summarise(
          revenue_idr = sum(est_revenue, na.rm = TRUE),
          revenue_mn  = revenue_idr / 1e6,
          wo_count    = n_distinct(FileNumber),
          .groups = "drop"
        ) %>%
        arrange(OrderDate) %>%
        mutate(
          day_key = format(OrderDate, "%Y-%m-%d"),
          x_lab   = format(OrderDate, "%b %d"),
          hover   = paste0(
            "Office: ", office_code,
            "<br>Date: ", day_key,
            "<br>Revenue (IDR Mn): ", sprintf("%.1f", revenue_mn),
            "<br>WO Count: ", wo_count
          )
        )
    })
  }
  
  daily_df_clg <- daily_df_office("FKSCLG")
  daily_df_sby <- daily_df_office("FKSSBY")
  
  output$plot_daily_clg <- renderPlotly({
    daily <- daily_df_clg()
    req(nrow(daily) > 0)
    
    p <- plot_ly(
      data = daily,
      x = ~x_lab,
      y = ~revenue_mn,
      type = "bar",
      text = ~hover,
      hoverinfo = "text",
      key = ~day_key,
      source = "daily_clg"
    ) %>%
      layout(
        yaxis = list(
          title = "IDR Mn",
          separatethousands = TRUE,
          titlefont = list(size = 10),
          tickfont  = list(size = 9),
          automargin = FALSE
        ),
        xaxis = list(
          title = "",
          tickangle = -30,
          tickfont = list(size = 7),
          automargin = FALSE
        ),
        margin = list(l = 55, r = 10, b = 30, t = 6)
      )
    
    p <- event_register(p, "plotly_click")
    p <- event_register(p, "plotly_doubleclick")
    p
  })
  
  output$plot_daily_sby <- renderPlotly({
    daily <- daily_df_sby()
    req(nrow(daily) > 0)
    
    p <- plot_ly(
      data = daily,
      x = ~x_lab,
      y = ~revenue_mn,
      type = "bar",
      marker = list(color = "#ff7f0e"),
      text = ~hover,
      hoverinfo = "text",
      key = ~day_key,
      source = "daily_sby"
    ) %>%
      layout(
        yaxis = list(
          title = "IDR Mn",
          separatethousands = TRUE,
          titlefont = list(size = 10),
          tickfont  = list(size = 9),
          automargin = FALSE
        ),
        xaxis = list(
          title = "",
          tickangle = -30,
          tickfont = list(size = 7),
          automargin = FALSE
        ),
        margin = list(l = 55, r = 10, b = 30, t = 6)
      )
    
    p <- event_register(p, "plotly_click")
    p <- event_register(p, "plotly_doubleclick")
    p
  })
  
  observeEvent(event_data("plotly_click", source = "daily_clg"), {
    ed <- event_data("plotly_click", source = "daily_clg")
    if (is.null(ed) || is.null(ed$key)) return()
    
    clicked_date <- as.Date(as.character(ed$key))
    if (!is.null(selected_day()) && selected_day() == clicked_date) {
      selected_day(NULL)
    } else {
      selected_day(clicked_date)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "daily_sby"), {
    ed <- event_data("plotly_click", source = "daily_sby")
    if (is.null(ed) || is.null(ed$key)) return()
    
    clicked_date <- as.Date(as.character(ed$key))
    if (!is.null(selected_day()) && selected_day() == clicked_date) {
      selected_day(NULL)
    } else {
      selected_day(clicked_date)
    }
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "daily_clg"), {
    selected_day(NULL)
  })
  observeEvent(event_data("plotly_doubleclick", source = "daily_sby"), {
    selected_day(NULL)
  })
  
  # Treemap (plotly native)
  treemap_df <- reactive({
    df <- detail_view()
    
    cust <- df %>%
      group_by(Customer, customer_key) %>%
      summarise(
        revenue_idr = sum(est_revenue, na.rm = TRUE),
        revenue_mn  = revenue_idr / 1e6,
        .groups = "drop"
      ) %>%
      filter(revenue_idr > 0) %>%
      arrange(desc(revenue_idr))
    
    total_rev <- sum(cust$revenue_idr, na.rm = TRUE)
    
    cust %>%
      mutate(
        pct = ifelse(total_rev > 0, revenue_idr / total_rev, 0),
        label = paste0(Customer, "<br>", fmt_num(pct * 100, 1), "%"),
        hover = paste0(
          Customer,
          "<br>Share: ", fmt_num(pct * 100, 1), "%",
          "<br>Revenue (IDR Mn): ", fmt_num(revenue_mn, 0)
        )
      )
  })
  
  output$plot_treemap <- renderPlotly({
    cust <- treemap_df()
    req(nrow(cust) > 0)
    
    colors <- soft_palette(nrow(cust))
    
    p <- plot_ly(
      type = "treemap",
      labels  = cust$Customer,
      parents = rep("", nrow(cust)),
      values  = cust$revenue_idr,
      
      # IMPORTANT: let plotly compute percent of total (root)
      textinfo = "label+percent root",
      branchvalues = "total",
      
      # Use native percentRoot in hover (no manual pct string)
      hovertemplate = paste(
        "%{label}",
        "<br>Share: %{percentRoot:.1%}",
        "<br>Revenue (IDR Mn): %{value:,.0f}<extra></extra>"
      ),
      
      marker = list(colors = colors, line = list(width = 1, color = "white")),
      customdata = cust$customer_key,
      source = "treemap"
    ) %>%
      layout(margin = list(l = 10, r = 10, b = 10, t = 10))
    
    p <- event_register(p, "plotly_click")
    p <- event_register(p, "plotly_doubleclick")
    p
  })
  
  
  observeEvent(event_data("plotly_click", source = "treemap"), {
    ed <- event_data("plotly_click", source = "treemap")
    if (is.null(ed) || is.null(ed$customdata)) return()
    
    picked <- as.character(ed$customdata)
    if (!is.null(selected_customer_key()) && selected_customer_key() == picked) {
      selected_customer_key(NULL)
    } else {
      selected_customer_key(picked)
    }
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "treemap"), {
    selected_customer_key(NULL)
  })
  
  
  # -----------------------------
  # Ops (load marts)
  # -----------------------------
  ops <- reactiveValues(
    daily = NULL,
    by_type = NULL,
    kpi = NULL,
    comp_trend = NULL,
    err = NULL
  )
  
  observeEvent(TRUE, {
    out_daily <- tryCatch(ops_read_csv(path_ops_daily), error = function(e) e)
    out_type  <- tryCatch(ops_read_csv(path_ops_by_type), error = function(e) e)
    out_kpi   <- tryCatch(ops_read_csv(path_kpi_loc_compliance_mtd), error = function(e) e)
    out_trend <- tryCatch(ops_read_csv(path_loc_compliance_trend_mtd_cum), error = function(e) e)
    
    errs <- c()
    for (obj in list(out_daily, out_type, out_kpi, out_trend)) {
      if (inherits(obj, "error")) errs <- c(errs, obj$message)
    }
    if (length(errs) > 0) {
      ops$err <- paste(errs, collapse = " | ")
      return()
    }
    
    out_daily$Date <- as.Date(out_daily$Date)
    out_type$Date  <- as.Date(out_type$Date)
    
    # Support both formats:
    # - old mart: first_seen
    # - new mart: OrderDate
    if ("first_seen" %in% names(out_trend)) {
      out_trend$first_seen <- as.Date(out_trend$first_seen)
    } else if ("OrderDate" %in% names(out_trend)) {
      out_trend$first_seen <- as.Date(out_trend$OrderDate)
    } else {
      stop("Trend mart is missing date column: expected 'first_seen' or 'OrderDate'")
    }
    
    ops$daily <- out_daily
    ops$by_type <- out_type
    ops$kpi <- out_kpi
    ops$comp_trend <- out_trend
    ops$err <- NULL
  }, once = TRUE)
  
  output$ops_fatal_ui <- renderUI({
    if (is.null(ops$err)) return(NULL)
    div(
      style = "margin-top:10px; margin-bottom:10px; padding:12px 14px; border-radius:12px;
             background:#fff; border:1px solid rgba(239,68,68,0.35);",
      strong("Ops data load error"),
      div(style="color:#ef4444; margin-top:6px;", ops$err)
    )
  })
  
  # -----------------------------
  # Ops KPI helpers
  # -----------------------------
  ops_mtd_month <- reactive({
    req(ops$daily)
    format(max(ops$daily$Date, na.rm = TRUE), "%Y-%m")
  })
  
  ops_mtd_daily <- reactive({
    req(ops$daily)
    m <- ops_mtd_month()
    ops$daily %>% filter(format(Date, "%Y-%m") == m) %>% arrange(Date)
  })
  
  ops_latest_date <- reactive({
    req(ops$daily)
    max(ops$daily$Date, na.rm = TRUE)
  })
  
  output$ops_kpi_mtd_note <- renderText({
    paste0("Month: ", ops_mtd_month())
  })
  
  output$ops_kpi_mtd_util <- renderText({
    df <- ops_mtd_daily()
    total <- sum(as.numeric(df$truck_total), na.rm = TRUE)
    prod  <- sum(as.numeric(df$truck_productive), na.rm = TRUE)
    util <- ifelse(total > 0, prod / total, NA_real_)
    paste0(fmt_num(util * 100, 1), "%")
  })
  
  output$ops_kpi_fleet_latest <- renderText({
    df <- ops$daily
    latest <- ops_latest_date()
    fleet <- df %>% filter(Date == latest) %>%
      summarise(v = max(as.numeric(truck_total), na.rm = TRUE)) %>% pull(v)
    fmt_num(fleet, 0)
  })
  
  output$ops_kpi_fleet_note <- renderText({
    paste0("As of ", as.character(ops_latest_date()))
  })
  
  ops_site_sel <- reactive({
    req(input$ops_site)
    input$ops_site
  })
  
  ops_kpi_sel <- reactive({
    req(ops$kpi)
    if ("order_office" %in% names(ops$kpi)) {
      ops$kpi %>% dplyr::filter(order_office == ops_site_sel())
    } else {
      ops$kpi %>% dplyr::slice(1)
    }
  })
  
  ops_comp_trend_sel <- reactive({
    req(ops$comp_trend)
    
    df <- ops$comp_trend
    
    if ("order_office" %in% names(df)) {
      if (ops_site_sel() == "GLOBAL") {
        return(df)  # no filter → show all sites
      } else {
        return(df %>% dplyr::filter(order_office == ops_site_sel()))
      }
    }
    
    df
  })
  
  output$ops_kpi_compliance_rate <- renderText({
    k <- ops_kpi_sel()
    req(nrow(k) == 1)
    rate <- as.numeric(k$ops_compliance_rate[1])
    paste0(fmt_num(rate * 100, 1), "%")
  })
  
  output$ops_kpi_compliance_note <- renderText({
    k <- ops_kpi_sel()
    req(nrow(k) == 1)
    s <- as.character(k$period_start[1])
    e <- as.character(k$period_end[1])
    paste0("Period: ", s, " \u2192 ", e)
  })
  
  output$ops_kpi_revenue_fulfill <- renderText({
    k <- ops_kpi_sel()
    req(nrow(k) == 1)
    
    pot <- potential_revenue_wo_idr_by_site()
    
    total <- as.numeric(k$loc_revenue_total_idr[1])
    no_wo <- as.numeric(k$loc_revenue_without_wo_idr[1])
    realized_with_wo <- total - no_wo
    
    rate <- ifelse(pot > 0, realized_with_wo / pot, NA_real_)
    paste0(fmt_num(rate * 100, 1), "%")
  })
  
  output$ops_kpi_revenue_note <- renderText({
    k <- ops_kpi_sel()
    req(nrow(k) == 1)
    
    pot <- potential_revenue_wo_idr_by_site()
    
    total <- as.numeric(k$loc_revenue_total_idr[1])
    no_wo <- as.numeric(k$loc_revenue_without_wo_idr[1])
    realized_with_wo <- total - no_wo
    
    paste0(
      "Realized (WO): ", fmt_bn(realized_with_wo), " IDR • ",
      "Potential (WO): ", fmt_bn(pot), " IDR"
    )
  })
  
  # -----------------------------
  # Ops plot: Daily Utilization (MTD) — stacked + continuous daily axis
  # -----------------------------
  output$ops_plot_daily <- renderPlotly({
    df <- ops_mtd_daily()
    req(nrow(df) > 0)
    
    df <- df %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(Date) %>%
      summarise(
        truck_productive = sum(as.numeric(truck_productive), na.rm = TRUE),
        truck_idle       = sum(as.numeric(truck_idle), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Date)
    
    full_dates <- tibble(Date = seq(min(df$Date), max(df$Date), by = "day"))
    
    df_full <- full_dates %>%
      left_join(df, by = "Date") %>%
      mutate(
        truck_productive = coalesce(truck_productive, 0),
        truck_idle       = coalesce(truck_idle, 0),
        DateX = as.POSIXct(Date)
      )
    
    long <- df_full %>%
      pivot_longer(cols = c(truck_productive, truck_idle),
                   names_to = "status_raw", values_to = "count") %>%
      mutate(
        status = ifelse(status_raw == "truck_productive", "Productive", "Idle"),
        hover  = paste0(
          "Date: ", format(Date, "%Y-%m-%d"),
          "<br>Status: ", status,
          "<br>Trucks: ", count
        )
      )
    
    plot_ly(
      data = long,
      x = ~DateX,
      y = ~count,
      color = ~status,
      colors = c("Productive" = "#111827", "Idle" = "#9ca3af"),
      type = "bar",
      hovertext = ~hover,
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.02, y = 0.5,
          xanchor = "left", yanchor = "middle",
          font = list(size = 14),
          itemwidth = 40,
          tracegroupgap = 10
        ),
        xaxis = list(
          title = "Date",
          type = "date",
          tickformat = "%b %d",
          dtick = 86400000,
          tickfont = list(size = 10),
          titlefont = list(size = 11)
        ),
        yaxis = list(
          title = "Trucks",
          tickfont = list(size = 10),
          titlefont = list(size = 11)
        ),
        margin = list(l = 50, r = 120, b = 45, t = 10)
      )
  })
  
  # -----------------------------
  # Ops plot: Truck Type Breakdown — latest date only
  # -----------------------------
  output$ops_type_title <- renderText({
    req(ops$by_type)
    d <- max(as.Date(ops$by_type$Date), na.rm = TRUE)
    paste0("Truck Type Breakdown — Latest Date (", as.character(d), ")")
  })
  
  output$ops_plot_type_latest <- renderPlotly({
    req(ops$by_type)
    
    d <- max(as.Date(ops$by_type$Date), na.rm = TRUE)
    
    df <- ops$by_type %>%
      mutate(Date = as.Date(Date)) %>%
      filter(Date == d) %>%
      mutate(
        TruckType = ifelse(is.na(TruckType) | TruckType == "", "UNKNOWN", TruckType),
        truck_total = as.numeric(truck_total),
        truck_productive = as.numeric(truck_productive),
        truck_idle = as.numeric(truck_idle),
        util_rate = dplyr::coalesce(
          as.numeric(utilization_rate),
          ifelse(truck_total > 0, truck_productive / truck_total, 0)
        )
      ) %>%
      mutate(
        truck_total = dplyr::coalesce(truck_total, 0),
        truck_productive = dplyr::coalesce(truck_productive, 0),
        truck_idle = dplyr::coalesce(truck_idle, 0),
        util_rate = dplyr::coalesce(util_rate, 0),
        TruckTypeLabel = paste0(TruckType, " (", fmt_num(util_rate * 100, 1), "%)")
      ) %>%
      arrange(util_rate, desc(truck_total))
    
    if (nrow(df) == 0) {
      return(plotly::plot_ly() %>% plotly::layout(
        title = list(text = paste0("No truck-type data for ", d)),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ))
    }
    
    # lock order (lowest utilization first)
    df <- df %>%
      mutate(TruckTypeLabel = factor(TruckTypeLabel, levels = df$TruckTypeLabel))
    
    long <- df %>%
      tidyr::pivot_longer(
        cols = c(truck_productive, truck_idle),
        names_to = "status_raw",
        values_to = "count"
      ) %>%
      mutate(
        status = ifelse(status_raw == "truck_productive", "Productive", "Idle"),
        hover = paste0(
          "Date: ", as.character(d),
          "<br>TruckType: ", TruckType,
          "<br>Status: ", status,
          "<br>Trucks: ", count,
          "<br>Total: ", truck_total,
          "<br>Utilization: ", fmt_num(util_rate * 100, 1), "%"
        )
      )
    
    plotly::plot_ly(
      data = long,
      y = ~TruckTypeLabel,
      x = ~count,
      color = ~status,
      colors = c("Productive" = "#111827", "Idle" = "#9ca3af"),
      type = "bar",
      orientation = "h",
      hovertext = ~hover,
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        barmode = "stack",
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.02, y = 0.5,
          xanchor = "left", yanchor = "middle",
          font = list(size = 14),
          itemwidth = 40,
          tracegroupgap = 10
        ),
        xaxis = list(title = "Trucks", tickfont = list(size = 10), titlefont = list(size = 11)),
        yaxis = list(title = "", tickfont = list(size = 11)),
        margin = list(l = 120, r = 120, b = 40, t = 10)
      )
  })
  
  # -----------------------------
  # Ops plot: Compliance Trend (MTD cumulative) — LOC with WO vs without WO
  # -----------------------------
  output$ops_plot_compliance_trend <- renderPlotly({
    df <- ops_comp_trend_sel() %>%
      mutate(first_seen = as.Date(first_seen)) %>%
      arrange(first_seen)
    
    req(nrow(df) > 0)
    
    plot_ly(
      df,
      x = ~first_seen,
      y = ~ops_compliance_rate_cum,
      type = "scatter",
      mode = "lines+markers",
      color = ~order_office,
      hovertemplate = paste(
        "%{x}<br>",
        "Compliance: %{y:.1%}<br>",
        "<extra>%{fullData.name}</extra>"
      )
    ) %>%
      layout(
        yaxis = list(title = "Compliance Rate", tickformat = ".0%"),
        xaxis = list(title = ""),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
        margin = list(l = 55, r = 20, b = 70, t = 10)
      )
  })
  
  # -----------------------------
  # Ops plot: Revenue Split pie (MTD) — complied (WO) vs not complied (No WO)
  # -----------------------------
  output$ops_plot_revenue_pie <- renderPlotly({
    
    k <- ops_kpi_sel()
    req(nrow(k) == 1)
    
    total <- as.numeric(k$loc_revenue_total_idr[1])
    no_wo <- as.numeric(k$loc_revenue_without_wo_idr[1])
    with_wo <- total - no_wo
    
    df <- tibble(
      group = c("Not complied (No WO)", "Complied (WO)"),
      value = c(no_wo, with_wo),
      hover = c(
        paste0("Not complied (No WO)<br>", fmt_bn(no_wo), " IDR"),
        paste0("Complied (WO)<br>", fmt_bn(with_wo), " IDR")
      ),
      label = c(
        paste0("Not complied (No WO)<br>", fmt_bn(no_wo)),
        paste0("Complied (WO)<br>", fmt_bn(with_wo))
      )
    )
    
    plot_ly(
      data = df,
      labels = ~group,
      values = ~value,
      type = "pie",
      text = ~label,
      textinfo = "text",
      hovertext = ~hover,
      hoverinfo = "text"
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.05, y = 0.5,
          xanchor = "left", yanchor = "middle",
          font = list(size = 14),
          tracegroupgap = 10
        ),
        margin = list(l = 10, r = 120, b = 10, t = 10)
      )
  })
  
  output$ops_revenue_pie_note <- renderText({
    k <- ops_kpi_sel()
    req(nrow(k) == 1)
    
    total <- as.numeric(k$loc_revenue_total_idr[1])
    no_wo <- as.numeric(k$loc_revenue_without_wo_idr[1])
    with_wo <- total - no_wo
    
    paste0(
      "Total: ", fmt_bn(total), " IDR • With WO: ", fmt_bn(with_wo),
      " • Without WO: ", fmt_bn(no_wo)
    )
  })
}

shinyApp(ui_main, server_main)

