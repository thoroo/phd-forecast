library(shiny)
library(ggplot2)
library(lubridate)
library(plotly)
library(jsonlite)

# ============================================================
# Fixed variables
# ============================================================

required_work <- 48

# ============================================================
# Helpers
# ============================================================

available_languages <- function() {
  
  files <- list.files("lang",
                      pattern = "\\.json$",
                      full.names = TRUE)
  
  codes <- sub("\\.json$", "", basename(files))
  
  labels <- sapply(files, function(f) {
    
    j <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
    
    if (!is.null(j) && !is.null(j$language)) {
      j$language
    } else {
      sub("\\.json$", "", basename(f))
    }
  })
  
  stats::setNames(codes, labels)
}

load_language <- function(code) {
  path <- file.path("lang", paste0(code, ".json"))
  if (!file.exists(path))
    stop("Language file not found: ", path)
  fromJSON(path, simplifyVector = TRUE)
}

add_months_frac <- function(date, months) {
  whole <- floor(months)
  frac  <- months - whole
  d <- date %m+% months(whole)
  d + days(round(frac * 30))
}

next_checkpoint <- function(date) {
  y <- year(date)
  june30 <- as.Date(paste0(y, "-06-30"))
  dec31  <- as.Date(paste0(y, "-12-31"))
  if (date <= june30) return(june30)
  if (date <= dec31)  return(dec31)
  as.Date(paste0(y + 1, "-06-30"))
}

milestone_date <- function(start, today, end,
                           work_today, target) {
  
  if (target <= work_today) {
    rate <- work_today / as.numeric(today - start)
    return(start + days(round(target / rate)))
  } else {
    rate <- (required_work - work_today) /
      as.numeric(end - today)
    return(today +
             days(round((target - work_today) / rate)))
  }
}

months_between <- function(from, to) {
  as.numeric(interval(from, to) / months(1))
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  # ---------- Header row ----------
  fluidRow(
    
    column(
      10,
      uiOutput("title_ui")
    ),
    
    column(
      2,
      div(
        style = "margin-top: 20px;",
        selectInput(
          "lang_select",
          label = NULL,
          choices = available_languages(),
          selected = "en",
          width = "100%"
        )
      )
    )
  ),
  
  # ---------- Main content ----------
  fluidRow(
    
    column(
      4,
      uiOutput("controls_ui")
    ),
    
    column(
      8,
      
      uiOutput("forecast_title"),
      verbatimTextOutput("forecast_text"),
      
      fluidRow(
        
        column(
          8,
          plotlyOutput("timeline_plot", height = 520)
        ),
        
        column(
          4,
          
          uiOutput("milestones_title"),
          verbatimTextOutput("milestone_text"),
          
          hr(),
          
          uiOutput("stats_title"),
          verbatimTextOutput("stats_text")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----- Language state -----
  
  lang_code <- reactive({
    req(input$lang_select)
    input$lang_select
  })
  
  L <- reactive({
    load_language(lang_code())
  })
  
  output$title_ui <- renderUI({
    titlePanel(L()$title)
  })
  
  # ============================================================
  # Controls
  # ============================================================
  
  output$controls_ui <- renderUI({
    
    Ls <- L()
    
    tagList(
      
      h4(Ls$person),
      
      numericInput("birth_year",
                   Ls$birth_year,
                   value = 1990,
                   min = 1940,
                   max = year(Sys.Date())),
      
      h4(Ls$status),
      
      dateInput("start",
                Ls$start_date,
                value = "2024-08-15"),
      
      dateInput("today",
                Ls$today_date,
                value = Sys.Date()),
      
      numericInput("remaining",
                   Ls$remaining_activity,
                   value = 33.4,
                   min = 0,
                   max = 48),
      
      h4(Ls$future),
      
      sliderInput("future_prolong",
                  Ls$prolong,
                  min = 0,
                  max = 50,
                  value = 10),
      
      radioButtons("time_mode",
                   Ls$basis,
                   choices = c(Ls$sem),
                   selected = Ls$sem),
      
      checkboxInput("compare",
                    isTRUE(input$compare),
                    FALSE),
      
      conditionalPanel(
        "input.compare == true",
        sliderInput(
          "future_prolong_B",
          Ls$prolong,   # reuse label
          min = 0,
          max = 50,
          value = 20
        )
      ),
      
      hr(),
      
      strong(Ls$requirement),
      p(Ls$req_text)
    )
  })
  
  output$forecast_title   <- renderUI({ h3(L()$forecast) })
  output$milestones_title <- renderUI({ h4(L()$milestones) })
  output$stats_title      <- renderUI({ h4(L()$stats) })
  
  # ============================================================
  # Calculation
  # ============================================================
  
  calc <- function(prolong) {
    
    req(input$start,
        input$today,
        input$remaining,
        input$future_prolong)
    
    start <- as.Date(input$start)
    today_raw <- as.Date(input$today)
    
    req(!is.na(start), !is.na(today_raw))
    
    # Semester checkpoint (only mode you use)
    today <- next_checkpoint(today_raw)
    
    remaining_work <- input$remaining
    activity <- max(1 - prolong / 100, 0.01)
    
    remaining_cal <- remaining_work / activity
    end_date <- add_months_frac(today, remaining_cal)
    
    elapsed_cal <- as.numeric(interval(start, today) / months(1))
    done_work <- required_work - remaining_work
    
    list(
      start_date = start,
      today_date = today,
      end_date = end_date,
      done = done_work,
      elapsed = elapsed_cal,
      total = elapsed_cal + remaining_cal
    )
  }
  
  # ============================================================
  # Plot
  # ============================================================
  
  output$timeline_plot <- renderPlotly({
    
    Ls <- L()
    
    A <- calc(input$future_prolong)
    
    start <- A$start_date
    today <- A$today_date
    endA  <- A$end_date
    done  <- A$done
    
    ref_end <- start %m+% months(required_work)
    
    pathA <- data.frame(date = c(start, today, endA),
                        work = c(0, done, required_work))
    
    pathRef <- data.frame(date = c(start, ref_end),
                          work = c(0, required_work))
    
    p <- ggplot() +
      geom_line(data = pathRef,
                aes(date, work),
                linetype = "dashed",
                linewidth = 1.2,
                color = "grey40") +
      geom_line(data = pathA,
                aes(date, work),
                linewidth = 1.6,
                color = "steelblue")
    
    # Labels localized
    lab_start <- ifelse(lang_code() == "sv", "Start", "Start")
    lab_today <- ifelse(lang_code() == "sv", "Idag", "Today")
    lab_proj  <- ifelse(lang_code() == "sv", "Beräknat slut", "Projected end")
    
    m50 <- milestone_date(start, today, endA, done, 0.5 * required_work)
    m80 <- milestone_date(start, today, endA, done, 0.8 * required_work)
    
    marks <- data.frame(
      label = c(lab_start, lab_today, "50%", "80%", lab_proj),
      date  = c(start, today, m50, m80, endA),
      work  = c(0, done,
                0.5 * required_work,
                0.8 * required_work,
                required_work)
    )
    
    marks$tooltip <- paste0(
      Ls$today, ": ", format(marks$date, "%Y-%m-%d"),
      "<br>", Ls$activity_so_far, ": ",
      format(round(marks$work, 1),
             decimal.mark = ifelse(lang_code() == "sv", ",", "."))
    )
    
    marks$label_y <- marks$work - 2
    
    max_end <- max(marks$date)
    xmax_ext <- max_end %m+% months(12)
    
    p <- p +
      geom_point(data = marks,
                 aes(date, work, text = tooltip),
                 size = 3) +
      geom_text(data = marks,
                aes(date, label_y, label = label),
                size = 4,
                inherit.aes = FALSE) +
      geom_vline(xintercept = as.numeric(marks$date),
                 linetype = "dotted",
                 alpha = 0.4) +
      scale_x_date(limits = c(min(marks$date), xmax_ext)) +
      coord_cartesian(clip = "off") +
      theme_minimal()
    
    plt <- suppressWarnings(ggplotly(p, tooltip = "text"))
    
    for (i in seq_along(plt$x$data)) {
      md <- plt$x$data[[i]]$mode
      if (!is.null(md) && md == "text") {
        plt$x$data[[i]]$hoverinfo <- "skip"
      }
    }
    
    plt
  })
  
  # ============================================================
  # Milestones text
  # ============================================================
  
  output$milestone_text <- renderText({
    
    Ls <- L()
    
    A <- calc(input$future_prolong)
    
    today <- A$today_date
    endA  <- A$end_date
    done  <- A$done
    
    m50_A <- milestone_date(A$start_date, today, endA, done,
                            0.5 * required_work)
    m80_A <- milestone_date(A$start_date, today, endA, done,
                            0.8 * required_work)
    
    to50_A  <- months_between(today, m50_A)
    to80_A  <- months_between(today, m80_A)
    toEnd_A <- months_between(today, endA)
    
    paste0(
      Ls$scenarioA, " (", input$future_prolong, "% ", Ls$prolongation, ")\n",
      "50%: ", m50_A, "  —  ", round(to50_A, 1), " ", Ls$months_remaining, "\n",
      "80%: ", m80_A, "  —  ", round(to80_A, 1), " ", Ls$months_remaining, "\n",
      Ls$end, ": ", endA, "  —  ", round(toEnd_A, 1), " ", Ls$months_remaining, "\n",
      Ls$total_duration, ": ", round(A$total, 2), " ", Ls$months
    )
  })
  
  # ============================================================
  # Statistics
  # ============================================================
  
  output$stats_text <- renderText({
    
    Ls <- L()
    A <- calc(input$future_prolong)
    
    done <- A$done
    elapsed <- A$elapsed
    
    efficiency <- if (elapsed > 0) done / elapsed else NA_real_
    efficiency <- max(min(efficiency, 1), 0)
    
    prolong_pct <- (1 - efficiency) * 100
    
    paste0(
      Ls$activity_so_far, ": ", round(done, 1), " ", Ls$months, "\n",
      Ls$prolongation, " (%): ", round(prolong_pct, 1), Ls$percent, "\n",
      Ls$efficiency, ": ", round(100 * efficiency, 1), Ls$percent
    )
  })
  
}

shinyApp(ui, server)
