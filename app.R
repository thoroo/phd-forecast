
library(shiny)
library(ggplot2)
library(lubridate)
library(plotly)
library(jsonlite)

# ============================================================
# Fixed variables
# ============================================================
required_work <- 48  # total required work in "activity months"

# Default assumption for current status in average tab
default_past_months  <- 6
default_past_prolong <- 10
default_remaining_activity <- required_work - default_past_months * (1 - default_past_prolong / 100)
# = 42.6

# ============================================================
# Helpers
# ============================================================
safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) return(as.Date(NA))
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

available_languages <- function() {
  files <- list.files("lang", pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0) return(c(English = "en"))
  
  codes <- sub("\\.json$", "", basename(files))
  
  labels <- sapply(files, function(f) {
    j <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
    if (!is.null(j) && !is.null(j$language)) j$language else sub("\\.json$", "", basename(f))
  })
  
  stats::setNames(codes, labels)
}

load_language <- function(code) {
  path <- file.path("lang", paste0(code, ".json"))
  if (!file.exists(path)) stop("Language file not found: ", path)
  fromJSON(path, simplifyVector = TRUE)
}

tr <- function(Ls, key, fallback = key) {
  val <- Ls[[key]]
  if (!is.null(val) && length(val) > 0 && !is.na(val) && nzchar(as.character(val))) {
    as.character(val)
  } else {
    fallback
  }
}

add_months_frac <- function(date, months) {
  date <- safe_date(date)
  if (is.na(date) || is.na(months)) return(as.Date(NA))
  
  whole <- floor(months)
  frac  <- months - whole
  d <- date %m+% months(as.integer(whole))
  d + days(as.integer(round(frac * 30)))
}

next_checkpoint <- function(date) {
  date <- safe_date(date)
  if (is.na(date)) return(as.Date(NA))
  y <- year(date)
  june30 <- as.Date(paste0(y, "-06-30"))
  dec31  <- as.Date(paste0(y, "-12-31"))
  
  if (date <= june30) return(june30)
  if (date <= dec31)  return(dec31)
  as.Date(paste0(y + 1, "-06-30"))
}

next_checkpoint_after <- function(date) {
  date <- safe_date(date)
  if (is.na(date)) return(as.Date(NA))
  y <- year(date)
  june30 <- as.Date(paste0(y, "-06-30"))
  dec31  <- as.Date(paste0(y, "-12-31"))
  
  if (date < june30) return(june30)
  if (date < dec31)  return(dec31)
  as.Date(paste0(y + 1, "-06-30"))
}

months_between <- function(from, to) {
  from <- safe_date(from)
  to   <- safe_date(to)
  if (is.na(from) || is.na(to)) return(NA_real_)
  time_length(interval(from, to), "month")
}

work_to_pct <- function(work) {
  100 * work / required_work
}

format_duration_ymd <- function(Ls, from, to) {
  from <- safe_date(from)
  to   <- safe_date(to)
  if (is.na(from) || is.na(to) || to <= from) return("0 months")
  
  total_days <- as.numeric(to - from)
  years  <- floor(total_days / 365.25)
  rem    <- total_days - years * 365.25
  months <- floor(rem / 30.44)
  days   <- round(rem - months * 30.44)
  
  if (years > 0) {
    paste0(
      years, " ", tr(Ls, "years", "years"), ", ",
      months, " ", tr(Ls, "months", "months"), ", ",
      days, " ", tr(Ls, "days", "days")
    )
  } else {
    paste0(
      months, " ", tr(Ls, "months", "months"), ", ",
      days, " ", tr(Ls, "days", "days")
    )
  }
}

semester_label <- function(date, Ls) {
  date <- safe_date(date)
  if (is.na(date)) return(NA_character_)
  y <- year(date)
  if (month(date) <= 6) paste0(tr(Ls, "spring", "Spring"), " ", y) else paste0(tr(Ls, "fall", "Fall"), " ", y)
}

milestone_date_model <- function(start, today, completed_today, elapsed_months,
                                 prolong_pct, target_work) {
  start <- safe_date(start)
  today <- safe_date(today)
  
  past_rate <- if (!is.na(elapsed_months) && elapsed_months > 0) completed_today / elapsed_months else 0
  past_rate <- min(max(past_rate, 0), 1)
  future_rate <- max(1 - prolong_pct / 100, 0.01)
  
  if (target_work <= 0) return(start)
  
  if (target_work <= completed_today) {
    if (past_rate <= 0) return(start)
    months_needed <- target_work / past_rate
    add_months_frac(start, months_needed)
  } else {
    months_needed <- (target_work - completed_today) / future_rate
    add_months_frac(today, months_needed)
  }
}

adjusted_hours_per_year <- function(birth_year, today = Sys.Date()) {
  1732
}

calc_average <- function(start, today, remaining_at_checkpoint, prolong_pct) {
  start <- safe_date(start)
  today <- safe_date(today)
  
  checkpoint <- next_checkpoint(today)
  elapsed <- months_between(start, today)
  to_checkpoint <- months_between(today, checkpoint)
  
  actual_left_raw <- max(remaining_at_checkpoint + to_checkpoint, 0)
  completed_raw <- max(required_work - actual_left_raw, 0)
  
  adjusted <- FALSE
  completed <- completed_raw
  actual_left <- actual_left_raw
  
  if (!is.na(elapsed) && elapsed > 0 && completed_raw > elapsed) {
    adjusted <- TRUE
    completed <- elapsed
    actual_left <- max(required_work - completed, 0)
  }
  
  efficiency <- if (!is.na(elapsed) && elapsed > 0) completed / elapsed else NA_real_
  
  future_rate <- max(1 - prolong_pct / 100, 0.01)
  extended_left <- actual_left / future_rate
  end_date <- add_months_frac(today, extended_left)
  
  list(
    start = start,
    today = today,
    checkpoint = checkpoint,
    elapsed = elapsed,
    to_checkpoint = to_checkpoint,
    completed = completed,
    actual_left = actual_left,
    end_date = end_date,
    efficiency = efficiency,
    adjusted = adjusted
  )
}

simulate_semesters_from_start <- function(start, prolong_vec, min_semesters = 8, max_extra_semesters = 20) {
  start <- safe_date(start)
  prolong_vec <- as.numeric(prolong_vec)
  prolong_vec[is.na(prolong_vec)] <- 0
  
  if (length(prolong_vec) == 0) prolong_vec <- 0
  
  if (length(prolong_vec) < min_semesters) {
    prolong_vec <- c(prolong_vec, rep(tail(prolong_vec, 1), min_semesters - length(prolong_vec)))
  }
  
  cp_start <- start
  dates <- c(start)
  work  <- c(0)
  current_work <- 0
  sem_index <- 1
  total_limit <- length(prolong_vec) + max_extra_semesters
  
  while (sem_index <= total_limit) {
    p_val <- if (sem_index <= length(prolong_vec)) prolong_vec[sem_index] else tail(prolong_vec, 1)
    
    cp_end <- next_checkpoint_after(cp_start)
    sem_days <- as.numeric(cp_end - cp_start)
    
    if (is.na(sem_days) || sem_days <= 0) {
      sem_index <- sem_index + 1
      cp_start <- cp_end
      next
    }
    
    sem_months <- sem_days / 30.44
    activity_done <- max(sem_months * (1 - p_val / 100), 0)
    
    if (current_work + activity_done >= required_work) {
      remaining <- required_work - current_work
      end_date <- if (activity_done > 0) {
        cp_start + round((remaining / activity_done) * sem_days)
      } else {
        cp_end
      }
      
      dates <- c(dates, end_date)
      work  <- c(work, required_work)
      
      return(list(
        end_date = safe_date(end_date),
        dates = dates,
        work = work,
        completed = required_work
      ))
    }
    
    current_work <- current_work + activity_done
    dates <- c(dates, cp_end)
    work  <- c(work, current_work)
    cp_start <- cp_end
    sem_index <- sem_index + 1
  }
  
  list(
    end_date = NA_Date_,
    dates = dates,
    work = work,
    completed = current_work
  )
}

get_semester_prolong_vec <- function(input, n = 8) {
  x <- unlist(lapply(1:n, function(i) input[[paste0("sem_p_", i)]]), use.names = FALSE)
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  x
}

finite_date_range <- function(dates, fallback_start, fallback_end) {
  dates <- safe_date(dates)
  dates <- dates[!is.na(dates)]
  
  fallback_start <- safe_date(fallback_start)
  fallback_end   <- safe_date(fallback_end)
  
  if (length(dates) == 0 || is.na(fallback_start) || is.na(fallback_end)) {
    return(c(Sys.Date(), Sys.Date() + 30))
  }
  
  xmin <- min(dates)
  xmax <- max(dates)
  
  if (!is.finite(as.numeric(xmin))) xmin <- fallback_start
  if (!is.finite(as.numeric(xmax))) xmax <- fallback_end
  
  c(xmin, xmax)
}

semester_breaks <- function(xmin, xmax) {
  seq(from = floor_date(xmin, "year"), to = ceiling_date(xmax, "year"), by = "6 months")
}

# ============================================================
# UI
# ============================================================
lang_choices <- available_languages()
lang_default <- if ("en" %in% unname(lang_choices)) "en" else unname(lang_choices)[1]

ui <- fluidPage(
  fluidRow(
    column(8, uiOutput("title_ui")),
    column(
      2,
      div(
        style = "margin-top: 20px;",
        checkboxInput("advanced_mode", "Advanced mode", value = FALSE)
      )
    ),
    column(
      2,
      div(
        style = "margin-top: 20px;",
        selectInput(
          "lang_select",
          NULL,
          choices = lang_choices,
          selected = lang_default,
          width = "100%"
        )
      )
    )
  ),
  
  uiOutput("tabs_ui")
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  lang_code <- reactive({
    req(input$lang_select)
    input$lang_select
  })
  
  L <- reactive({
    load_language(lang_code())
  })
  
  output$title_ui <- renderUI({
    Ls <- L()
    titlePanel(tr(Ls, "title", "PhD Completion Forecast Simulator"))
  })
  
  output$tabs_ui <- renderUI({
    Ls <- L()
    
    tabsetPanel(
      tabPanel(
        tr(Ls, "tab_average_prolongation", "Average prolongation"),
        fluidRow(
          column(4, uiOutput("controls_ui")),
          column(
            8,
            uiOutput("forecast_title"),
            plotlyOutput("timeline_plot", height = 350),
            hr(),
            fluidRow(
              column(
                6,
                uiOutput("milestones_title"),
                verbatimTextOutput("milestone_text")
              ),
              column(
                6,
                uiOutput("stats_title"),
                verbatimTextOutput("stats_text")
              )
            )
          )
        )
      ),
      
      tabPanel(
        tr(Ls, "tab_semester_prolongation", "Semester prolongation"),
        fluidRow(
          column(4, uiOutput("controls_sem_ui")),
          column(
            8,
            uiOutput("forecast_title_sem"),
            plotlyOutput("timeline_plot_sem", height = 520),
            hr(),
            fluidRow(
              column(
                6,
                h4(tr(Ls, "milestones", "Milestones")),
                verbatimTextOutput("milestone_text_sem")
              ),
              column(
                6,
                h4(tr(Ls, "stats", "Statistics")),
                verbatimTextOutput("stats_text_sem")
              )
            ),
            hr(),
            verbatimTextOutput("summary_sem")
          )
        )
      ),
      
      tabPanel(
        tr(Ls, "tab_instructions", "Instructions"),
        fluidRow(
          column(12, uiOutput("instructions_ui"))
        )
      ),
      
      tabPanel(
        tr(Ls, "tab_changelog", "Changelog"),
        fluidRow(
          column(12, uiOutput("changelog_ui"))
        )
      ),
      
      tabPanel(
        tr(Ls, "tab_about", "About"),
        fluidRow(
          column(12, uiOutput("about_ui"))
        )
      )
    )
  })
  
  output$forecast_title   <- renderUI({ Ls <- L(); h3(tr(Ls, "forecast", "Projected Completion")) })
  output$forecast_title_sem <- renderUI({ Ls <- L(); h3(tr(Ls, "semester_forecast", "Semester-specific projection")) })
  output$milestones_title <- renderUI({ Ls <- L(); h4(tr(Ls, "milestones", "Milestones")) })
  output$stats_title      <- renderUI({ Ls <- L(); h4(tr(Ls, "stats", "Statistics")) })
  
  output$controls_ui <- renderUI({
    Ls <- L()
    avg_plot_choices <- c("timeline", "activity")
    names(avg_plot_choices) <- c(
      tr(Ls, "timeline_view", "Timeline view"),
      tr(Ls, "activity_view", "Activity view")
    )
    
    tagList(
      h4(tr(Ls, "person", "Person")),
      numericInput("birth_year", tr(Ls, "birth_year", "Birth year"), value = year(Sys.Date()) - 30, min = 1940, max = year(Sys.Date())),
      h4(tr(Ls, "status", "Current status")),
      dateInput("start", tr(Ls, "start_date", "Start date"), value = Sys.Date() %m-% months(6)),
      conditionalPanel(
        condition = "input.advanced_mode == true",
        dateInput("today", tr(Ls, "today_date", "Current date"), value = Sys.Date())
      ),
      numericInput("remaining_checkpoint", tr(Ls, "remaining_activity", "Remaining activity (months)"), value = default_remaining_activity, min = 0, max = required_work, step = 0.01),
      h4(tr(Ls, "future", "Future assumption")),
      sliderInput("future_prolong", tr(Ls, "prolong", "Expected future prolongation (%)"), min = 0, max = 50, value = 10),
      conditionalPanel(
        condition = "input.advanced_mode == true",
        checkboxInput("compare", label = tr(Ls, "compare", "Enable scenario B"), value = FALSE),
        conditionalPanel(
          condition = "input.compare == true",
          sliderInput("future_prolong_B", tr(Ls, "prolong", "Expected future prolongation (%)"), min = 0, max = 50, value = 20)
        )
      ),
      radioButtons(
        "avg_plot_mode",
        label = tr(Ls, "plot_style", "Plot style"),
        choices = avg_plot_choices,
        selected = "timeline",
        inline = TRUE
      ),
      hr(),
      strong(tr(Ls, "requirement", "Program requirement:")),
      p(tr(Ls, "req_text", "48 work months (no prolongation)"))
    )
  })
  
  output$controls_sem_ui <- renderUI({
    Ls <- L()
    sem_plot_choices <- c("timeline", "activity")
    names(sem_plot_choices) <- c(
      tr(Ls, "timeline_view", "Timeline view"),
      tr(Ls, "activity_view", "Activity view")
    )
    
    tagList(
      h4(tr(Ls, "person", "Person")),
      numericInput("birth_year_sem", tr(Ls, "birth_year", "Birth year"), value = year(Sys.Date()) - 30, min = 1940, max = year(Sys.Date())),
      h4(tr(Ls, "status", "Current status")),
      dateInput("start_sem", tr(Ls, "start_date", "Start date"), value = Sys.Date() %m-% months(6)),
      radioButtons(
        "sem_plot_mode",
        label = tr(Ls, "plot_style", "Plot style"),
        choices = sem_plot_choices,
        selected = "timeline",
        inline = TRUE
      ),
      h4(tr(Ls, "prolongation_per_semester", "Prolongation per semester (%)")),
      lapply(1:8, function(i) {
        numericInput(
          inputId = paste0("sem_p_", i),
          label   = paste(tr(Ls, "semester", "Semester"), i),
          value   = 0,
          min     = 0,
          max     = 100,
          step    = 1
        )
      })
    )
  })
  
  output$instructions_ui <- renderUI({
    includeMarkdown(file.path("lang", paste0(lang_code(), "_instructions.md")))
  })
  
  output$changelog_ui <- renderUI({
    includeMarkdown("CHANGELOG.md")
  })
  
  output$about_ui <- renderUI({
    includeMarkdown("ABOUT.md")
  })
  
  output$timeline_plot <- renderPlotly({
    Ls <- L()
    
    A <- calc_average(input$start, input$today, input$remaining_checkpoint, input$future_prolong)
    
    start <- A$start
    today <- A$today
    done <- A$completed
    elapsed <- A$elapsed
    endA <- A$end_date
    pA <- input$future_prolong
    
    req(!is.na(start), !is.na(today), !is.na(endA))
    
    ref_end <- start %m+% months(required_work)
    
    m50A  <- milestone_date_model(start, today, done, elapsed, pA, 0.5 * required_work)
    m80A  <- milestone_date_model(start, today, done, elapsed, pA, 0.8 * required_work)
    mEndA <- milestone_date_model(start, today, done, elapsed, pA, required_work)
    
    compare_on <- isTRUE(input$compare)
    if (compare_on) {
      B <- calc_average(input$start, input$today, input$remaining_checkpoint, input$future_prolong_B)
      endB <- B$end_date
      pB <- input$future_prolong_B
    }
    
    if (identical(input$avg_plot_mode, "activity")) {
      
      pathA <- data.frame(date = c(start, today, endA), activity = c(0, work_to_pct(done), 100))
      pathRef <- data.frame(date = c(start, ref_end), activity = c(0, 100))
      
      marks <- data.frame(
        label = c("Start", "Today", "50%", "80%", tr(Ls, "end", "End")),
        date  = c(start, today, m50A, m80A, mEndA),
        activity = c(0, work_to_pct(done), 50, 80, 100),
        offset = c(0, 0, 3, 3, 3),
        stringsAsFactors = FALSE
      )
      
      marks$hover <- paste0(
        marks$label, ": ",
        formatC(marks$activity, format = "f", digits = 0),
        "%"
      )
      
      if (compare_on) {
        pathB <- data.frame(date = c(start, today, endB), activity = c(0, work_to_pct(B$completed), 100))
        
        p <- ggplot() +
          geom_line(data = pathRef, aes(date, activity), linetype = "dashed", linewidth = 1.2, color = "grey40") +
          geom_line(data = pathA, aes(date, activity), linewidth = 1.6, color = "steelblue") +
          geom_line(data = pathB, aes(date, activity), linewidth = 1.6, color = "firebrick")
        
        valid_dates <- c(pathA$date, pathB$date, pathRef$date, marks$date)
      } else {
        p <- ggplot() +
          geom_line(data = pathRef, aes(date, activity), linetype = "dashed", linewidth = 1.2, color = "grey40") +
          geom_line(data = pathA, aes(date, activity), linewidth = 1.6, color = "steelblue")
        
        valid_dates <- c(pathA$date, pathRef$date, marks$date)
      }
      
      xrange <- finite_date_range(valid_dates, start, ref_end)
      xmin <- xrange[1]
      xmax <- xrange[2]
      
      p <- suppressWarnings(
        p +
          geom_point(data = marks, aes(date, activity, text = hover), size = 3) +
          geom_text(data = marks, aes(date, activity + offset, label = label), size = 4, inherit.aes = FALSE) +
          scale_x_date(limits = c(xmin, xmax %m+% months(12))) +
          scale_y_continuous(
            limits = c(0, 100),
            breaks = seq(0, 100, by = 10),
            labels = function(x) paste0(x, "%")
          ) +
          labs(x = NULL, y = "Activity (%)") +
          theme_minimal()
      )
      
      ggplotly(p, tooltip = "text")
      
    } else {
      
      y_nom <- 1.00
      y_A   <- 0.72
      y_B   <- 0.44
      
      nominal <- data.frame(date = c(start, today, ref_end), y = c(y_nom, y_nom, y_nom))
      
      scenario_y_path <- function(date, today, end_date, y_end, y_past = y_nom - 0.06) {
        date <- safe_date(date)
        today <- safe_date(today)
        end_date <- safe_date(end_date)
        
        if (is.na(date) || is.na(today) || is.na(end_date)) return(NA_real_)
        
        if (date <= today) {
          total_past <- as.numeric(today - start)
          if (!is.finite(total_past) || total_past <= 0) return(y_past)
          
          elapsed <- as.numeric(date - start)
          frac <- max(min(elapsed / total_past, 1), 0)
          return(y_nom + frac * (y_past - y_nom))
        }
        
        total_future <- as.numeric(end_date - today)
        if (!is.finite(total_future) || total_future <= 0) return(y_end)
        
        elapsed_future <- as.numeric(date - today)
        frac <- max(min(elapsed_future / total_future, 1), 0)
        y_past + frac * (y_end - y_past)
      }
      
      scenA <- data.frame(
        date = c(start, today, endA),
        y = c(
          scenario_y_path(start, today, endA, y_A),
          scenario_y_path(today, today, endA, y_A),
          y_A
        )
      )
      
      m50A  <- milestone_date_model(start, today, done, elapsed, pA, 0.5 * required_work)
      m80A  <- milestone_date_model(start, today, done, elapsed, pA, 0.8 * required_work)
      mEndA <- milestone_date_model(start, today, done, elapsed, pA, required_work)
      
      y50A  <- scenario_y_path(m50A, today, endA, y_A)
      y80A  <- scenario_y_path(m80A, today, endA, y_A)
      yEndA <- y_A
      
      marksA <- data.frame(
        label = c("Start", "Today", "50%", "80%", tr(Ls, "end", "End"), format(ref_end, "%Y-%m-%d")),
        date  = c(start, today, m50A, m80A, mEndA, ref_end),
        y     = c(
          y_nom,
          scenario_y_path(today, today, endA, y_A),
          y50A,
          y80A,
          yEndA,
          y_nom
        ),
        stringsAsFactors = FALSE
      )
      marksA$text <- paste0(marksA$label, ": ", format(marksA$date, "%Y-%m-%d"))
      
      if (compare_on) {
        
        B <- calc_average(input$start, input$today, input$remaining_checkpoint, input$future_prolong_B)
        endB <- B$end_date
        pB <- input$future_prolong_B
        
        m50B <- milestone_date_model(start, today, B$completed, B$elapsed, pB, 0.5 * required_work)
        m80B <- milestone_date_model(start, today, B$completed, B$elapsed, pB, 0.8 * required_work)
        mEndB <- milestone_date_model(start, today, B$completed, B$elapsed, pB, required_work)
        
        scenB <- data.frame(
          date = c(start, today, endB),
          y = c(
            scenario_y_path(start, today, endB, y_B, y_past = y_nom - 0.10),
            scenario_y_path(today, today, endB, y_B, y_past = y_nom - 0.10),
            y_B
          )
        )
        scenB$text <- c(
          paste0("Start B: ", format(start, "%Y-%m-%d")),
          paste0("Today B: ", format(today, "%Y-%m-%d")),
          paste0("Scenario B end: ", format(endB, "%Y-%m-%d"))
        )
        
        y50B  <- scenario_y_path(m50B, today, endB, y_B, y_past = y_nom - 0.10)
        y80B  <- scenario_y_path(m80B, today, endB, y_B, y_past = y_nom - 0.10)
        yEndB <- y_B
        
        marksB <- data.frame(
          label = c("50%", "80%", tr(Ls, "end", "End"), format(endB, "%Y-%m-%d")),
          date  = c(m50B, m80B, mEndB, endB),
          y     = c(y50B, y80B, yEndB, yEndB),
          stringsAsFactors = FALSE
        )
        marksB$text <- paste0(marksB$label, ": ", format(marksB$date, "%Y-%m-%d"))
        
        p <- ggplot() +
          geom_line(data = nominal, aes(date, y), linetype = "dotted", linewidth = 1.2, color = "grey40") +
          geom_line(data = scenA, aes(date, y), linewidth = 1.6, color = "steelblue") +
          geom_line(data = scenB, aes(date, y), linewidth = 1.6, color = "firebrick")
        
        marks_plot <- rbind(marksA, marksB)
        valid_dates <- c(nominal$date, scenA$date, scenB$date, marks_plot$date)
        
      } else {
        
        p <- ggplot() +
          geom_line(data = nominal, aes(date, y), linetype = "dotted", linewidth = 1.2, color = "grey40") +
          geom_line(data = scenA, aes(date, y), linewidth = 1.6, color = "steelblue")
        
        marks_plot <- marksA
        valid_dates <- c(nominal$date, scenA$date, marks_plot$date)
      }
      
      valid_dates <- valid_dates[!is.na(valid_dates)]
      if (length(valid_dates) == 0) valid_dates <- c(start, ref_end)
      
      xrange <- finite_date_range(valid_dates, start, ref_end)
      xmin <- xrange[1]
      xmax <- xrange[2]
      
      x_breaks <- semester_breaks(xmin, xmax)
      
      p <- suppressWarnings(
        p +
          geom_point(data = marks_plot, aes(date, y, text = text), size = 3) +
          geom_text(
            data = marks_plot,
            aes(date, y, label = label),
            nudge_y = -0.06,
            size = 4,
            inherit.aes = FALSE
          ) +
          scale_x_date(
            limits = c(xmin, xmax %m+% months(12)),
            breaks = x_breaks,
            date_labels = "%Y %b"
          ) +
          scale_y_continuous(
            limits = c(0.3, 1.05),
            breaks = NULL,
            labels = NULL
          ) +
          labs(x = NULL, y = NULL) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      )
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$milestone_text <- renderText({
    Ls <- L()
    
    A <- calc_average(input$start, input$today, input$remaining_checkpoint, input$future_prolong)
    
    start <- A$start
    today <- A$today
    done <- A$completed
    elapsed <- A$elapsed
    pA <- input$future_prolong
    
    m50A  <- milestone_date_model(start, today, done, elapsed, pA, 0.5 * required_work)
    m80A  <- milestone_date_model(start, today, done, elapsed, pA, 0.8 * required_work)
    mEndA <- milestone_date_model(start, today, done, elapsed, pA, required_work)
    
    dur50A  <- format_duration_ymd(Ls, today, m50A)
    dur80A  <- format_duration_ymd(Ls, today, m80A)
    durEndA <- format_duration_ymd(Ls, today, mEndA)
    
    end_label <- tr(Ls, "end", "End")
    
    if (!isTRUE(input$compare)) {
      return(paste0(
        "50%: ", format(m50A, "%Y-%m-%d"), " — ", dur50A, "\n",
        "80%: ", format(m80A, "%Y-%m-%d"), " — ", dur80A, "\n",
        end_label, ": ", format(mEndA, "%Y-%m-%d"), " — ", durEndA
      ))
    }
    
    pB <- input$future_prolong_B
    m50B  <- milestone_date_model(start, today, done, elapsed, pB, 0.5 * required_work)
    m80B  <- milestone_date_model(start, today, done, elapsed, pB, 0.8 * required_work)
    mEndB <- milestone_date_model(start, today, done, elapsed, pB, required_work)
    
    dur50B  <- format_duration_ymd(Ls, today, m50B)
    dur80B  <- format_duration_ymd(Ls, today, m80B)
    durEndB <- format_duration_ymd(Ls, today, mEndB)
    
    paste0(
      "Scenario A (", pA, "% prolongation)\n",
      "50%: ", format(m50A, "%Y-%m-%d"), " — ", dur50A, "\n",
      "80%: ", format(m80A, "%Y-%m-%d"), " — ", dur80A, "\n",
      end_label, ": ", format(mEndA, "%Y-%m-%d"), " — ", durEndA,
      "\n\n",
      "Scenario B (", pB, "% prolongation)\n",
      "50%: ", format(m50B, "%Y-%m-%d"), " — ", dur50B, "\n",
      "80%: ", format(m80B, "%Y-%m-%d"), " — ", dur80B, "\n",
      end_label, ": ", format(mEndB, "%Y-%m-%d"), " — ", durEndB
    )
  })
  
  output$stats_text <- renderText({
    Ls <- L()
    A <- calc_average(input$start, input$today, input$remaining_checkpoint, input$future_prolong)
    
    warn <- ""
    if (isTRUE(A$adjusted)) {
      warn <- "⚠️ Template input implies faster-than-nominal progress. Adjusted to keep efficiency ≤ 100%.\n\n"
    }
    
    paste0(
      warn,
      tr(Ls, "activity_so_far", "Activity so far"), ": ", round(A$completed, 2), " ", tr(Ls, "months", "months"), "\n",
      tr(Ls, "efficiency", "Efficiency"), ": ", round(100 * A$efficiency, 1), tr(Ls, "percent", "%"), "\n",
      tr(Ls, "adjusted_working_hours_per_year", "Adjusted working hours/year"), ": ",
      round(adjusted_hours_per_year(input$birth_year, input$today), 0), " ", tr(Ls, "hours", "hours")
    )
  })
  
  semester_metrics <- reactive({
    start <- safe_date(input$start_sem)
    today <- Sys.Date()
    
    prolong_vec <- get_semester_prolong_vec(input, 8)
    sim <- simulate_semesters_from_start(start, prolong_vec, min_semesters = 8, max_extra_semesters = 20)
    
    path <- data.frame(
      date = safe_date(sim$dates),
      work = as.numeric(sim$work)
    )
    path <- path[!is.na(path$date) & !is.na(path$work), , drop = FALSE]
    if (nrow(path) == 0) {
      path <- data.frame(date = c(start, start %m+% months(1)), work = c(0, 0))
    }
    path$activity <- work_to_pct(path$work)
    
    ref_end <- start %m+% months(required_work)
    
    list(
      start = start,
      today = today,
      prolong_vec = prolong_vec,
      sim = sim,
      path = path,
      ref_end = ref_end
    )
  })
  
  output$milestone_text_sem <- renderText({
    Ls <- L()
    start <- safe_date(input$start_sem)
    today <- Sys.Date()
    req(!is.na(start))
    
    prolong_vec <- get_semester_prolong_vec(input, 8)
    sim <- simulate_semesters_from_start(start, prolong_vec, min_semesters = 8, max_extra_semesters = 20)
    
    path <- data.frame(
      date = safe_date(sim$dates),
      work = as.numeric(sim$work)
    )
    path <- path[!is.na(path$date) & !is.na(path$work), , drop = FALSE]
    path <- path[!duplicated(path$date), , drop = FALSE]
    path <- path[order(path$date), , drop = FALSE]
    path$activity <- work_to_pct(path$work)
    
    first_reach_date <- function(df, threshold) {
      idx <- which(df$activity >= threshold)
      if (length(idx) == 0) return(NA_Date_)
      df$date[idx[1]]
    }
    
    pct50_date  <- first_reach_date(path, 50)
    pct80_date  <- first_reach_date(path, 80)
    pct100_date <- sim$end_date
    
    paste0(
      tr(Ls, "label_start", "Start"), ": ", format(start, "%Y-%m-%d"), "
",
      tr(Ls, "label_today", "Today"), ": ", format(today, "%Y-%m-%d"), "
",
      tr(Ls, "pct_50", "50%"), ": ", ifelse(is.na(pct50_date), tr(Ls, "not_reached", "not reached"), format(pct50_date, "%Y-%m-%d")), "
",
      tr(Ls, "pct_80", "80%"), ": ", ifelse(is.na(pct80_date), tr(Ls, "not_reached", "not reached"), format(pct80_date, "%Y-%m-%d")), "
",
      tr(Ls, "pct_100", "100%"), ": ", ifelse(is.na(pct100_date), tr(Ls, "not_reached", "not reached"), format(pct100_date, "%Y-%m-%d"))
    )
  })
  
  output$stats_text_sem <- renderText({
    Ls <- L()
    m <- semester_metrics()
    path <- m$path
    today <- m$today
    sim <- m$sim
    
    current_activity <- if (nrow(path) >= 2) {
      approx(
        x = as.numeric(path$date),
        y = path$activity,
        xout = as.numeric(today),
        rule = 2
      )$y
    } else {
      path$activity[1]
    }
    
    end_activity <- if (!is.na(sim$end_date)) 100 else tail(path$activity, 1)
    
    paste0(
      tr(Ls, "activity_so_far", "Activity so far"), ": ", round(current_activity, 2), "%
",
      tr(Ls, "projected_final_activity", "Projected final activity"), ": ", round(end_activity, 2), "%
",
      tr(Ls, "semester_inputs", "Semester inputs"), ": ", paste(m$prolong_vec, collapse = ", "), "
",
      tr(Ls, "estimated_completion", "Estimated completion"), ": ", ifelse(is.na(sim$end_date), tr(Ls, "unavailable", "unavailable"), format(sim$end_date, "%Y-%m-%d"))
    )
  })
  
  output$timeline_plot_sem <- renderPlotly({
    Ls <- L()
    start <- safe_date(input$start_sem)
    today <- Sys.Date()
    req(!is.na(start))
    
    prolong_vec <- get_semester_prolong_vec(input, 8)
    sim <- simulate_semesters_from_start(start, prolong_vec, min_semesters = 8, max_extra_semesters = 20)
    
    path <- data.frame(
      date = safe_date(sim$dates),
      work = as.numeric(sim$work)
    )
    path <- path[!is.na(path$date) & !is.na(path$work), , drop = FALSE]
    
    if (nrow(path) == 0) {
      path <- data.frame(date = c(start, start %m+% months(1)), work = c(0, 0))
    }
    
    path <- path[!duplicated(path$date), , drop = FALSE]
    path <- path[order(path$date), , drop = FALSE]
    path$activity <- work_to_pct(path$work)
    
    ref_end <- start %m+% months(required_work)
    ref <- data.frame(date = c(start, ref_end), activity = c(0, 100))
    
    first_reach_date <- function(df, threshold) {
      idx <- which(df$activity >= threshold)
      if (length(idx) == 0) return(NA_Date_)
      df$date[idx[1]]
    }
    
    y_nom  <- 1.00
    y_proj <- 0.72
    
    scenario_y_path <- function(date, today, end_date, y_end, y_past = y_nom - 0.06) {
      date <- safe_date(date)
      today <- safe_date(today)
      end_date <- safe_date(end_date)
      
      if (is.na(date) || is.na(today) || is.na(end_date)) return(NA_real_)
      
      if (date <= today) {
        total_past <- as.numeric(today - start)
        if (!is.finite(total_past) || total_past <= 0) return(y_past)
        
        elapsed <- as.numeric(date - start)
        frac <- max(min(elapsed / total_past, 1), 0)
        return(y_nom + frac * (y_past - y_nom))
      }
      
      total_future <- as.numeric(end_date - today)
      if (!is.finite(total_future) || total_future <= 0) return(y_end)
      
      elapsed_future <- as.numeric(date - today)
      frac <- max(min(elapsed_future / total_future, 1), 0)
      y_past + frac * (y_end - y_past)
    }
    
    pct50_date  <- first_reach_date(path, 50)
    pct80_date  <- first_reach_date(path, 80)
    pct100_date <- sim$end_date
    
    semester_dates <- path$date[-1]
    semester_labels <- sapply(semester_dates, semester_label, Ls = Ls)
    
    if (identical(input$sem_plot_mode, "activity")) {
      
      today_activity <- if (nrow(path) >= 2) {
        approx(
          x = as.numeric(path$date),
          y = path$activity,
          xout = as.numeric(today),
          rule = 2
        )$y
      } else {
        path$activity[1]
      }
      
      semester_points <- data.frame(
        date = semester_dates,
        activity = path$activity[-1],
        label = semester_labels,
        label_y = path$activity[-1] + 6,
        stringsAsFactors = FALSE
      )
      
      milestone_list <- list(
        data.frame(label = tr(Ls, "label_start", "Start"), date = start, activity = 0, stringsAsFactors = FALSE),
        data.frame(label = tr(Ls, "label_today", "Today"), date = today, activity = today_activity, stringsAsFactors = FALSE)
      )
      
      if (!is.na(pct50_date)) {
        milestone_list[[length(milestone_list) + 1]] <-
          data.frame(label = tr(Ls, "pct_50", "50%"), date = pct50_date, activity = 50, stringsAsFactors = FALSE)
      }
      if (!is.na(pct80_date)) {
        milestone_list[[length(milestone_list) + 1]] <-
          data.frame(label = tr(Ls, "pct_80", "80%"), date = pct80_date, activity = 80, stringsAsFactors = FALSE)
      }
      if (!is.na(pct100_date)) {
        milestone_list[[length(milestone_list) + 1]] <-
          data.frame(label = tr(Ls, "pct_100", "100%"), date = pct100_date, activity = 100, stringsAsFactors = FALSE)
      }
      
      marks <- do.call(rbind, milestone_list)
      marks$hover <- paste0(marks$label, ": ", formatC(marks$activity, format = "f", digits = 0), "%")
      marks$label_y <- marks$activity - 5
      
      valid_dates <- c(path$date, ref$date, sim$end_date, start, today, pct50_date, pct80_date, pct100_date)
      valid_dates <- valid_dates[!is.na(valid_dates)]
      xrange <- finite_date_range(valid_dates, start, ref_end)
      xmin <- xrange[1]
      xmax <- xrange[2]
      
      p <- suppressWarnings(
        ggplot() +
          geom_line(data = ref, aes(date, activity), linetype = "dashed", color = "grey40", linewidth = 1.2) +
          geom_line(data = path, aes(date, activity), color = "darkgreen", linewidth = 1.6) +
          geom_text(
            data = semester_points,
            aes(date, label_y, label = label),
            vjust = 0,
            size = 4
          ) +
          geom_point(data = marks, aes(date, activity, text = hover), color = "steelblue", size = 3) +
          geom_text(
            data = marks,
            aes(date, label_y, label = label),
            vjust = 1,
            size = 4,
            fontface = "bold"
          ) +
          scale_x_date(limits = c(xmin, xmax), date_labels = "%Y-%m-%d") +
          scale_y_continuous(
            limits = c(0, 100),
            breaks = seq(0, 100, by = 10),
            labels = function(x) paste0(x, "%")
          ) +
          labs(x = NULL, y = "Activity (%)") +
          theme_minimal()
      )
      
      ggplotly(p, tooltip = "text")
      
    } else {
      
      nominal <- data.frame(
        date = c(start, today, ref_end),
        y = c(y_nom, y_nom, y_nom)
      )
      
      scen <- data.frame(
        date = c(start, today, sim$end_date),
        y = c(
          scenario_y_path(start, today, sim$end_date, y_proj),
          scenario_y_path(today, today, sim$end_date, y_proj),
          y_proj
        )
      )
      
      semester_points <- data.frame(
        date = semester_dates,
        y = sapply(semester_dates, function(d) scenario_y_path(d, today, sim$end_date, y_proj)),
        label = semester_labels,
        stringsAsFactors = FALSE
      )
      semester_points$label_y <- semester_points$y + 0.05
      semester_points$text <- paste0(semester_points$label, ": ", format(semester_points$date, "%Y-%m-%d"))
      
      milestone_dates <- c(pct50_date, pct80_date, pct100_date)
      milestone_labels <- c(tr(Ls, "pct_50", "50%"), tr(Ls, "pct_80", "80%"), tr(Ls, "pct_100", "100%"))
      valid <- !is.na(milestone_dates)
      
      milestone_df <- data.frame(
        label = milestone_labels[valid],
        date = milestone_dates[valid],
        stringsAsFactors = FALSE
      )
      
      milestone_df$y <- sapply(milestone_df$date, function(d) scenario_y_path(d, today, sim$end_date, y_proj))
      milestone_df$label_y <- milestone_df$y - 0.05
      milestone_df$text <- paste0(milestone_df$label, ": ", format(milestone_df$date, "%Y-%m-%d"))
      
      valid_dates <- c(start, today, semester_dates, milestone_dates, ref_end, sim$end_date)
      valid_dates <- valid_dates[!is.na(valid_dates)]
      xrange <- finite_date_range(valid_dates, start, ref_end)
      xmin <- xrange[1]
      xmax <- xrange[2]
      x_breaks <- semester_breaks(xmin, xmax)
      
      p <- suppressWarnings(
        ggplot() +
          geom_line(data = nominal, aes(date, y), linetype = "dotted", linewidth = 1.2, color = "grey40") +
          geom_line(data = scen, aes(date, y), linewidth = 1.6, color = "darkgreen") +
          geom_point(data = semester_points, aes(date, y, text = text), color = "darkgreen", size = 3) +
          geom_text(
            data = semester_points,
            aes(date, label_y, label = label),
            vjust = 0,
            size = 4
          ) +
          geom_point(data = milestone_df, aes(date, y, text = text), color = "steelblue", size = 3) +
          geom_text(
            data = milestone_df,
            aes(date, label_y, label = label),
            vjust = 1,
            size = 4,
            fontface = "bold"
          ) +
          scale_x_date(limits = c(xmin, xmax %m+% months(12)), breaks = x_breaks, date_labels = "%Y %b") +
          scale_y_continuous(limits = c(0.3, 1.05), breaks = NULL, labels = NULL) +
          labs(x = NULL, y = NULL) +
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      )
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$summary_sem <- renderText({
    Ls <- L()
    m <- semester_metrics()
    sim <- m$sim
    today <- m$today
    
    if (is.na(sim$end_date)) {
      return(paste0(
      tr(Ls, "label_today", "Today"), ": ", format(today, "%Y-%m-%d"), "
",
        tr(Ls, "projected_completion", "Projected completion"), ": ", tr(Ls, "not_reached_within_horizon", "not reached within simulated horizon"), "
",
        tr(Ls, "time_remaining", "Time remaining"), ": ", tr(Ls, "unavailable", "unavailable")
      ))
    }
    
    paste0(
      tr(Ls, "label_today", "Today"), ": ", format(today, "%Y-%m-%d"), "
",
      tr(Ls, "projected_completion", "Projected completion"), ": ", format(sim$end_date, "%Y-%m-%d"), "
",
      tr(Ls, "time_remaining", "Time remaining"), ": ", format_duration_ymd(Ls, today, sim$end_date)
    )
  })
}

shinyApp(ui, server)
