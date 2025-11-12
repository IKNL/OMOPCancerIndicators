# app.R - Breast Cancer Indicators (Unified Green UI with OMOP vs Source Comparison)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(reshape2)
library(readr)
library(RColorBrewer)

# ---------- Helpers ----------
safe_read_csv <- function(path, expected_cols = NULL) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(
    read_csv(path, show_col_types = FALSE),
    error = function(e) { warning("Error reading ", path); NULL }
  )
  if (!is.null(expected_cols)) {
    missing <- setdiff(expected_cols, names(df))
    if (length(missing) > 0) warning("Missing columns: ", paste(missing, collapse = ", "))
  }
  df
}

# ---------- Paths and Data ----------
appDir <- system.file("shiny/Diagnostics", package = "OMOPCancerIndicators")
pathFile <- file.path(appDir, "resultsPath.txt")
if (!file.exists(pathFile)) stop("Results path file not found. Run runDiagnostics() first.")
resultsFolder <- readLines(pathFile, warn = FALSE)
if (!dir.exists(resultsFolder)) stop("Results folder does not exist: ", resultsFolder)

# --- Load Data ---
cohortCounts_omop <- safe_read_csv(file.path(resultsFolder, "omop","cohortCounts_omop.csv"))
cohortCounts_source <- safe_read_csv(file.path(resultsFolder, "source", "cohortCounts_source.csv"))
ageDistribution_omop <- safe_read_csv(file.path(resultsFolder, "omop","ageDistribution_omop.csv"))
ageDistribution_source <- safe_read_csv(file.path(resultsFolder,"source", "ageDistribution_source.csv"))
overlaps <- safe_read_csv(file.path(resultsFolder, "omop","overlaps_omop.csv"))
stage_omop_df <- safe_read_csv(file.path(resultsFolder,"omop", "stage_omop.csv"))
stage_source_df <- safe_read_csv(file.path(resultsFolder, "source","stage_source.csv"))
receptor_omop_df <- safe_read_csv(file.path(resultsFolder,"omop", "measurements_omop.csv"))
receptor_source_df <- safe_read_csv(file.path(resultsFolder,"source", "measurements_source.csv"))

# --- Combine Source and OMOP ---
cohortCounts_omop$db <- "OMOP"
cohortCounts_source$db <- "Source"
cohortCounts <- rbind(cohortCounts_omop, cohortCounts_source)

ageDistribution_omop$db <- "OMOP"
ageDistribution_source$db <- "Source"
ageDistribution <- rbind(ageDistribution_omop, ageDistribution_source)

stage_omop_df$db <- "OMOP"
stage_source_df$db <- "Source"
stage_df <- rbind(stage_omop_df, stage_source_df)

if (!is.null(receptor_omop_df)) receptor_omop_df$db <- "OMOP"
if (!is.null(receptor_source_df)) receptor_source_df$db <- "Source"
receptor_df <- rbind(receptor_omop_df, receptor_source_df)

# --- Validation ---
if (is.null(cohortCounts)) stop("cohortCounts.csv is required.")
if (!"cohortName" %in% names(cohortCounts)) stop("cohortCounts.csv must contain 'cohortName'.")
if (!"n" %in% names(cohortCounts)) {
  candidate <- grep("count|Count|person", names(cohortCounts), value = TRUE)[1]
  if (!is.na(candidate)) cohortCounts <- cohortCounts %>% rename(n = !!sym(candidate))
  else stop("cohortCounts.csv must contain a count column 'n'.")
}

# ---------- Color Palette ----------
main_green  <- "#004d4d"
mid_green   <- "#007777"
light_green <- "#33aaaa"
pale_green  <- "#d0f0e0"

omop_color <- "#800000"
source_color <- "#F08080"

# ---------- UI ----------
ui <- dashboardPage(
  dashboardHeader(title = "Breast Cancer Indicators"),
  dashboardSidebar(width = 272,
                   sidebarMenu(
                     menuItem("Overview", tabName = "overview", icon = icon("table")),
                     menuItem("Age", tabName = "age", icon = icon("chart-bar")),
                     menuItem("Stage", tabName = "stage", icon = icon("layer-group")),
                     menuItem("Receptor", tabName = "receptor", icon = icon("vials")),
                     menuItem("Overlaps", tabName = "overlaps", icon = icon("th"))
                   )
  ),
  dashboardBody(
    tags$style(HTML(paste0("
      body { font-family: 'Segoe UI', Roboto, sans-serif; background-color: #f4f9f4; }
      .box { border-top: 3px solid ", main_green, "; box-shadow: 0 4px 10px rgba(0,0,0,0.08); border-radius: 12px; }
      .box-title { font-weight: 600; font-size: 15px; color: ", main_green, "; }

      /* Green box header strips */
      .box.box-primary { border-top-color: ", mid_green, "; }
      .box.box-info    { border-top-color: ", light_green, "; }
      .box.box-success { border-top-color: ", main_green, "; }
      .box.box-warning { border-top-color: ", mid_green, "; }

      /* Header and sidebar styling */
      .skin-blue .main-header .logo {
        background-color: ", main_green, ";
        color: white;
        font-weight: 600;
        font-size: 16px;
        width: auto !important;
        padding: 0 15px;
        white-space: nowrap;
      }
      .skin-blue .main-header .logo:hover {
        background-color: ", light_green, ";
        color: white;
      }
      .skin-blue .main-header .navbar { background-color: ", main_green, "; }
      .skin-blue .main-sidebar { background-color: ", main_green, "; }
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        background-color: ", light_green, "; color: white;
      }

      /* Menu toggle (hamburger) hover */
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: ", light_green, ";
      }

      /* Radio button (metric selector) styling - fully green */
      .radio label { color: ", main_green, "; font-weight: 500; }
      input[type='radio'] {
        accent-color: ", mid_green, ";
      }
      input[type='radio']:hover {
        accent-color: ", light_green, ";
      }

      /* DataTable theming */
      table.dataTable thead th {
        background-color: ", mid_green, ";
        color: white;
        font-weight: 600;
      }
      table.dataTable tbody tr:hover {
        background-color: ", pale_green, " !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: ", mid_green, " !important;
        color: white !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: ", light_green, " !important;
        color: white !important;
      }
    "))),
    
    tabItems(
      # Overview
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 6, title = "Cohort Counts (Table)", status = "primary", DTOutput("cohort_table")),
                box(width = 6, title = "Cohort Counts (Bar Chart)", status = "primary", plotOutput("cohort_bar", height = "420px"))
              )
      ),
      # Age
      tabItem(tabName = "age",
              fluidRow(
                box(width = 4, title = "Filters", status = "info",
                    selectInput("age_cohort", "Select Cohort", choices = unique(ageDistribution$cohortName))
                ),
                box(width = 8, title = "Age Distribution", status = "primary", plotOutput("age_plot", height = "420px"))
              ),
              fluidRow(box(width = 12, DTOutput("age_table")))
      ),
      # Stage
      tabItem(tabName = "stage",
              fluidRow(
                box(width = 2, title = "Display Metric", status = "info",
                    radioButtons("stage_metric", "Show:", choices = c("Percentage", "Count"), selected = "Percentage")
                ),
                box(width = 6, title = "Stage Distribution (Bar Chart)", status = "primary", plotOutput("stage_plot", height = "420px")),
                box(width = 4, title = "Stage Distribution (Table)", status = "primary", DTOutput("stage_table"))
              )
      ),
      # Receptor
      tabItem(tabName = "receptor",
              fluidRow(
                box(width = 2, title = "Display Metric", status = "info",
                    radioButtons("receptor_metric", "Show:", choices = c("Percentage", "Count"), selected = "Percentage")
                ),
                box(width = 10, title = "Receptor Distribution", status = "primary", plotOutput("receptor_plot", height = "420px"))
              )
      ),
      # Overlaps
      tabItem(tabName = "overlaps",
              fluidRow(
                box(width = 12, title = "Overlap Heatmap (Counts)", status = "primary", plotOutput("overlap_heatmap", height = "600px"))
              )
      )
    )
  ), skin = "blue"
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # ---- Overview ----
  output$cohort_table <- renderDT({
    datatable(cohortCounts %>% arrange(desc(n)) %>% 
                select(Database = db, Cohort = cohortName, Count = n),
              options = list(pageLength = 12, scrollX = TRUE, rownames = FALSE))
  })
  
  output$cohort_bar <- renderPlot({
    df <- cohortCounts %>% arrange(n)
    fill_colors <- c("OMOP" = omop_color, "Source" = source_color)
    ggplot(df, aes(x = reorder(cohortName, n), y = n, fill = db)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = fill_colors) +
      labs(x = "", y = "Count", fill = "Database", title = "Cohort Sizes (OMOP vs Source)") +
      theme_minimal(base_size = 13)
  })
  
  # ---- Age ----
  output$age_plot <- renderPlot({
    req(input$age_cohort)
    df <- ageDistribution %>% filter(cohortName == input$age_cohort)
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No age data", size = 6) + theme_void()
    } else {
      fill_colors <- c("OMOP" = omop_color, "Source" = source_color)
      
      # Ensure proper and consistent ordering of age groups
      possible_ages <- sprintf("%d-%d", seq(0, 100, 10), seq(9, 109, 10))
      if (!"ageGroup" %in% names(df)) stop("ageDistribution must contain 'ageGroup' column.")
      df <- df %>% mutate(ageGroup = factor(ageGroup, levels = possible_ages, ordered = TRUE))
      
      ggplot(df, aes(x = ageGroup, y = n, fill = db)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = n), position = position_dodge(width = 0.9),
                  vjust = -0.3, size = 3.5, color = main_green) +
        scale_fill_manual(values = fill_colors) +
        labs(x = "Age Group", y = "Count", fill = "Database",
             title = paste("Age Distribution (OMOP vs Source):", input$age_cohort)) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$age_table <- renderDT({
    datatable(ageDistribution %>% filter(cohortName == input$age_cohort) %>%
                select(Database = db, Cohort = cohortName, AgeGroup = ageGroup, Count = n),
              options = list(pageLength = 10, scrollX = TRUE, rownames = FALSE))
  })
  
  # ---- Stage ----
  stage_breast <- reactive({
    req(stage_df)
    df <- stage_df %>% select(stage, n, percent, db)
    df
  })
  
  output$stage_plot <- renderPlot({
    df <- stage_breast()
    df$stage <- factor(df$stage, levels = c("0", "1", "2", "3", "4", "N/A"))
    fill_colors <- c("OMOP" = omop_color, "Source" = source_color)
    
    if (input$stage_metric == "Count") {
      ggplot(df, aes(x = stage, y = n, fill = db)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = n), position = position_dodge(width = 0.9),
                  vjust = -0.5, color = main_green) +
        scale_fill_manual(values = fill_colors) +
        labs(x = "Stage", y = "Count", fill = "Database", title = "Stage Distribution") +
        theme_minimal(base_size = 13)
    } else {
      ggplot(df, aes(x = stage, y = percent, fill = db)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = percent), position = position_dodge(width = 0.9),
                  vjust = -0.5, color = main_green) +
        scale_fill_manual(values = fill_colors) +
        labs(x = "Stage", y = "Percentage", fill = "Database", title = "Stage Distribution") +
        theme_minimal(base_size = 13)
    }
  })
  
  output$stage_table <- renderDT({
    df <- stage_breast()
    if (input$stage_metric == "Count") datatable(df %>% select(Database = db, Stage = stage, Count = n), 
                                                 options = list(pageLength = 12, scrollX = TRUE, rownames = FALSE))
    else datatable(df %>% select(Database = db, Stage = stage, Percentage = percent), 
                   options = list(pageLength = 12, scrollX = TRUE, rownames = FALSE))
  })
  
  # ---- Receptor ----
  output$receptor_plot <- renderPlot({
    req(receptor_df)
    
    if (nrow(receptor_df) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No receptor data", size = 6) + 
        theme_void()
    } else {
      df <- receptor_df
      df$receptor <- factor(df$receptor, levels = c("HER2", "ER", "PR", "Not available"))
      df$status <- factor(df$status, levels = c("Positive", "Negative", "Unknown"))
      df$db <- factor(df$db, levels = c("OMOP", "Source"))
      
      # Define ordered combo for bar order
      df$combo <- factor(
        paste0(df$status, "-", df$db),
        levels = c(
          "Positive-OMOP", "Negative-OMOP", "Unknown-OMOP",
          "Positive-Source", "Negative-Source", "Unknown-Source"
        )
      )
      
      # Define colors
      fill_map <- c(
        "Positive-OMOP"   = "#B22222",
        "Negative-OMOP"   = "#CD5C5C",
        "Unknown-OMOP"    = "#F4A6A6",
        "Positive-Source" = "#004d4d",
        "Negative-Source" = "#007777",
        "Unknown-Source"  = "#a7d7c5"
      )
      
      # Legend labels
      legend_labels <- c(
        "Positive-OMOP"   = "OMOP: Positive",
        "Negative-OMOP"   = "OMOP: Negative",
        "Unknown-OMOP"    = "OMOP: Unknown",
        "Positive-Source" = "Source: Positive",
        "Negative-Source" = "Source: Negative",
        "Unknown-Source"  = "Source: Unknown"
      )
      
      if (input$receptor_metric == "Count") {
        ggplot(df, aes(x = receptor, y = n, fill = combo)) +
          geom_col(position = position_dodge(width = 0.9)) +
          geom_text(aes(label = n), 
                    position = position_dodge(width = 0.9), 
                    vjust = -0.4, size = 3.5, color = "black") +
          scale_fill_manual(
            values = fill_map,
            labels = legend_labels,
            name = "Database / Status"
          ) +
          labs(x = "Receptor", y = "Count",
               title = "Receptor Positivity by Database and Status") +
          theme_minimal(base_size = 13) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        ggplot(df, aes(x = receptor, y = percent, fill = combo)) +
          geom_col(position = position_dodge(width = 0.9)) +
          geom_text(aes(label = sprintf("%.1f%%", percent)), 
                    position = position_dodge(width = 0.9), 
                    vjust = -0.4, size = 3.5, color = "black") +
          scale_fill_manual(
            values = fill_map,
            labels = legend_labels,
            name = "Database / Status"
          ) +
          labs(x = "Receptor", y = "Percentage (%)",
               title = "Receptor Positivity by Database and Status") +
          theme_minimal(base_size = 13) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }
  })
  
  
  
  # ---- Overlaps ----
  output$overlap_heatmap <- renderPlot({
    req(overlaps)
    cohorts <- sort(unique(c(overlaps$cohortName1, overlaps$cohortName2)))
    all_pairs <- expand.grid(cohortName1 = cohorts, cohortName2 = cohorts, stringsAsFactors = FALSE)
    mat_df <- overlaps %>%
      group_by(cohortName1, cohortName2) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      right_join(all_pairs, by = c("cohortName1", "cohortName2")) %>%
      mutate(n = ifelse(is.na(n), 0, n),
             diagonal = cohortName1 == cohortName2)
    
    ggplot(mat_df, aes(x = cohortName1, y = cohortName2, fill = n)) +
      geom_tile(data = subset(mat_df, !diagonal), color = "grey") +
      geom_tile(data = subset(mat_df, diagonal), fill = "black") +
      scale_fill_gradient(low = "white", high = mid_green) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Cohort 1", y = "Cohort 2", fill = "Count", title = "Cohort Overlap Heatmap")
  })
}

# ---------- RUN APP ----------
shinyApp(ui = ui, server = server)
