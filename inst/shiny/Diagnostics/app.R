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

# Helper to add db column only when df exists
tag_db <- function(df, dbname) {
  if (is.null(df)) return(NULL)
  df$db <- dbname
  df
}

# Helper to merge OMOP + Source even if one side is missing
safe_bind <- function(df1, df2) {
  if (is.null(df1) && is.null(df2)) return(data.frame())   # return empty df
  if (is.null(df1)) return(df2)
  if (is.null(df2)) return(df1)
  dplyr::bind_rows(df1, df2)
}

# ---------- Load Data ----------
cohortCounts_omop       <- tag_db(safe_read_csv(file.path(resultsFolder, "omop",   "cohortCounts_omop.csv")), "OMOP")
cohortCounts_source     <- tag_db(safe_read_csv(file.path(resultsFolder, "source", "cohortCounts_source.csv")), "Source")

ageDistribution_omop    <- tag_db(safe_read_csv(file.path(resultsFolder, "omop",   "ageDistribution_omop.csv")), "OMOP")
ageDistribution_source  <- tag_db(safe_read_csv(file.path(resultsFolder, "source", "ageDistribution_source.csv")), "Source")

stage_omop_df           <- tag_db(safe_read_csv(file.path(resultsFolder, "omop",   "stage_omop.csv")), "OMOP")
stage_source_df         <- tag_db(safe_read_csv(file.path(resultsFolder, "source", "stage_source.csv")), "Source")

receptor_omop_df        <- tag_db(safe_read_csv(file.path(resultsFolder, "omop",   "measurements_omop.csv")), "OMOP")
receptor_source_df      <- tag_db(safe_read_csv(file.path(resultsFolder, "source", "measurements_source.csv")), "Source")

overlaps_omop           <- tag_db(safe_read_csv(file.path(resultsFolder, "omop",   "overlaps_omop.csv")), "OMOP")
overlaps_source         <- tag_db(safe_read_csv(file.path(resultsFolder, "source", "overlaps_source.csv")), "Source")

# ---------- Merge Datasets ----------
cohortCounts     <- safe_bind(cohortCounts_omop, cohortCounts_source)
ageDistribution  <- safe_bind(ageDistribution_omop, ageDistribution_source)
stage_df         <- safe_bind(stage_omop_df, stage_source_df)
receptor_df      <- safe_bind(receptor_omop_df, receptor_source_df)
overlaps         <- safe_bind(overlaps_omop, overlaps_source)

# ---------- Validation ----------
if (nrow(cohortCounts) == 0)
  stop("No cohortCounts input available (neither OMOP nor Source found).")

# ensure count column exists
if (!"n" %in% names(cohortCounts)) {
  candidate <- grep("count|Count|person", names(cohortCounts), value = TRUE)[1]
  if (!is.na(candidate)) cohortCounts <- cohortCounts %>% rename(n = !!sym(candidate))
  else stop("cohortCounts must contain a count column ('n' or recognizable equivalent).")
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
  dashboardHeader(title = "Cancer Indicators"),
  dashboardSidebar(width = 272,
                   sidebarMenu(
                     selectInput("cancer_type", "Select Cancer Type",
                                 choices = sort(unique(cohortCounts$cancer_type))),
                     menuItem("Overview", tabName = "overview", icon = icon("table")),
                     menuItem("Age", tabName = "age", icon = icon("chart-bar")),
                     menuItem("Stage", tabName = "stage", icon = icon("layer-group")),
                     menuItem("Measurements", tabName = "measurements", icon = icon("vials")),
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
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 6, title = "Cohort Counts (Table)", status = "primary", DTOutput("cohort_table")),
                box(width = 6, title = "Cohort Counts (Bar Chart)", status = "primary", plotOutput("cohort_bar", height = "420px"))
              )
      ),
      tabItem(tabName = "age",
              fluidRow(
                box(width = 4, title = "Filters", status = "info",
                    selectInput("age_cohort", "Select Cohort", choices = NULL)
                ),
                box(width = 8, title = "Age Distribution", status = "primary", plotOutput("age_plot", height = "420px"))
              ),
              fluidRow(box(width = 12, DTOutput("age_table")))
      ),
      tabItem(tabName = "stage",
              fluidRow(
                box(width = 2, title = "Display Metric", status = "info",
                    radioButtons("stage_metric", "Show:", choices = c("Percentage", "Count"), selected = "Percentage")
                ),
                box(width = 6, title = "Stage Distribution (Bar Chart)", status = "primary", plotOutput("stage_plot", height = "420px")),
                box(width = 4, title = "Stage Distribution (Table)", status = "primary", DTOutput("stage_table"))
              )
      ),
      tabItem(tabName = "measurements",
              fluidRow(
                box(width = 3, title = "Filters", status = "info",
                    radioButtons("measurement_metric", "Show:",
                                 choices = c("Percentage", "Count"), selected = "Percentage"),
                    
                    selectInput("measurement_type", "Measurement Type",
                                choices = c("All"), selected = "All")
                ),
                box(width = 9, title = "Measurement Distribution", status = "primary",
                    plotOutput("measurement_plot", height = "450px"))
              )
      ),
      
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
  
  # --- Reactive filtered datasets based on selected cancer type ---
  cancer_filtered <- reactive({
    req(input$cancer_type)
    
    list(
      cohortCounts = cohortCounts %>% 
        filter(cancer_type == input$cancer_type),
      
      ageDistribution = ageDistribution %>% 
        filter(cancer_type == input$cancer_type),
      
      stage_df = stage_df %>% 
        filter(cancer_type == input$cancer_type),
      
      receptor_df = {
        df <- receptor_df %>% filter(cancer_type == input$cancer_type)
        if (nrow(df) == 0) NULL else df
      },
      
      overlaps = overlaps %>% 
        filter(cancer_type == input$cancer_type)
    )
  })
  
  
  
  stage_data <- reactive({
    df <- cancer_filtered()$stage_df
    req(df)
    df$stage <- factor(df$stage, levels = c("0","1","2","3","4","N/A"))
    df
  })
  
  output$stage_plot <- renderPlot({
    df <- stage_data()
    if (nrow(df) == 0) {
      ggplot() + 
        annotate("text", x=0.5, y=0.5, label="No stage data", size=6) + 
        theme_void()
    } else {
      fill_colors <- c("OMOP"=omop_color, "Source"=source_color)
      if (input$stage_metric == "Count") {
        ggplot(df, aes(x=stage, y=n, fill=db)) +
          geom_col(position="dodge") +
          geom_text(aes(label=n), position=position_dodge(0.9), vjust=-0.5, color=main_green) +
          scale_fill_manual(values=fill_colors) +
          labs(x="Stage", y="Count", fill="Database", title=paste("Stage Distribution:", input$cancer_type)) +
          theme_minimal(base_size=13)
      } else {
        ggplot(df, aes(x=stage, y=percent, fill=db)) +
          geom_col(position="dodge") +
          geom_text(aes(label=percent), position=position_dodge(0.9), vjust=-0.5, color=main_green) +
          scale_fill_manual(values=fill_colors) +
          labs(x="Stage", y="Percentage", fill="Database", title=paste("Stage Distribution:", input$cancer_type)) +
          theme_minimal(base_size=13)
      }
    }
  })
  
  
  # --- Update cohort dropdown for Age tab dynamically ---
  observe({
    cohorts <- unique(cancer_filtered()$ageDistribution$cohortName)
    updateSelectInput(session, "age_cohort", choices = cohorts, selected = cohorts[1])
  })
  
  # ---- Overview ----
  output$cohort_table <- renderDT({
    df <- cancer_filtered()$cohortCounts %>% arrange(desc(n))
    datatable(df %>% select(Database = db, Cohort = cohortName, Count = n),
              options = list(pageLength = 12, scrollX = TRUE, rownames = FALSE))
  })
  
  output$cohort_bar <- renderPlot({
    df <- cancer_filtered()$cohortCounts %>% arrange(n)
    fill_colors <- c("OMOP" = omop_color, "Source" = source_color)
    ggplot(df, aes(x = reorder(cohortName, n), y = n, fill = db)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = fill_colors) +
      labs(x = "", y = "Count", fill = "Database", title = paste(input$cancer_type, "Cohort Sizes")) +
      theme_minimal(base_size = 13)
  })
  
  # ---- Age ----
  output$age_plot <- renderPlot({
    req(input$age_cohort)
    df <- cancer_filtered()$ageDistribution %>% filter(cohortName == input$age_cohort)
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No age data", size = 6) + theme_void()
    } else {
      df <- df %>% mutate(
        age_lower = as.numeric(sub("[-+].*$", "", ageGroup)),
        ageGroup = factor(ageGroup, levels = unique(ageGroup)[order(unique(age_lower))], ordered = TRUE)
      )
      fill_colors <- c("OMOP" = omop_color, "Source" = source_color)
      ggplot(df, aes(x = ageGroup, y = n, fill = db)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5, color = main_green) +
        scale_fill_manual(values = fill_colors) +
        labs(x = "Age Group", y = "Count", fill = "Database", title = paste("Age Distribution:", input$age_cohort)) +
        theme_minimal(base_size = 13) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$age_table <- renderDT({
    df <- cancer_filtered()$ageDistribution %>% filter(cohortName == input$age_cohort)
    df <- df %>% mutate(
      age_lower = as.numeric(sub("[-+].*$", "", ageGroup)),
      ageGroup = factor(ageGroup, levels = unique(ageGroup)[order(unique(age_lower))], ordered = TRUE)
    ) %>% arrange(ageGroup)
    datatable(df %>% select(Database = db, Cohort = cohortName, AgeGroup = ageGroup, Count = n),
              options = list(pageLength = 10, scrollX = TRUE, rownames = FALSE))
  })
  
  # ---- Stage ----
  output$stage_plot <- renderPlot({
    df <- cancer_filtered()$stage_df
    req(df)
    df$stage <- factor(df$stage, levels = c("0","1","2","3","4","N/A"))
    fill_colors <- c("OMOP" = omop_color, "Source" = source_color)
    if (input$stage_metric == "Count") {
      ggplot(df, aes(x = stage, y = n, fill = db)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, color = main_green) +
        scale_fill_manual(values = fill_colors) +
        labs(x = "Stage", y = "Count", fill = "Database", title = paste(input$cancer_type, "Stage Distribution")) +
        theme_minimal(base_size = 13)
    } else {
      ggplot(df, aes(x = stage, y = percent, fill = db)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = percent), position = position_dodge(width = 0.9), vjust = -0.5, color = main_green) +
        scale_fill_manual(values = fill_colors) +
        labs(x = "Stage", y = "Percentage", fill = "Database", title = paste(input$cancer_type, "Stage Distribution")) +
        theme_minimal(base_size = 13)
    }
  })
  
  output$stage_table <- renderDT({
    df <- cancer_filtered()$stage_df
    if (input$stage_metric == "Count") datatable(df %>% select(Database = db, Stage = stage, Count = n),
                                                 options = list(pageLength = 12, scrollX = TRUE, rownames = FALSE))
    else datatable(df %>% select(Database = db, Stage = stage, Percentage = percent),
                   options = list(pageLength = 12, scrollX = TRUE, rownames = FALSE))
  })
  
  # ---- Measurements ----
  
  observe({
    df <- cancer_filtered()$receptor_df
    if (is.null(df)) {
      updateSelectInput(session, "measurement_type",
                        choices = c("All"), selected = "All")
    } else {
      types <- sort(unique(df$measurement))
      updateSelectInput(session, "measurement_type",
                        choices = c("All", types),
                        selected = "All")
    }
  })
  
  
  output$measurement_plot <- renderPlot({
    df <- cancer_filtered()$receptor_df
    req(df)
    
    # --- No data available ---
    if (nrow(df) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No measurement data available",
                        size = 6) +
               theme_void())
    }
    
    # --- Filter by measurement type ---
    if (input$measurement_type != "All") {
      df <- df %>% filter(measurement == input$measurement_type)
      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No data for selected measurement",
                          size = 6) +
                 theme_void())
      }
    }
    
    # ---------- Color Palette Generator ----------
    generate_color_palette <- function(combo_levels) {
      
      source_tag <- sapply(strsplit(combo_levels, " - "), function(x) tail(x, 1))
      
      unique_sources <- unique(source_tag)
      palette <- character(length(combo_levels))
      
      # base palettes
      omop_base   <- c("#B22222", "#CD5C5C", "#F4A6A6")   # reds
      source_base <- c("#004d4d", "#007777", "#a7d7c5")  # greens
      
      for (src in unique_sources) {
        idx <- which(source_tag == src)
        n <- length(idx)
        
        if (grepl("omop", src, ignore.case = TRUE)) {
          colors <- colorRampPalette(omop_base)(n)
        } else if (grepl("source", src, ignore.case = TRUE)) {
          colors <- colorRampPalette(source_base)(n)
        } else {
          colors <- grey.colors(n, start = 0.4, end = 0.8)
        }
        
        palette[idx] <- colors
      }
      
      names(palette) <- combo_levels
      return(palette)
    }
    
    # ---------- Grouping Logic ----------
    # Only include meaningful grouping columns
    potential_groups <- intersect(
      c("value", "db", "subset"),   # expected fields
      names(df)
    )
    
    if (length(potential_groups) == 0) {
      df$combo <- factor("All")
    } else {
      df$combo <- apply(df[potential_groups], 1, paste, collapse = " - ")
      df$combo <- factor(df$combo)
    }
    
    # ---------- Generate Colors ----------
    palette <- generate_color_palette(levels(df$combo))
    
    # ---------- Plot ----------
    if (input$measurement_metric == "Count") {
      ggplot(df, aes(x = measurement, y = n, fill = combo)) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.4, size = 3.5) +
        scale_fill_manual(values = palette, name = "Group") +
        labs(x = "Measurement", y = "Count",
             title = paste(input$cancer_type, "- Measurement Distribution")) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      ggplot(df, aes(x = measurement, y = percent, fill = combo)) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_text(aes(label = sprintf("%.1f%%", percent)),
                  position = position_dodge(width = 0.9),
                  vjust = -0.4, size = 3.5) +
        scale_fill_manual(values = palette, name = "Group") +
        labs(x = "Measurement", y = "Percentage",
             title = paste(input$cancer_type, "- Measurement Distribution")) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  # ---- Overlaps ----
  output$overlap_heatmap <- renderPlot({
    df <- cancer_filtered()$overlaps
    req(df)
    cohorts <- sort(unique(c(df$cohortName1, df$cohortName2)))
    all_pairs <- expand.grid(cohortName1=cohorts, cohortName2=cohorts, stringsAsFactors = FALSE)
    mat_df <- df %>%
      group_by(cohortName1, cohortName2) %>%
      summarise(n=sum(n, na.rm=TRUE), .groups="drop") %>%
      right_join(all_pairs, by=c("cohortName1","cohortName2")) %>%
      mutate(n=ifelse(is.na(n),0,n), diagonal=cohortName1==cohortName2)
    ggplot(mat_df, aes(x=cohortName1, y=cohortName2, fill=n)) +
      geom_tile(data=subset(mat_df,!diagonal), color="grey") +
      geom_tile(data=subset(mat_df,diagonal), fill="black") +
      scale_fill_gradient(low="white", high=mid_green) +
      theme_minimal(base_size=12) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
      labs(x="Cohort 1", y="Cohort 2", fill="Count", title=paste(input$cancer_type,"Cohort Overlap"))
  })
}


# ---------- RUN APP ----------
shinyApp(ui = ui, server = server)
