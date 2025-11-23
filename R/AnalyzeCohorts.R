#' Summarize cohort counts
#' @export
summarizeCohortCounts <- function(connectionDetails, cohortDatabaseSchema, cohortTable, cohortDefinitionSet) {
  library(SqlRender)
  library(DatabaseConnector)
  
  sql <- "
    SELECT cohort_definition_id AS cohort_id,
           COUNT(DISTINCT subject_id) AS n
    FROM @cohortDatabaseSchema.@cohortTable
    GROUP BY cohort_definition_id;
  "
  sql <- SqlRender::render(sql,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortTable = cohortTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  counts <- DatabaseConnector::querySql(conn, sql)
  names(counts) <- tolower(names(counts))
  
  # Rename column to match cohortDefinitionSet
  names(counts)[names(counts) == "cohort_id"] <- "cohortId"
  
  # Merge cohort names
  counts <- merge(counts,
                  cohortDefinitionSet[, c("cohortId", "cohortName")],
                  by = "cohortId",
                  all.x = TRUE)
  
  return(counts)
}

#' Summarize pairwise overlaps between cohorts
#' @export
summarizeOverlap <- function(connectionDetails, cohortDatabaseSchema, cohortTable, cohortDefinitionSet) {
  library(dplyr)
  library(SqlRender)
  library(DatabaseConnector)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  n <- nrow(cohortDefinitionSet)
  results_list <- list()
  counter <- 1
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      c1 <- cohortDefinitionSet$cohortId[i]
      c2 <- cohortDefinitionSet$cohortId[j]
      
      sql <- "
        SELECT COUNT(DISTINCT a.subject_id) AS overlapCount
        FROM @cohortDatabaseSchema.@cohortTable a
        INNER JOIN @cohortDatabaseSchema.@cohortTable b
          ON a.subject_id = b.subject_id
        WHERE a.cohort_definition_id = @id1
          AND b.cohort_definition_id = @id2;
      "
      
      rendered <- SqlRender::render(
        sql,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        id1 = c1,
        id2 = c2
      )
      translated <- SqlRender::translate(rendered, targetDialect = connectionDetails$dbms)
      
      overlap <- DatabaseConnector::querySql(conn, translated)
      names(overlap) <- tolower(names(overlap))
      
      overlapCount <- ifelse(nrow(overlap) == 0, 0, as.numeric(overlap$overlapcount[1]))
      
      results_list[[counter]] <- data.frame(
        cohortId1 = c1,
        cohortName1 = cohortDefinitionSet$cohortName[i],
        cohortId2 = c2,
        cohortName2 = cohortDefinitionSet$cohortName[j],
        n = overlapCount,
        stringsAsFactors = FALSE
      )
      counter <- counter + 1
    }
  }
  
  results <- dplyr::bind_rows(results_list)
  return(results)
}

#' Summarize age distribution with customizable bin size
#' @export
summarizeAgeDistribution <- function(connectionDetails, 
                                     cdmDatabaseSchema, 
                                     cohortDatabaseSchema, 
                                     cohortTable, 
                                     cohortDefinitionSet,
                                     ageBinSize = 10,
                                     collapseOldestAge = TRUE) {
  library(dplyr)
  library(SqlRender)
  library(DatabaseConnector)
  library(glue)
  
  # Helper function to retrieve age expression per DBMS
  getAgeExpr <- function(connectionDetails) {
    db <- tolower(connectionDetails$dbms)
    
    # Birth date fallback expression per DBMS
    birth_expr <- switch(
      db,
      "postgresql" = "COALESCE(p.birth_datetime, MAKE_DATE(p.year_of_birth, COALESCE(p.month_of_birth,6), COALESCE(p.day_of_birth,15)))",
      "redshift"   = "COALESCE(p.birth_datetime, MAKE_DATE(p.year_of_birth, COALESCE(p.month_of_birth,6), COALESCE(p.day_of_birth,15)))",
      "sql server" = "COALESCE(p.birth_datetime, DATEFROMPARTS(p.year_of_birth, ISNULL(p.month_of_birth,6), ISNULL(p.day_of_birth,15)))",
      "pdw"        = "COALESCE(p.birth_datetime, DATEFROMPARTS(p.year_of_birth, ISNULL(p.month_of_birth,6), ISNULL(p.day_of_birth,15)))",
      "oracle"     = "COALESCE(p.birth_datetime, TO_DATE(p.year_of_birth || '-' || NVL(p.month_of_birth,6) || '-' || NVL(p.day_of_birth,15),'YYYY-MM-DD'))",
      stop("DBMS not supported for age calculation")
    )
    
    # Full age expression per DBMS
    age_expr <- switch(
      db,
      "postgresql" = glue("EXTRACT(YEAR FROM AGE(c.cohort_start_date, {birth_expr}))"),
      "redshift"   = glue("EXTRACT(YEAR FROM AGE(c.cohort_start_date, {birth_expr}))"),
      "sql server" = glue(
        "DATEDIFF(YEAR, {birth_expr}, c.cohort_start_date) -
         CASE WHEN DATEADD(YEAR, DATEDIFF(YEAR, {birth_expr}, c.cohort_start_date), {birth_expr}) >
              c.cohort_start_date THEN 1 ELSE 0 END"
      ),
      "pdw"        = glue(
        "DATEDIFF(YEAR, {birth_expr}, c.cohort_start_date) -
         CASE WHEN DATEADD(YEAR, DATEDIFF(YEAR, {birth_expr}, c.cohort_start_date), {birth_expr}) >
              c.cohort_start_date THEN 1 ELSE 0 END"
      ),
      "oracle"     = glue("FLOOR(MONTHS_BETWEEN(c.cohort_start_date, {birth_expr})/12)")
    )
    
    return(age_expr)
  }
  
  # Retrieve age expression
  age_expr <- getAgeExpr(connectionDetails)
  
  sql <- glue("
    SELECT 
      c.cohort_definition_id AS cohort_id,
      {age_expr} AS age,
      c.subject_id,
      p.birth_datetime,
      p.month_of_birth,
      p.day_of_birth
    FROM @cohortDatabaseSchema.@cohortTable c
    JOIN @cdmDatabaseSchema.person p
      ON c.subject_id = p.person_id
  ")
  
  sql <- SqlRender::render(sql,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortTable = cohortTable)
  sql <- SqlRender::translate(sql, connectionDetails$dbms)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  age_data <- DatabaseConnector::querySql(conn, sql)
  names(age_data) <- tolower(names(age_data))
  
  # Diagnostics
  n_null_birth <- sum(is.na(age_data$birth_datetime))
  n_missing_month <- sum(is.na(age_data$month_of_birth))
  n_missing_day <- sum(is.na(age_data$day_of_birth))
  
  if (n_null_birth > 0) {
    warning(sprintf("[WARN] %s persons had NULL birth_datetime → reconstructed date used",
                    format(n_null_birth, big.mark=",")))
  }
  
  if (n_missing_month > 0 || n_missing_day > 0) {
    warning(sprintf(
      "[WARN] %s persons missing month/day → fallback values (month=6, day=15) used",
      format(n_missing_month + n_missing_day, big.mark=",")
    ))
  }
  
  age_data$age <- as.numeric(age_data$age)
  names(age_data)[names(age_data) == "cohort_id"] <- "cohortId"
  
  # Missing cohort check
  existing_ids <- unique(age_data$cohortId)
  expected_ids <- unique(cohortDefinitionSet$cohortId)
  missing <- setdiff(expected_ids, existing_ids)
  
  if (length(missing) > 0) {
    warning(sprintf("[WARN] Cohort IDs expected but not found: %s", paste(missing, collapse=", ")))
  }
  
  # Flexible binning
  createAgeGroups <- function(age, binSize = 10, collapseOld = TRUE, cutoff = 85) {
    
    maxAge <- 120
    
    if (collapseOld) {
      # Breaks below cutoff
      lower_breaks <- seq(0, cutoff, by = binSize)
      
      # Example with binSize=10 → 0,10,20,...,80,90? 
      # Ensure last break is exactly cutoff
      if (tail(lower_breaks, 1) != cutoff) {
        lower_breaks <- c(lower_breaks, cutoff)
      }
      
      # Build standard labels below cutoff
      lower_labels <- paste0(
        lower_breaks[-length(lower_breaks)], "-",
        lower_breaks[-1] - 1
      )
      
      # Add the final open-ended "85+"
      final_breaks  <- c(lower_breaks, Inf)
      final_labels  <- c(lower_labels, paste0(cutoff, "+"))
      
      grp <- cut(
        age,
        breaks = final_breaks,
        right = FALSE,
        labels = final_labels
      )
      
    } else {
      # Standard fixed-width bins up to maxAge
      breaks <- seq(0, maxAge, by = binSize)
      labels <- paste0(
        breaks[-length(breaks)], "-",
        breaks[-1] - 1
      )
      
      grp <- cut(
        age,
        breaks = c(breaks, Inf),
        right = FALSE,
        labels = c(labels, paste0(maxAge, "+"))
      )
    }
    
    return(droplevels(grp))
  }
  
  age_data$ageGroup <- createAgeGroups(age_data$age, ageBinSize, collapseOldestAge)
  
  # Aggregate
  agg <- aggregate(subject_id ~ cohortId + ageGroup, age_data, function(x) length(unique(x)))
  names(agg)[names(agg) == "subject_id"] <- "n"
  
  # Add names
  agg <- merge(agg, cohortDefinitionSet[, c("cohortId", "cohortName")], by = "cohortId", all.x = TRUE)
  
  # Final output
  result <- agg[, c("cohortId", "ageGroup", "n", "cohortName")]

  return(result)
}



#' Summarize cancer stage distribution (with missing stages) for all cancers
#' @export
summarizeStageDistribution <- function(cohortCounts) {
  library(dplyr)
  
  if (!all(c("cohortName", "n") %in% names(cohortCounts))) {
    stop("cohortCounts must contain 'cohortName' and 'n' columns.")
  }
  
  # 1) Identify stage-related cohorts and extract stage and cancer_type
  stage_df <- cohortCounts %>%
    filter(grepl("stage", cohortName, ignore.case = TRUE)) %>%
    mutate(
      # extract numeric stage (e.g., "stage0" -> "0")
      stage = gsub("(?i).*stage[_ ]?([0-9]+).*", "\\1", cohortName, perl = TRUE),
      # cancer_type is the prefix before "_stage..."
      cancer_type = sub("(?i)_stage.*$", "", cohortName, perl = TRUE)
    )
  
  # 2) Totals per cancer: find *_All cohorts and extract cancer_type the same way
  total_df <- cohortCounts %>%
    filter(grepl("_all$", cohortName, ignore.case = TRUE)) %>%
    mutate(cancer_type = sub("(?i)_all$", "", cohortName, perl = TRUE)) %>%
    select(cancer_type, total = n)
  
  # 3) Sum stage counts per cancer_type and stage
  stage_summary <- stage_df %>%
    group_by(cancer_type, stage) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  
  # 4) Compute missing ("Not available") per cancer_type = total - sum(staged)
  missing_df <- stage_summary %>%
    group_by(cancer_type) %>%
    summarise(stage_sum = sum(n, na.rm = TRUE), .groups = "drop") %>%
    left_join(total_df, by = "cancer_type") %>%
    mutate(
      total = ifelse(is.na(total), 0, total),
      n = pmax(total - stage_sum, 0),
      stage = "Not available"
    ) %>%
    select(cancer_type, stage, n)
  
  # 5) Combine and compute percent (n / total)
  final_stage <- bind_rows(stage_summary, missing_df) %>%
    left_join(total_df, by = "cancer_type") %>%
    mutate(
      total = ifelse(is.na(total), 0, total),
      percent = ifelse(total > 0, round(n / total * 100, 1), NA_real_)
    )
  
  # 6) Order stages sensibly: 0..4 then Not available
  stage_levels <- c(as.character(0:9), "Not available") # keep generic up to 9
  final_stage <- final_stage %>%
    mutate(stage = as.character(stage)) %>%
    arrange(cancer_type, factor(stage, levels = stage_levels))
  
  # final columns
  final_stage %>% select(cancer_type, stage, n, total, percent)
}



#' Summarize measurement/receptor positivity (including Unknown) for all cancers
#' @param cohortCounts Data frame returned from summarizeCohortCounts
#' @param measurement_config Path to measurement JSON file
#' @export
summarizeMeasurements <- function(cohortCounts, measurement_config) {
  library(dplyr)
  library(jsonlite)
  
  # Load all measurement definitions
  measurements_all <- fromJSON(measurement_config)
  measurement_df <- data.frame()
  
  # Loop through each measurement (e.g., ER, PR, HER2, PD-L1, etc.)
  for (mName in names(measurements_all)) {
    mData <- measurements_all[[mName]]
    categories <- setdiff(names(mData), "concept_id")  # e.g. positive, negative
    
    # Extract counts for each cancer × measurement × status
    temp_df <- data.frame()
    if (length(categories) > 0) {
      for (cat in categories) {
        temp <- cohortCounts %>%
          filter(grepl(paste0("_", mName, "_", cat), cohortName, ignore.case = TRUE)) %>%
          mutate(receptor = mName, status = cat)
        temp_df <- bind_rows(temp_df, temp %>% select(n, receptor, status, cohortName))
      }
    } else {
      temp <- cohortCounts %>%
        filter(grepl(paste0("_", mName, "_Any"), cohortName, ignore.case = TRUE)) %>%
        mutate(receptor = mName, status = "Any")
      temp_df <- bind_rows(temp_df, temp %>% select(n, receptor, status, cohortName))
    }
    
    measurement_df <- bind_rows(measurement_df, temp_df)
  }
  
  # Compute total counts per cancer type
  total_counts <- cohortCounts %>%
    filter(grepl("_all$", cohortName, ignore.case = TRUE)) %>%
    select(cohortName, n) %>%
    rename(total = n) %>%
    mutate(cancer_type = gsub("_all$", "", cohortName))
  
  # Add cancer type, merge totals
  measurement_df <- measurement_df %>%
    mutate(cancer_type = gsub("_.*", "", cohortName)) %>%
    left_join(total_counts, by = "cancer_type") %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    group_by(cancer_type, receptor) %>%
    mutate(percent = round(n / total * 100, 1)) %>%
    ungroup()
  
  # --- Add "Unknown" counts ---
  known_summary <- measurement_df %>%
    group_by(cancer_type, receptor) %>%
    summarise(known_n = sum(n, na.rm = TRUE),
              total = first(total),
              .groups = "drop")
  
  unknown_df <- known_summary %>%
    mutate(
      n = pmax(total - known_n, 0),
      percent = round(n / total * 100, 1),
      status = "Unknown"
    ) %>%
    select(cancer_type, receptor, status, n, percent)
  
  # Combine known and unknown into one final table
  measurement_df <- measurement_df %>%
    select(cancer_type, receptor, status, n, percent) %>%
    bind_rows(unknown_df) %>%
    arrange(cancer_type, receptor, match(status, c("Positive", "Negative", "Any", "Unknown")))
  
  return(measurement_df)
}

