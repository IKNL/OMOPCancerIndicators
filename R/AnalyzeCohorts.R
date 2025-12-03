

#' Summarize cohort counts and attach cancer_type + subset via lookup
#' @param connectionDetails DatabaseConnector connection details
#' @param cohortDatabaseSchema target schema name
#' @param cohortTable cohort table name
#' @param cohortDefinitionSet data.frame with at least cohortId and cohortName

#' @return data.frame with cohortId, n, cohortName, cancer_type, subset, category
#' @export
summarizeCohortCounts <- function(connectionDetails,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  lookup) {
  library(SqlRender)
  library(DatabaseConnector)
  library(dplyr)
  
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
  if ("cohort_id" %in% names(counts)) names(counts)[names(counts) == "cohort_id"] <- "cohortId"
  
  # Merge cohort names (left join by cohortId)
  cohortCounts <- counts %>%
    left_join(lookup, by = "cohortId")
  
  # # Optionally write CSV
  # if (!is.null(write_csv)) {
  #   tryCatch({
  #     write.csv(cohortCounts, file = write_csv, row.names = FALSE)
  #   }, error = function(e) {
  #     warning("Failed to write cohort counts CSV to ", write_csv, ": ", e$message)
  #   })
  # }
  
  return(as.data.frame(cohortCounts))
}


#' Generate cohort lookup table from cohortDefinitionSet/cohortCounts
#' @param cohortDef Data frame containing at least cohortId and cohortName (cohortDefinitionSet)
#' @return data.frame with cohortId, cohortName, cancer_type, subset, category
#' @export
generateCohortLookup <- function(cohortDef) {
  library(dplyr)
  library(stringr)
  
  if (is.null(cohortDef) || nrow(cohortDef) == 0) return(data.frame())
  
  # Ensure cohortName exists
  if (!"cohortName" %in% names(cohortDef)) stop("cohortDef must contain 'cohortName'")
  
  df <- cohortDef %>%
    mutate(cohortName = as.character(cohortName)) %>%
    # normalize whitespace
    mutate(cohortName_clean = str_squish(cohortName))
  
  # cancer_type: everything before first "_" (preserve original case but also provide lowercase)
  df <- df %>%
    mutate(
      cancer_type = ifelse(
        grepl("_", cohortName_clean),
        sub("_.*$", "", cohortName_clean),
        cohortName_clean
      ),
      subset = ifelse(
        grepl("_", cohortName_clean),
        sub("^[^_]+_", "", cohortName_clean),
        ""
      )
    )
  
  # category: total (ends with _all), stage (subset contains 'stage'), else measurement
  df <- df %>%
    mutate(
      category = case_when(
        grepl("_all$", cohortName_clean, ignore.case = TRUE) ~ "total",
        grepl("stage", subset, ignore.case = TRUE) ~ "stage",
        subset == "" ~ "total",
        TRUE ~ "measurement"
      )
    ) %>%
    # make consistent format
    mutate(
      cancer_type = as.character(cancer_type),
      subset = as.character(subset),
      category = as.character(category),
      cancer_type_lower = tolower(cancer_type)
    ) %>%
    select(-cohortName_clean)
  
  # Return essential columns (keep cohortId if present)
  if ("cohortId" %in% names(df)) {
    df <- df %>% select(cohortId, cohortName, cancer_type, cancer_type_lower, subset, category)
  } else {
    df <- df %>% select(cohortName, cancer_type, cancer_type_lower, subset, category)
  }
  
  return(as.data.frame(df))
}




#' Summarize age distribution with customizable bin size
#' @export
summarizeAgeDistribution <- function(connectionDetails, 
                                     cdmDatabaseSchema, 
                                     cohortDatabaseSchema, 
                                     cohortTable, 
                                     cohortDefinitionSet,
                                     lookup,
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
  expected_ids <- unique(lookup$cohortId)
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
  agg <- merge(agg, lookup, by = "cohortId", all.x = TRUE)
  
  # Final output
  result <- agg[, c("cohortId", "ageGroup", "n", "cohortName", "cancer_type")]

  return(result)
}


#' Summarize stage distribution from CohortCounts
#' @export
summarizeStageDistribution <- function(CohortCounts) {
  library(dplyr)
  
  # Expect columns
  if (!all(c("cancer_type", "subset", "n", "category") %in% names(CohortCounts))) {
    stop("CohortCounts must contain cancer_type, subset, n, category")
  }
  
  # Extract stage rows
  stage_df <- CohortCounts %>%
    filter(category == "stage") %>%
    mutate(stage = gsub("(?i).*stage[_ ]?([0-9]+).*", "\\1", subset, perl = TRUE))
  
  # Extract true totals
  total_df <- CohortCounts %>%
    filter(category == "total") %>%
    select(cancer_type, total = n)
  
  # Summarize staged counts
  stage_summary <- stage_df %>%
    group_by(cancer_type, stage) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  
  # Compute missing ("Not available")
  missing_df <- stage_summary %>%
    group_by(cancer_type) %>%
    summarise(staged_n = sum(n), .groups = "drop") %>%
    left_join(total_df, by = "cancer_type") %>%
    mutate(
      total = ifelse(is.na(total), 0, total),
      n = pmax(total - staged_n, 0),
      stage = "Not available"
    ) %>%
    select(cancer_type, stage, n)
  
  # Combine known + missing
  final_stage <- bind_rows(stage_summary, missing_df) %>%
    left_join(total_df, by = "cancer_type") %>%
    mutate(
      percent = ifelse(total > 0, round(n / total * 100, 1), NA_real_)
    )
  
  # Order stage labels
  stage_levels <- c(as.character(0:9), "Not available")
  
  final_stage %>%
    mutate(stage = factor(stage, levels = stage_levels)) %>%
    arrange(cancer_type, stage) %>%
    select(cancer_type, stage, n, total, percent)
}





#' Summarize measurements from CohortCounts
#' @export
summarizeMeasurements <- function(CohortCounts) {
  library(dplyr)
  
  df <- CohortCounts %>% filter(category == "measurement")
  if (nrow(df) == 0) return(data.frame())
  
  df <- df %>% mutate(
    measurement = gsub("(_.*$)", "", subset),
    value   = sub("^[^_]+_", "", subset)
  )
  
  # True population totals
  total_df <- CohortCounts %>%
    filter(category == "total") %>%
    select(cancer_type, total = n)
  
  # Known measurement values
  known <- df %>%
    left_join(total_df, by = "cancer_type") %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      percent = ifelse(total > 0, round(n / total * 100, 1), NA_real_)
    ) %>%
    select(cancer_type, measurement, value, n, percent, total)
  
  # Sum known per measurement
  known_sum <- known %>%
    group_by(cancer_type, measurement) %>%
    summarise(
      known_n = sum(n),
      total = first(total),
      .groups = "drop"
    )
  
  # Unknown = total - sum(known)
  unknown <- known_sum %>%
    mutate(
      n = pmax(total - known_n, 0),
      percent = ifelse(total > 0, round(n / total * 100, 1), NA_real_),
      value = "Unknown"
    ) %>%
    select(cancer_type, measurement, value, n, percent)
  
  # Final combined table
  bind_rows(
    known %>% select(-total),
    unknown
  ) %>%
    arrange(cancer_type, measurement, value)
}



#' Summarize pairwise overlaps between cohorts
#' @export
summarizeOverlap <- function(connectionDetails, cohortDatabaseSchema, cohortTable, lookup) {
  library(dplyr)
  library(SqlRender)
  library(DatabaseConnector)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  results_list <- list()
  counter <- 1
  
  # Loop per cancer type
  for (ct in unique(lookup$cancer_type)) {
    
    ct_df <- lookup %>% filter(cancer_type == ct)
    
    # If only 1 cohort exists, nothing to compare
    if (nrow(ct_df) < 2) next
    
    # Compute pairwise combinations WITHIN cancer type
    for (i in 1:(nrow(ct_df) - 1)) {
      for (j in (i + 1):nrow(ct_df)) {
        
        c1 <- ct_df$cohortId[i]
        c2 <- ct_df$cohortId[j]
        
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
        overlapCount <- as.numeric(overlap$overlapcount[1])
        
        results_list[[counter]] <- data.frame(
          cancer_type = ct,
          cohortId1 = c1,
          cohortName1 = ct_df$cohortName[i],
          cohortId2 = c2,
          cohortName2 = ct_df$cohortName[j],
          n = overlapCount,
          stringsAsFactors = FALSE
        )
        counter <- counter + 1
      }
    }
  }
  
  results <- dplyr::bind_rows(results_list)
  return(results)
}
