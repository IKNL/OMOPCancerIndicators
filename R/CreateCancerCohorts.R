#' @title Create Multi-Cancer Cohort Definitions
#'
#' @description
#' Automatically generates multi-cancer cohort definitions (all, stage-based, and
#' measurement-based subcohorts) directly from JSON configuration files.
#'
#' @param cdmDatabaseSchema Character. OMOP CDM schema.
#' @param cohortDatabaseSchema Character. Target schema for cohort table.
#' @param cohortTable Character. Name of the cohort table.
#' @param year Integer. Diagnosis year.
#' @param gender Integer. OMOP concept ID for gender.
#' @param diagnosis_config,stage_config,measurement_config Character. JSON configuration file paths.
#' @param windowDays Integer. Measurement window around diagnosis date.
#' @param startCohortId Integer. Starting cohort ID.
#'
#' @return List with cohortSql and cohortDefinitionSet.
#' @export
createCancerCohorts <- function(
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable,
    year,
    gender,
    diagnosis_config = "inst/settings/cancer_diagnosis.json",
    stage_config = "inst/settings/cancer_stages.json",
    measurement_config = "inst/settings/measurements.json",
    windowDays = 30,
    startCohortId = 1
) {
  library(SqlRender)
  library(CohortGenerator)
  library(jsonlite)
  library(glue)
  
  # -----------------------------
  # Load configurations
  # -----------------------------
  diagnosis_all <- fromJSON(diagnosis_config)
  stages_config <- fromJSON(stage_config)
  measurements_all <- fromJSON(measurement_config)
  
  cancer_types <- names(diagnosis_all)
  cohortSql <- list()
  cohortNames <- c()
  cohortIds <- c()
  counter <- startCohortId
  
  # -----------------------------
  # Helper function to build SQL
  # -----------------------------
  createTemplate <- function(cohortId, extraJoin = "", extraWhere = "") {
    glue("
      INSERT INTO @cohort_database_schema.@cohort_table
        (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
      SELECT DISTINCT
        {cohortId} AS cohort_definition_id,
        co.person_id AS subject_id,
        co.condition_start_date AS cohort_start_date,
        co.condition_start_date AS cohort_end_date
      FROM @cdm_database_schema.condition_occurrence co
      JOIN @cdm_database_schema.person pe
        ON co.person_id = pe.person_id
      JOIN @cdm_database_schema.concept_ancestor ca
        ON ca.descendant_concept_id = co.condition_concept_id
      {extraJoin}
      WHERE ca.ancestor_concept_id IN @diagnosis_included
        AND co.condition_concept_id NOT IN @diagnosis_excluded
        AND pe.gender_concept_id in (@gender)
        AND EXTRACT(YEAR FROM co.condition_start_date) in (@year)
        {extraWhere};
    ")
  }
  
  # -----------------------------
  # Loop over all cancers
  # -----------------------------
  for (cancer in cancer_types) {
    diag <- diagnosis_all[[cancer]]
    diagnosis_included <- if (length(diag$included) > 0)
      paste0("(", paste(diag$included, collapse=","), ")") else "(-1)"
    diagnosis_excluded <- if (length(diag$excluded) > 0)
      paste0("(", paste(diag$excluded, collapse=","), ")") else "(-1)"
    
    # --- Base (All) cohort ---
    baseName <- glue("{cancer}_all")
    cohortNames <- c(cohortNames, baseName)
    cohortIds <- c(cohortIds, counter)
    cohortSql[[baseName]] <- createTemplate(counter)
    counter <- counter + 1
    
    # --- Stage cohorts (with hierarchical exclusions)
    if (all(c("general_ancestor", "pathological", "clinical") %in% names(stages_config))) {
      general_ancestor_stages <- stages_config$general_ancestor
      pathological_stages <- stages_config$pathological
      clinical_stages <- stages_config$clinical
      
      for (i in seq_along(general_ancestor_stages)) {
        stage_label <- names(general_ancestor_stages)[i]
        
        general_sql <- if (length(general_ancestor_stages[[i]]) > 0)
          paste0(paste(general_ancestor_stages[[i]], collapse = ",")) else "(-1)"
        clinical_sql <- if (length(clinical_stages[[i]]) > 0)
          paste0(paste(clinical_stages[[i]], collapse = ",")) else "(-1)"
        pathological_sql <- if (length(pathological_stages[[i]]) > 0)
          paste0(paste(pathological_stages[[i]], collapse = ",")) else "(-1)"
        
        extraJoin <- glue("
          JOIN (
            WITH included_measurements AS (
              SELECT descendant_concept_id AS measurement_concept_id
              FROM @cdm_database_schema.concept_ancestor
              WHERE ancestor_concept_id IN ({general_sql})
            ),
            excluded_measurements AS (
              SELECT descendant_concept_id AS measurement_concept_id
              FROM @cdm_database_schema.concept_ancestor
              WHERE ancestor_concept_id IN ({clinical_sql}, {pathological_sql})
            ),
            allowed_measurements AS (
              SELECT measurement_concept_id
              FROM included_measurements
              WHERE measurement_concept_id NOT IN (SELECT measurement_concept_id FROM excluded_measurements)
            )
            SELECT m.person_id, m.measurement_date, m.measurement_concept_id
            FROM @cdm_database_schema.measurement m
            JOIN @cdm_database_schema.condition_occurrence co2
              ON co2.person_id = m.person_id
             AND m.measurement_event_id = co2.condition_occurrence_id
            JOIN allowed_measurements am
              ON am.measurement_concept_id = m.measurement_concept_id
            WHERE EXTRACT(YEAR FROM co2.condition_start_date) in (@year)
          ) m
          ON m.person_id = co.person_id
        ")
        
        extraWhere <- glue("
          AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '{windowDays}' DAY
          AND co.condition_start_date + INTERVAL '{windowDays}' DAY
        ")
        
        stageName <- glue("{cancer}_{stage_label}")
        cohortNames <- c(cohortNames, stageName)
        cohortIds <- c(cohortIds, counter)
        cohortSql[[stageName]] <- createTemplate(counter, extraJoin, extraWhere)
        counter <- counter + 1
      }
    }
    
    # --- Measurements (biomarkers, lab tests, etc.) ---
    for (mName in names(measurements_all)) {
      mData <- measurements_all[[mName]]
      measurement_concepts <- mData$concept_id
      categories <- setdiff(names(mData), "concept_id") # e.g. positive/negative/high/low
      
      # If no categories, just build one cohort for presence of measurement
      if (length(categories) == 0) {
        mCohortName <- glue("{cancer}_{mName}_Any")
        measurement_sql <- paste0("m.measurement_concept_id IN (", paste(measurement_concepts, collapse=","), ")")
        
        extraJoin <- glue("
          INNER JOIN @cdm_database_schema.measurement m
            ON m.person_id = co.person_id
            AND {measurement_sql}
            AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '{windowDays}' DAY
            AND co.condition_start_date + INTERVAL '{windowDays}' DAY
        ")
        cohortNames <- c(cohortNames, mCohortName)
        cohortIds <- c(cohortIds, counter)
        cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
        counter <- counter + 1
      } else {
        # Multiple category-based subcohorts (user-defined)
        for (cat in categories) {
          valConcepts <- mData[[cat]]
          measurement_sql <- paste0("m.measurement_concept_id IN (", paste(measurement_concepts, collapse=","), ")")
          val_sql <- paste0("m.value_as_concept_id IN (", paste(valConcepts, collapse=","), ")")
          
          mCohortName <- glue("{cancer}_{mName}_{cat}")
          extraJoin <- glue("
            INNER JOIN @cdm_database_schema.measurement m
              ON m.person_id = co.person_id
              AND {measurement_sql}
              AND {val_sql}
              AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '{windowDays}' DAY
              AND co.condition_start_date + INTERVAL '{windowDays}' DAY
          ")
          cohortNames <- c(cohortNames, mCohortName)
          cohortIds <- c(cohortIds, counter)
          cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
          counter <- counter + 1
        }
      }
    }
  }
  
  # -----------------------------
  # Render all SQLs
  # -----------------------------
  renderedSql <- lapply(cohortSql, function(sql) {
    SqlRender::render(
      sql,
      cdm_database_schema = cdmDatabaseSchema,
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      year = year,
      gender = gender,
      diagnosis_included = diagnosis_included,
      diagnosis_excluded = diagnosis_excluded
    )
  })
  
  # -----------------------------
  # Build cohort definition set
  # -----------------------------
  cohortDefinitionSet <- data.frame(
    cohortId = cohortIds,
    cohortName = cohortNames,
    sql = unlist(renderedSql),
    json = "{}",
    stringsAsFactors = FALSE
  )
  
  list(
    cohortSql = renderedSql,
    cohortDefinitionSet = cohortDefinitionSet
  )
}
