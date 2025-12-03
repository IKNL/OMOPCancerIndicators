#' @title Create Multi-Cancer Cohort Definitions
#'
#' @description
#' Automatically generates multi-cancer cohort definitions (all, stage-based, and
#' measurement-based subcohorts) directly from JSON configuration files.
#'
#' @param cdmDatabaseSchema Character. OMOP CDM schema.
#' @param cohortDatabaseSchema Character. Target schema for cohort table.
#' @param cohortTable Character. Name of the cohort table.
#' @param connectionDetails DatabaseConnector connection details object.
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
    connectionDetails,
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
  library(DatabaseConnector)
  
  # ---------------------
  # DBMS helpers
  # ---------------------
  getYearExpr <- function(connectionDetails, alias = "co") {
    expr <- switch(
      tolower(connectionDetails$dbms),
      "postgresql" = "EXTRACT(YEAR FROM {alias}.condition_start_date)",
      "redshift"   = "EXTRACT(YEAR FROM {alias}.condition_start_date)",
      "sql server" = "YEAR({alias}.condition_start_date)",
      "pdw"        = "YEAR({alias}.condition_start_date)",
      "oracle"     = "EXTRACT(YEAR FROM {alias}.condition_start_date)",
      stop("Unsupported DBMS: ", connectionDetails$dbms)
    )
    gsub("\\{alias\\}", alias, expr)
  }
  

  # For measurement join referencing co2.*
  getYearExprMeasurement <- function(connectionDetails) {
    getYearExpr(connectionDetails, alias = "co2")
  }


  getWindowExpr <- function(windowDays, connectionDetails, alias = "co") {
    dbms <- tolower(connectionDetails$dbms)
    if (dbms %in% c("postgresql", "redshift", "oracle")) {
      paste0(alias, ".condition_start_date - INTERVAL '", windowDays, "' DAY AND ",
             alias, ".condition_start_date + INTERVAL '", windowDays, "' DAY")
    } else if (dbms %in% c("sql server", "pdw")) {
      paste0("DATEADD(day, -", windowDays, ", ", alias, ".condition_start_date) AND ",
             "DATEADD(day, ", windowDays, ", ", alias, ".condition_start_date)")
    } else {
      stop("Unsupported DBMS: ", dbms)
    }
  }
  
  
  # ----------------------------------------------------------------------
  # Helper function to create stage measurement join
  # ----------------------------------------------------------------------
  makeStageMeasurementJoin <- function(year_expr_measurement,
                                       included_sql,
                                       excluded_sql) {
    glue("
      LEFT JOIN (
          SELECT 
              m.person_id,
              m.measurement_date,
              m.measurement_concept_id
          FROM @cdm_database_schema.measurement m
          JOIN @cdm_database_schema.condition_occurrence co2
            ON m.person_id = co2.person_id
           AND m.measurement_event_id = co2.condition_occurrence_id
          JOIN (
              SELECT descendant_concept_id AS measurement_concept_id
              FROM @cdm_database_schema.concept_ancestor
              WHERE ancestor_concept_id IN ({included_sql})
                AND descendant_concept_id NOT IN (
                  SELECT descendant_concept_id
                  FROM @cdm_database_schema.concept_ancestor
                  WHERE ancestor_concept_id IN ({excluded_sql})
                )
          ) allowed
            ON allowed.measurement_concept_id = m.measurement_concept_id
          WHERE {year_expr_measurement} IN (@year)
      ) m
        ON m.person_id = co.person_id
    ")
  }
  
  
  
  # ---------------------
  # Load JSON configs
  # ---------------------
  diagnosis_all <- fromJSON(diagnosis_config)
  stage_all     <- fromJSON(stage_config)
  measurement_all <- fromJSON(measurement_config)
  
  cancer_types <- names(diagnosis_all)
  
  cohortSql <- list()
  cohortNames <- c()
  cohortIds <- c()
  counter <- startCohortId
  perCohort_params <- list()
  
  # ---------------------
  # Template for cohort creation
  # ---------------------
  createTemplate <- function(cohortId, extraJoin = "", extraWhere = "") {
    year_expr <- getYearExpr(connectionDetails)
    
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
      WHERE ca.ancestor_concept_id IN (@diagnosis_included)
        AND co.condition_concept_id NOT IN (@diagnosis_excluded)
        AND pe.gender_concept_id IN (@gender)
        AND {year_expr} IN (@year)
        {extraWhere};
    ")
  }
  
  # ---------------------
  # Loop through cancers
  # ---------------------
  for (cancer in cancer_types) {
    diag <- diagnosis_all[[cancer]]
    diag_included <- paste(diag$included, collapse = ",")
    diag_excluded <- ifelse(length(diag$excluded) == 0, "-1", paste(diag$excluded, collapse = ","))
    interval_expr <- getWindowExpr(windowDays, connectionDetails)
    year_expr_measurement <- getYearExprMeasurement(connectionDetails)
    
    # -------------------
    # 1. Base ALL-cohort
    # -------------------
    baseName <- glue("{cancer}_all")
    cohortNames <- c(cohortNames, baseName)
    cohortIds   <- c(cohortIds, counter)
    
    cohortSql[[baseName]] <- createTemplate(counter)
    perCohort_params[[baseName]] <- list(diagnosis_included = diag_included,
                                         diagnosis_excluded = diag_excluded)
    counter <- counter + 1
    
    # -------------------
    # 2. Stage cohorts
    # -------------------
    for (stageName in names(stage_all)) {
      stageDef <- stage_all[[stageName]]
      
      if (!is.null(stageDef$applies_to) && !(cancer %in% stageDef$applies_to)) next
      
      for (stage_label in names(stageDef$included)) {
        included_stage <- paste(stageDef$included[[stage_label]], collapse = ",")
        excluded_stage <- ifelse(length(stageDef$excluded[[stage_label]])==0, "-1",
                                 paste(stageDef$excluded[[stage_label]], collapse = ","))

        
        stageJoin <- makeStageMeasurementJoin(year_expr_measurement, included_stage, excluded_stage)
        
        extraWhere <- glue("AND m.measurement_date BETWEEN {interval_expr}")
        
        stageCohortName <- glue("{cancer}_{stage_label}")
        cohortNames <- c(cohortNames, stageCohortName)
        cohortIds   <- c(cohortIds, counter)
        
        cohortSql[[stageCohortName]] <- createTemplate(counter,
                                                       extraJoin = stageJoin,
                                                       extraWhere = extraWhere)
        perCohort_params[[stageCohortName]] <- list(diagnosis_included = diag_included,
                                                    diagnosis_excluded = diag_excluded)
        counter <- counter + 1
      }
    }
    
    # -------------------
    # 3. Measurement cohorts
    # -------------------
    for (mName in names(measurement_all)) {
      mData <- measurement_all[[mName]]
      if (!is.null(mData$applies_to) && !(cancer %in% mData$applies_to)) next
      
      hasConceptId <- "concept_id" %in% names(mData)


      # CASE 1: measurement has type concept + values 
      # ----------------------------
      if (hasConceptId) {
        measurement_concepts <- paste(mData$concept_id, collapse = ",")
        categories <- setdiff(names(mData), c("concept_id", "applies_to"))

        # No categories = “Any”
        if (length(categories) == 0) {
          extraJoin <- glue("
            INNER JOIN @cdm_database_schema.measurement m
              ON m.person_id = co.person_id
            AND m.measurement_concept_id IN ({measurement_concepts})
            AND m.measurement_date BETWEEN {interval_expr}
          ")

          mCohortName <- glue("{cancer}_{mName}_Any")
          cohortNames <- c(cohortNames, mCohortName)
          cohortIds   <- c(cohortIds, counter)
          cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
          perCohort_params[[mCohortName]] <- list(diagnosis_included = diag_included,
                                                  diagnosis_excluded = diag_excluded)
          counter <- counter + 1

        } else {
          # Categories = values (Positive, Negative, etc.)
          for (cat in categories) {
            valConcepts <- paste(mData[[cat]], collapse = ",")
            extraJoin <- glue("
              INNER JOIN @cdm_database_schema.measurement m
                ON m.person_id = co.person_id
              AND m.measurement_concept_id IN ({measurement_concepts})
              AND m.value_as_concept_id IN ({valConcepts})
              AND m.measurement_date BETWEEN {interval_expr}
            ")

            mCohortName <- glue("{cancer}_{mName}_{cat}")
            cohortNames <- c(cohortNames, mCohortName)
            cohortIds   <- c(cohortIds, counter)
            cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
            perCohort_params[[mCohortName]] <- list(diagnosis_included = diag_included,
                                                    diagnosis_excluded = diag_excluded)
            counter <- counter + 1
          }
        }


      # CASE 2: NO concept_id field - keys ARE measurement concepts
      # ----------------------------
      } else {
        categories <- setdiff(names(mData), "applies_to")

        for (cat in categories) {
          measurementConcepts <- paste(mData[[cat]], collapse = ",")

          extraJoin <- glue("
            INNER JOIN @cdm_database_schema.measurement m
              ON m.person_id = co.person_id
            AND m.measurement_concept_id IN ({measurementConcepts})
            AND m.measurement_date BETWEEN {interval_expr}
          ")

          mCohortName <- glue("{cancer}_{mName}_{cat}")
          cohortNames <- c(cohortNames, mCohortName)
          cohortIds   <- c(cohortIds, counter)
          cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
          perCohort_params[[mCohortName]] <- list(diagnosis_included = diag_included,
                                                  diagnosis_excluded = diag_excluded)
          counter <- counter + 1
        }
      }
    }

  }
  
  # -------------------
  # Render SQL
  # -------------------
  renderedSql <- list()
  for (nm in names(cohortSql)) {
    renderedSql[[nm]] <- SqlRender::render(
      cohortSql[[nm]],
      cdm_database_schema = cdmDatabaseSchema,
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      diagnosis_included = perCohort_params[[nm]]$diagnosis_included,
      diagnosis_excluded = perCohort_params[[nm]]$diagnosis_excluded,
      year = year,
      gender = gender,
      warnOnMissingParameters = FALSE
    )
  }
  
  # -------------------
  # Cohort definition set
  # -------------------
  cohortDefinitionSet <- data.frame(
    cohortId = cohortIds,
    cohortName = cohortNames,
    sql = unlist(renderedSql),
    json = "{}",
    stringsAsFactors = FALSE
  )
  
  # -------------------
  # Return
  # -------------------
  list(
    cohortSql = renderedSql,
    cohortDefinitionSet = cohortDefinitionSet
  )
}
