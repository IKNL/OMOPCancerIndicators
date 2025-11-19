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

  # ----------------------------------------------------------------------
  # Helper function to create YEAR() expression for different DBMS
  # ----------------------------------------------------------------------
  getYearExpr <- function(connectionDetails, alias = "co") {
    expr <- switch(
      tolower(connectionDetails$dbms),
      "postgresql" = "EXTRACT(YEAR FROM {alias}.condition_start_date)",
      "redshift"   = "EXTRACT(YEAR FROM {alias}.condition_start_date)",
      "sql server" = "YEAR({alias}.condition_start_date)",
      "pdw"        = "YEAR({alias}.condition_start_date)",
      "oracle"     = "EXTRACT(YEAR FROM {alias}.condition_start_date)",
      stop("DBMS not supported for year extraction: ", connectionDetails$dbms)
    )
    gsub("\\{alias\\}", alias, expr)
  }

  # For measurement join referencing co2.*
  getYearExprMeasurement <- function(connectionDetails) {
    getYearExpr(connectionDetails, alias = "co2")
  }

  # ----------------------------------------------------------------------
  # Helper function to create interval windowdays expression for different DBMS
  # ----------------------------------------------------------------------
  getWindowExpr <- function(windowDays, connectionDetails) {
    dbms <- tolower(connectionDetails$dbms)
    if (dbms %in% c("postgresql", "redshift", "oracle")) {
      paste0("co.condition_start_date - INTERVAL '", windowDays, "' DAY",
            " AND co.condition_start_date + INTERVAL '", windowDays, "' DAY")
    } else if (dbms %in% c("sql server", "pdw")) {
      paste0("DATEADD(day, -", windowDays, ", co.condition_start_date)",
            " AND DATEADD(day, ", windowDays, ", co.condition_start_date)")
    } else {
      stop("DBMS not supported for windowDays expression: ", dbms)
    }
  }


  # ----------------------------------------------------------------------
  # Helper function to create stage measurement join
  # ----------------------------------------------------------------------
  makeStageMeasurementJoin <- function(year_expr_measurement,
                                       general_sql,
                                       clinical_sql,
                                       pathological_sql) {
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
              WHERE ancestor_concept_id IN ({general_sql})
                AND descendant_concept_id NOT IN (
                  SELECT descendant_concept_id
                  FROM @cdm_database_schema.concept_ancestor
                  WHERE ancestor_concept_id IN ({clinical_sql}, {pathological_sql})
                )
          ) allowed
            ON allowed.measurement_concept_id = m.measurement_concept_id
          WHERE {year_expr_measurement} IN (@year)
      ) m
        ON m.person_id = co.person_id
    ")
  }


  # ----------------------------------------------------------------------
  # Load configuration files
  # ----------------------------------------------------------------------
  diagnosis_all <- fromJSON(diagnosis_config)
  stages_config <- fromJSON(stage_config)
  measurements_all <- fromJSON(measurement_config)

  cancer_types <- names(diagnosis_all)
  cohortSql <- list()
  cohortNames <- c()
  cohortIds <- c()
  counter <- startCohortId


  # ----------------------------------------------------------------------
  # Template builder for each cohort
  # ----------------------------------------------------------------------
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


  # ----------------------------------------------------------------------
  # Loop over cancers
  # ----------------------------------------------------------------------

  for (cancer in cancer_types) {

    diag <- diagnosis_all[[cancer]]
    diagnosis_included <- paste(diag$included, collapse = ",")
    diagnosis_excluded <- paste(diag$excluded, collapse = ",")

    # ------------------------------------------------------------------
    # 1. Base ALL cohort
    # ------------------------------------------------------------------
    baseName <- glue("{cancer}_all")
    cohortNames <- c(cohortNames, baseName)
    cohortIds   <- c(cohortIds, counter)
    cohortSql[[baseName]] <- createTemplate(counter)
    counter <- counter + 1


    # ------------------------------------------------------------------
    # 2. Stage cohorts
    # ------------------------------------------------------------------
    if (all(c("general_ancestor", "pathological", "clinical") %in% names(stages_config))) {

      general_ancestor_stages <- stages_config$general_ancestor
      pathological_stages     <- stages_config$pathological
      clinical_stages         <- stages_config$clinical

      for (i in seq_along(general_ancestor_stages)) {

        stage_label <- names(general_ancestor_stages)[i]

        general_sql      <- paste(general_ancestor_stages[[i]], collapse = ",")
        clinical_sql     <- paste(clinical_stages[[i]], collapse = ",")
        pathological_sql <- paste(pathological_stages[[i]], collapse = ",")

        year_expr_measurement <- getYearExprMeasurement(connectionDetails)
        interval_expr <- getWindowExpr(windowDays, connectionDetails)

        stageJoin <- makeStageMeasurementJoin(year_expr_measurement, general_sql, clinical_sql, pathological_sql)

        extraWhere <- glue("
          AND m.measurement_date BETWEEN {interval_expr}
        ")

        stageName <- glue("{cancer}_{stage_label}")
        cohortNames <- c(cohortNames, stageName)
        cohortIds   <- c(cohortIds, counter)

        cohortSql[[stageName]] <- createTemplate(
          counter,
          extraJoin = stageJoin,
          extraWhere = extraWhere
        )
        counter <- counter + 1
      }
    }


    # ------------------------------------------------------------------
    # 3. Measurement-based cohorts
    # ------------------------------------------------------------------
    for (mName in names(measurements_all)) {

      mData <- measurements_all[[mName]]
      measurement_concepts <- mData$concept_id
      categories <- setdiff(names(mData), "concept_id")

      # --------------------------------------------------------------
      # No categories → simple presence cohort
      # --------------------------------------------------------------
      if (length(categories) == 0) {

        measurement_sql <- paste(measurement_concepts, collapse=",")

        extraJoin <- glue("
          INNER JOIN @cdm_database_schema.measurement m
            ON m.person_id = co.person_id
           AND m.measurement_concept_id IN ({measurement_sql})
           AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '{windowDays}' DAY
                                    AND co.condition_start_date + INTERVAL '{windowDays}' DAY
        ")

        mCohortName <- glue("{cancer}_{mName}_Any")
        cohortNames <- c(cohortNames, mCohortName)
        cohortIds   <- c(cohortIds, counter)
        cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
        counter <- counter + 1

      } else {

        # --------------------------------------------------------------
        # Category-specific measurement cohorts
        # --------------------------------------------------------------
        for (cat in categories) {
          valConcepts <- paste(mData[[cat]], collapse = ",")
          measurement_sql <- paste(measurement_concepts, collapse = ",")

          extraJoin <- glue("
            INNER JOIN @cdm_database_schema.measurement m
              ON m.person_id = co.person_id
             AND m.measurement_concept_id IN ({measurement_sql})
             AND m.value_as_concept_id IN ({valConcepts})
             AND m.measurement_date BETWEEN co.condition_start_date - INTERVAL '{windowDays}' DAY
                                      AND co.condition_start_date + INTERVAL '{windowDays}' DAY
          ")

          mCohortName <- glue("{cancer}_{mName}_{cat}")
          cohortNames <- c(cohortNames, mCohortName)
          cohortIds   <- c(cohortIds, counter)
          cohortSql[[mCohortName]] <- createTemplate(counter, extraJoin)
          counter <- counter + 1
        }
      }
    }
  }


  # ----------------------------------------------------------------------
  # Render SQL for each cohort
  # ----------------------------------------------------------------------
  renderedSql <- mapply(
    FUN = function(sql, i) {
      SqlRender::render(
        sql,
        cdm_database_schema = cdmDatabaseSchema,
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table = cohortTable,
        diagnosis_included = diagnosis_included,
        diagnosis_excluded = diagnosis_excluded,
        year = year,
        gender = gender,
        warnOnMissingParameters = FALSE
      )
    },
    sql = cohortSql,
    i = seq_along(cohortSql),
    SIMPLIFY = FALSE
  )


  # ----------------------------------------------------------------------
  # Build cohort definition set
  # ----------------------------------------------------------------------
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
