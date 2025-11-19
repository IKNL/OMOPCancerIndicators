#' @title Run Full Study
#'
#' @description
#' A wrapper function that orchestrates the full analysis pipeline:
#' cohort creation, generation, and diagnostics for breast cancer
#' in an OMOP CDM database.
#'
#' @details
#' This function first calls \code{\link{createCancerCohorts}} to build
#' all cohort definitions (main, stage, and receptor subcohorts), then runs
#' \code{\link{runCohortGeneration}} to instantiate these cohorts, and finally
#' executes \code{\link{runDiagnostics}} to obtain descriptive summaries.
#'
#' @param connectionDetails A DatabaseConnector connection details object.
#' @param cdmDatabaseSchema Schema name where the OMOP CDM resides.
#' @param cohortDatabaseSchema Schema name where the cohort tables will be written.
#' @param cohortTable Name of the results cohort table.
#' @param outputFolder Local path for writing output and diagnostic files.
#' @param year Integer. Diagnosis year to include.
#' @param gender Integer. OMOP concept ID for gender (e.g., 8532 = female).
#' @param diagnosis_config,stage_config,measurement_config Character. JSON configuration file paths.
#' @param windowDays Integer. Days before/after diagnosis to consider receptor or stage measurements at diagnosis. Default = 30.
#'
#' @return None. Side effects include cohort table creation and diagnostic outputs written to disk.
#' @export
runStudy <- function(connectionDetails,
                     cdmDatabaseSchema,
                     cohortDatabaseSchema,
                     cohortTable,
                     outputFolder,
                     year,
                     gender,
                     diagnosis_config,
                     stage_config,
                     measurement_config,
                     windowDays = 30) {

  message("Creating cancer cohort definitions...")
  cohorts <- createCancerCohorts(
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    connectionDetails = connectionDetails,
    year = year,
    gender = gender,
    diagnosis_config = diagnosis_config,
    stage_config = stage_config,
    measurement_config = measurement_config,
    windowDays = windowDays
  )

  message("Generating cohorts in the results schema...")
  runCohortGeneration(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohorts = cohorts,
    cohortTable = cohortTable,
    cohortDefinitionSet = cohorts$cohortDefinitionSet,
    incremental = FALSE
  )

  message("Cohorts generated successfully.")
  
  message("Analyze cohorts...")
  runDiagnostics(
    connectionDetails     = connectionDetails,
    cdmDatabaseSchema     = cdmDatabaseSchema,
    cohortDatabaseSchema  = cohortDatabaseSchema,
    cohortTable           = cohortTable,
    cohortDefinitionSet   = cohorts$cohortDefinitionSet
  )

  # Return cohortDefinitionSet for further use
  return(list(cohortDefinitionSet = cohorts$cohortDefinitionSet))
  }
