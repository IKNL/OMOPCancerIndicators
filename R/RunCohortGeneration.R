#' @title Run Cohort Generation
#' @description Instantiates all breast cancer cohorts in the target database.
#' @export
runCohortGeneration <- function(connectionDetails,
                                cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                cohortTable,
                                cohorts,
                                cohortDefinitionSet,
                                incrementalFolder = NULL) {
  library(CohortGenerator)


  # Step 2: Create cohort tables in results schema
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames
  )

  # Step 3: Generate all cohorts
  full_cohortDefinitionSet <- data.frame(
    cohortId = as.integer(cohorts$cohortDefinitionSet$cohortId),
    cohortName = as.character(cohorts$cohortDefinitionSet$cohortName),
    sql = as.character(cohorts$cohortDefinitionSet$sql),
    json = ifelse(is.null(cohorts$cohortDefinitionSet$json), NA, as.character(cohorts$cohortDefinitionSet$json)),
    stringsAsFactors = FALSE
  )

  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = full_cohortDefinitionSet,
    incremental = FALSE
  )



  # generateCohortSet(
  #   connectionDetails = connectionDetails,
  #   cdmDatabaseSchema = cdmDatabaseSchema,
  #   cohortDatabaseSchema = cohortDatabaseSchema,
  #   cohortTable = cohortTable,
  #   cohortDefinitionSet = cohortDefinitionSet,
  #   incrementalFolder = incrementalFolder
  # )

  # Step 4: Save SQL/JSON if desired
  # if (isTRUE(save_cohorts)) {
    saveCohortDefinitionSet(
      cohortDefinitionSet = cohorts$cohortDefinitionSet,
      settingsFileName = file.path(system.file(package="OMOPCancerIndicators"), "cohorts/settings/CohortsToCreate.csv"),
      jsonFolder = file.path(system.file(package="OMOPCancerIndicators"), "cohorts/json"),
      sqlFolder = file.path(system.file(package="OMOPCancerIndicators"), "cohorts/sql/sql_server")
    )
  # }

}
