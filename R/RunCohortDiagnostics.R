#' @title Run Cohort Diagnostics
#' @description Executes CohortDiagnostics for all generated breast cancer cohorts.
#' @export
runCohortDiagnostics <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 cohortDefinitionSet,
                                 exportFolder) {
  library(CohortDiagnostics)
  runCohortDiagnostics(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortDefinitionSet = cohortDefinitionSet,
    exportFolder = exportFolder,
    minCellCount = 5
  )
}
