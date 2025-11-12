#' Run diagnostics for cohorts
#' @export
runDiagnostics <- function(connectionDetails,
                                cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                cohortTable,
                                outputFolder = "Results",
                                measurement_config = "inst/settings/measurements.json",
                                cohortDefinitionSet) {

  message("▶ Summarizing cohort counts...")
  cohortCounts <- summarizeCohortCounts(connectionDetails,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortDefinitionSet = cohortDefinitionSet)

  message("▶ Summarizing age distribution...")
  ageDistribution <- summarizeAgeDistribution(connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortTable,
                                              cohortDefinitionSet = cohortDefinitionSet)
  message("▶ Summarizing overlaps...")
  overlaps <- summarizeOverlap( connectionDetails,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 cohortDefinitionSet = cohortDefinitionSet)
  
  message("▶ Summarizing stage...")
  stage <- summarizeStageDistribution(cohortCounts)
  
  message("▶ Summarizing measurements...")
  measurements <- summarizeMeasurements(cohortCounts, measurement_config)

  # Ensure output folder exists
  if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)

  # Write CSVs
  utils::write.csv(cohortCounts, file.path(outputFolder, "omop","cohortCounts_omop.csv"), row.names = FALSE)
  utils::write.csv(ageDistribution, file.path(outputFolder, "omop", "ageDistribution_omop.csv"), row.names = FALSE)
  utils::write.csv(overlaps, file.path(outputFolder, "omop","overlaps_omop.csv"), row.names = FALSE)
  utils::write.csv(stage, file.path(outputFolder, "omop", "stage_omop.csv"), row.names = FALSE)
  utils::write.csv(measurements, file.path(outputFolder, "omop", "measurements_omop.csv"), row.names = FALSE)
  
  # --- Save path for Shiny app ---
  appDir <- system.file("shiny/Diagnostics", package = "OMOPCancerIndicators")
  pathFile <- file.path(appDir, "resultsPath.txt")
  writeLines(normalizePath(outputFolder), pathFile)
  message("Diagnostics results folder saved for Shiny app: ", normalizePath(outputFolder))

  # message("Done. Results saved to: ", outputFolder)

  invisible(list(cohortCounts = cohortCounts, ageDistribution = ageDistribution, overlaps = overlaps))
}
