library(DatabaseConnector)
library(OMOPCancerIndicators)

# Create connection details (modify if needed)
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms           = Sys.getenv("DBMS"),
  server         = Sys.getenv("DB_SERVER"),
  port           = Sys.getenv("DB_PORT"),
  user           = Sys.getenv("DB_USER"),
  password       = Sys.getenv("DB_PASSWORD"),
  pathToDriver   = Sys.getenv("PATH_TO_DRIVER")
)

# Define database details (modify if needed)
cohortTable           <- "PBCR_breast_cancer_cohorts"
cdmDatabaseSchema     <- Sys.getenv("CDM_SCHEMA")
cohortDatabaseSchema  <- Sys.getenv("RESULTS_SCHEMA")

# Run the study
result <- runStudy(
  connectionDetails    = connectionDetails,
  cdmDatabaseSchema    = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable          = cohortTable,
  outputFolder         = outputFolder,
  year                 = 2019,
  gender               = 8532, # females only
  diagnosis_config     = "inst/settings/cancer_diagnosis.json",
  stage_config         = "inst/settings/cancer_stages.json",
  measurement_config   = "inst/settings/measurements.json",
  windowDays           = 30
)

# Launch interactive visualization
shiny::runApp(system.file("shiny/Diagnostics", package = "OMOPCancerIndicators"))

