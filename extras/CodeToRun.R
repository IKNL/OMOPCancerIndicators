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
cohortTable           <- "PBCR_cancer_cohorts"
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
  gender               = c(8532), # females only
  diagnosis_config     = "inst/settings/cancer_diagnosis.json",
  stage_config         = "inst/settings/cancer_stages.json",
  measurement_config   = "inst/settings/measurements.json",
  windowDays           = 30,    # Number of days before and after diagnosis to look for measurements
  ageBinSize           = 10,    # set the size of age bins (e.g. 10 for 0-9, 10-19, etc. or 5 for 0-4, 5-9, etc.)
  collapseOldestAge    = FALSE  # set to TRUE to combine all ages above 85+
)

# Launch interactive visualization
shiny::runApp(system.file("shiny/Diagnostics", package = "OMOPCancerIndicators"))

