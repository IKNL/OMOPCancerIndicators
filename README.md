# OMOP Cancer Indicators

**OMOPCancerIndicators** is an R package for automated generation, and analysis of cancer cohorts and indicators from OMOP CDM databases.  

This package facilitates reproducible cancer cohort creation, including **stage-specific cohorts, measurement-based subcohorts**, and comprehensive cohort summarization (counts, overlaps, age distributions, stage, receptor status, and measurement positivity). These results can be compared to counts from the source data to validate the data conversion and visualized in a Shiny app.

---

## Features

- Generate **(multi) cancer cohorts** directly from **JSON configuration files**:
  - Cancer diagnosis concept IDs
  - Staging information (general, clinical, pathological)
  - User-defined measurement categories
- Create **subcohorts** based on stage or measurement results
- Summarize cohort counts with descriptive names
- Compute **pairwise overlaps** between cohorts
- Summarize **age distribution** in conventional 10-year bins
- Summarize **cancer stage distribution** including missing/unknown stages
- Summarize **receptor and measurement positivity**, including unknowns
- Compatible with OHDSI `CohortGenerator`, `DatabaseConnector` and `SqlRender` for PostgreSQL, SQL Server, and other OMOP-supported databases

---

## Folder Structure

```
OMOPCancerIndicators/
├── R/
│   ├── AnalyzeCohorts.R
|   ├── CreateCancerCohorts.R
│   ├── RunCohortGeneration.R
│   ├── RunCohortDiagnostics.R
|   ├── RunDiagnostics.R
│   └── RunStudy.R
├── inst/
│   ├── cohorts/
│   |   ├── json/
│   |   ├── sql/
│   |   └── settings/
|   ├── settings/
│   |   ├── cancer_diagnosis.json
│   |   ├── cancer_stages.json
│   |   └── measurements.json
|   └── shiny/
├── extras/
│   └── codeToRun.R
├── man/
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## Installation

Clone the repository or download the zip folder, extract it in the file location and then install the package using devtools. This allows you to make changes directly without reinstalling the package. If you edit the script, do not forget to call `devtools::load_all()` first, otherwise the changes are not loaded.

```r
# Install devtools if not already installed
install.packages("devtools")

# Install package from local folder
devtools::install("path/to/OMOPCancerIndicators")
```
---

## Usage
Once the package is installed, open the `extras/CodeToRun.R` and modify the connectionDetails, cdmDatabaseSchema and CohortDatabaseSchema if needed. Run this file and inspect the results in the Shiny app.

**Disclaimer:** The package has only been tested on a PostgreSQL database.

### Customization and usage of individual functions
#### 1. Configuration Files

The concepts that are included in the analyses are stored in individual JSON files. These can be extended or modified if different concepts are used in your database

**cancer_diagnosis.json**
The ancestor concept for primary malignant breast cancer is included, meaning that all descendants are included. The list of excluded concepts are a one to one match (so no descendants) and these are excluded from the selection.
**cancer_stages.json** 
The script is now written to choose the highest mapping for each stage, while excluding the specific pathological and clinical stages as this creates multiple records in the NCR. This may need to be adjusted if the logic is different in other databases.
**measurements.json**
These are the receptor measurement

#### 2. Generate Cancer Cohorts
```r
library(OMOPCancerIndicators)
cohorts <- createCancerCohorts(
  cdmDatabaseSchema = "omopcdm",
  cohortDatabaseSchema = "results",
  cohortTable = "cancer_cohorts",
  year = 2019,
  gender = 8532,
  diagnosis_config = "inst/settings/cancer_diagnosis.json",
  stage_config = "inst/settings/cancer_stages.json",
  measurement_config = "inst/settings/measurements.json",
  windowDays = 30,
  startCohortId = 1
)
```

#### 3. Run Cohort Generation
```r
runCohortGeneration(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "omopcdm",
  cohortDatabaseSchema = "results",
  cohorts = cohorts,
  cohortTable = "cancer_cohorts",
  cohortDefinitionSet = cohorts$cohortDefinitionSet
)
```

#### 4. Summarize Cohorts
```r
counts <- summarizeCohortCounts(
  connectionDetails,
  cohortDatabaseSchema = "results",
  cohortTable = "cancer_cohorts",
  cohortDefinitionSet = cohorts$cohortDefinitionSet
)
overlaps <- summarizeOverlap(
  connectionDetails,
  cohortDatabaseSchema = "results",
  cohortTable = "cancer_cohorts",
  cohortDefinitionSet = cohorts$cohortDefinitionSet
)
age_summary <- summarizeAgeDistribution(
  connectionDetails,
  cdmDatabaseSchema = "omopcdm",
  cohortDatabaseSchema = "results",
  cohortTable = "cancer_cohorts",
  cohortDefinitionSet = cohorts$cohortDefinitionSet
)
stage_summary <- summarizeStageDistribution(counts)
measurement_summary <- summarizeMeasurements(counts, measurement_config = "inst/settings/measurements.json")
```

---

## Output

The main outputs saved in the Results/ folder. 

- The **OMOP analysis** is saved to the Results/omop folder:
  - cohortcounts_omop.csv
  - overlaps_omop.csv
  - ageDistribution_omop.csv
  - stage_omop.csv
  - measurements_omop.csv
- The **source data** needs to be added manually to the Results/source folder. This folder already contains the template files with counts set to 0 (as a placeholder). Update these counts to compare the OMOP extraction with your source data. In case modifications are made to the cohort structure, the structure of these files may need to adjusted to match the OMOP generated results.
  - cohortcounts_source.csv
  - ageDistribution_source.csv
  - stage_source.csv
  - measurements_source.csv


## Visualization

The results can be visualized using the Shiny app. 


## License

This project is licensed under the MIT License. 

## Contact

For questions:  
**Maaike van Swieten**  
📧 m.vanswieten@iknl.nl
🔗 GitHub: [MaaikevS](https://github.com/MaaikevS)

