#' This function is called by Rstudio when a Research project rproj template is created
#' Defines folder structure and creates a README
#' @importFrom utils download.file

research_project <- function(path, ...) {
  # Create folders -----------------------------------
  folders <- paste0(
    path,
    c("",
      "/docs",
      "/docs/planning-recourses",
      "/docs/financial",
      "/data",
      "/data/raw",
      "/data/clean",
      "/scripts",
      "/outputs",
      "/analysis",
      "/analysis/manuscripts",
      "/analysis/reports",
      "/photo-backups")
  )

  lapply(folders,
         function(path) if(!dir.exists(path)){
           dir.create(path, FALSE)})

  # Create README.txt ---------------------------------------
  writeLines(
    c("# Metadata",
      "What is this project?",
      "Who is involved?",
      "When did it start?",
      "",
      "# Journal",
      "What actions were taken to achieve the objectives and when?",
      "",
      "# Folder structure",
      "",
      "project-name",
      "|",
      "+-- project-name.Rproj",
      "|",
      "+-- docs\t\t\tLegal, financial and logistic documents",
      "|\t|",
      "|\t`-- planning-resources",
      "|",
      "+-- data\t\t\t\tData used to prepare the analysis and outputs",
      "|\t|",
      "|\t+-- raw\t\t\t\t\tData as collected - NOT ALTERED IN ANY WAY",
      "|\t|",
      "|\t+-- clean\t\t\t\tData that has been cleaned and corrected",
      "|\t|",
      "|\t+-- analysis\t\t\t\tProcessed, summarized or analyzed data",
      "|\t\t|",
      "|\t\t+-- *.csv\t\t\tData files ready for analysis",
      "|\t\t`-- *-README.txt\t\tData dictionary for data files",
      "|",
      "|",
      "+-- scripts\t\t\t\tScripts used for analysis ",
      "|\t|",
      "|\t+-- 0-process-data.R\t\t\tProcess raw data into clean",
      "|\t`-- k-*.R\t\t\t\tScripts for other processes",
      "|",
      "+-- outputs\t\t\t\tOutputs of scripts - figures, tables, models, etc",
      "|",
      "+-- analysis\t\t\t\tDocuments written about the data and outputs",
      "|\t|",
      "|\t+-- reports",
      "|\t|",
      "|\t`-- manuscript",
      "|\t\t|",
      "|\t \t`-- biblio.bib \t\t\tBibliography used in manuscript ",
      "|",
      "`-- photo-backups\t\t\tPhotographic backup of field tables or others"),
    paste0(path, "/README.txt")
  )

  # Download files -------------------------------------
  download.file(
    "https://github.com/MarcioFCMartins/MMartins/blob/master/workflow-files/metadata-sheet.xlsx?raw=true",
    paste0(path, "/02-data/metadata-template.xlsx")
  )
}
