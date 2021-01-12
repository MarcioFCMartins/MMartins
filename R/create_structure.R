#' This function is called by Rstudio when a Research project rproj template is created
#' Defines folder structure and creates a README
#'

research_project <- function(path, ...) {
  # Create folders
  folders <- paste0(
    path,
    c("",
      "/01-documentation",
      "/01-documentation/planning-recourses",
      "/02-data",
      "/02-data/raw",
      "/02-data/clean",
      "/02-data/photo-backups",
      "/03-scripts",
      "/04-outputs",
      "/05-analysis",
      "/05-analysis/manuscripts",
      "/05-analysis/reports")
  )

  lapply(folders,
         function(path) if(!dir.exists(path)){
           dir.create(path, FALSE)})

  writeLines(
    c("# Metadata", "What is this project?", "Who is involved?",
      "When did it start?", "", "# Journal", "What actions were taken to achieve the objectives and when?",
      "", "# Folder structure", "", "project-name", "|", "+-- project-name.Rproj",
      "|", "+-- 01-documentation\t\t\tLegal, financial and logistic documents\t  ",
      "|\t|", "|\t`-- planning-resources\t\t", "|", "+-- 02-data\t\t\t\t\t\t",
      "|\t|", "|\t+-- raw-data\t\t\t\tCollected data, as written/collected",
      "|\t|", "|\t+-- clean-data              Processed data for analysis",
      "|\t|\t|", "|\t|\t+-- *.csv\t\t\t\t\tData files ready for analysis",
      "|\t|\t`-- *-README.txt\t\t\tData dictionary for data files",
      "|\t|", "|\t`-- photo_backups\t\t\tPhotographic backup of field tables or others",
      "|", "+-- 03-scripts\t\t\t\t\tScripts used for analysis ", "|\t|",
      "|\t+-- 0-process-data.R\t\t\tProcess raw data into clean", "|\t`-- k-*.R\t\t\t\t\t\tScripts for other processes",
      "|", "+-- 04-outputs\t\t\t\t\tOutputs of scripts - figures, tables, models, etc",
      "|", "+-- 05-analysis\t\t\t\t\t", "|\t|", "|\t+-- reports\t\t\t\t\t",
      "|\t|", "|\t+-- manuscript", "|\t|\t|", "|\t|\t`-- biblio.bib \t\t\t\tBibliography used in manuscript "),
    paste0(path, "/README.txt")
  )

  # Create README.txt



}
