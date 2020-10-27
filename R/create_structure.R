#' Create folder structure
#'
#' This function creates folders inside your project folder to help organize your files
#' @examples
#' create_structure()
#' @export


# Create folder structure
create_structure <- function() {
  folders <- c("./01_documentation",
               "./01_documentation/01_planning_recourses",
               "./02_data",
               "./02_data/raw",
               "./02_data/z_photo_backups",
               "./03_code",
               "./04_outputs",
               "./05_analysis",
               "./05_analysis/manuscripts",
               "./05_analysis/reports"               )

  lapply(folders,
         function(path) if(!dir.exists(path)){
           dir.create(path, FALSE)})

  download.file(
    "https://github.com/MarcioFCMartins/MMartins/blob/master/workflow_files/clean_data.xlsx",
    "./02_data/clean_data.xlsx")
  download.file(
    "https://github.com/MarcioFCMartins/MMartins/blob/master/workflow_files/word_document2-template.docx",
    "./05_analysis/reports/word_document2-template.docx"
  )
}
