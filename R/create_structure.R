#' Create folder structure
#'
#' This function creates folders inside your project folder to help organize your files
#' @examples
#' create_structure()
#' @export


# Create folder structure
create_structure <- function() {
  folders <- c("./1_documentation",
               "./2_planning_recourses",
               "./3_data",
               "./3_data/raw",
               "./4_code",
               "./5_outputs",
               "./6_manuscripts",
               "./z_photo_backups")

  lapply(folders,
         function(path) if(!dir.exists(path)){
           dir.create(path, FALSE)})

  download.file(
    "https://github.com/MarcioFCMartins/MMartins/raw/master/project_structure/3_data/clean_data.xlsx",
    "./3_data/clean_data.xlsx")
}
