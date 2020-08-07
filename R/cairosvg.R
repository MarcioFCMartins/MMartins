#' Cairo SVG for ggsave
#'
#' This function allows you use cairosvg with ggsave - generally gives higher quality results than default svg engine.
#' The function is not called directly, but within ggsave
#' @examples
#' ggsave("folder", plot ,device = cairosvg, width = 25, height = 10, units = "cm")
#' @export


# CairoSVG function for ggplot2 -------------------------------------------
# Solves some issues with standard svg device - mainly usage of symbols
cairosvg <- function(filename, width, height){
  library(Cairo)
  CairoSVG(file = filename, width = width, height = height)
}
