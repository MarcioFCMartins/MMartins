% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cairosvg.R
\name{cairosvg}
\alias{cairosvg}
\title{Cairo SVG for ggsave}
\usage{
cairosvg(filename, width, height)
}
\description{
This function allows you use cairosvg with ggsave - generally gives higher quality results than default svg engine.
The function is not called directly, but within ggsave
}
\examples{
ggplot2::ggsave("folder", plot ,device = cairosvg, width = 25, height = 10, units = "cm")
}
