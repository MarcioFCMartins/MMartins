#' A Custom ggplot2 Theme
#'
#' Sets up the basic look of my ggplot2 themes and abstracts some of the options
#' @param serif Changes all fonts to serif. Defaults to FALSE
#' @param grid Should plots contain grid lines at major breaks? Can be FALSE (default), "x", "y" or "xy"
#' @export

theme_custom <- function(serif = FALSE, grid = FALSE, axis = "xy") {
  font_family <- ifelse(serif, "serif", "sans")

  grid_x  <-  if (grid == "x" | grid == "xy" | grid == "yx") {
    element_line(colour = "#bfbebe", linetype = "dashed")
  } else {
    element_blank()
  }

  grid_y  <-  if (grid == "y" | grid == "xy" | grid == "yx") {
    element_line(colour = "#bfbebe", linetype = "dashed")
  } else {
    element_blank()
  }

  axis_x  <-  if (axis == "x" | axis == "xy" | axis == "yx") {
    element_line(colour = "#bfbebe")
  } else {
    element_blank()
  }

  axis_y  <-  if (axis == "y" | axis == "xy" | axis == "yx") {
    element_line(colour = "#bfbebe")
  } else {
    element_blank()
  }

  theme(
    text = element_text(colour = "#646467", size = 11, family = font_family),
    plot.title         = element_text(size=13, hjust = 0, face = "bold"),
    plot.background    = element_blank(),
    strip.background   = element_blank(),
    strip.text         = element_text(colour = "#505050", angle=0, size = 12),
    strip.placement    = "outside",
    panel.background   = element_rect(fill="white"),
    #panel.spacing      = unit(c(15,15), "pt"),  This creates errors when many facets are used
    panel.border       = element_blank(),
    panel.grid.major.x = grid_x,
    panel.grid.major.y = grid_y,
    panel.grid.minor   = element_blank(),
    #axis.title.x       = element_text(margin = margin(5,0,0,0, "pt")),
    #axis.title.y       = element_text(margin = margin(0,5,0,0, "pt")),
    axis.line.x        = axis_x,
    axis.line.y        = axis_y,
    axis.ticks         = element_line(colour = "#bfbebe"),
    axis.text          = element_text(size = 10),
    plot.margin        = unit(c(1,1,0.5,0.5),"lines"),
    legend.background  = element_blank(),
    legend.position    = "top",
    legend.justification = "left",
    legend.margin      = margin(0,20,0,0,"pt"),
    legend.title       = element_text(face = "bold", hjust = 0, vjust = 1),
    legend.title.align = 0,
    legend.key         = element_blank(),
    legend.key.height  = unit(0.8, "line"),
    legend.text.align  = 0.5)
}
