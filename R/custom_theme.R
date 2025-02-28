#' A Custom ggplot2 Theme
#'
#' Sets up the basic look of my ggplot2 themes and abstracts some of the options
#' @param serif Changes all fonts to serif. Defaults to FALSE
#' @param grid Should plots contain grid lines at major breaks? Can be FALSE (default), "x", "y" or "xy"
#' @export

theme_custom <- function() {
    theme(
        text = element_text(colour = "#383838", size = 10),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
        # Facet headers (strips)
        strip.background = element_blank(),
        strip.text = element_text(size = 11, angle = 0, face = "bold"),
        strip.placement = "outside",
        # Panel where aesthetics are plotted
        panel.background = element_rect(fill = "#F9F9F9"),
        panel.border = element_rect(colour = "#757575", fill = NA),
        panel.grid.major.x = element_line(colour = "#b2b2b250", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "#b2b2b250", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        # Axis
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "#757575", linewidth = 0.5),
        axis.text = element_text(size = 9),
        axis.title = element_text(colour = "#282828", size = 11),
        # Legend
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.8, "line"),
        legend.text = element_text(size = 8, hjust = 0)
    )
}
