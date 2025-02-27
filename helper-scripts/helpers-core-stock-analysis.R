## Helper functions for BLUEFOREST cores analysis ##
##          Márcio Martins    2024-11-19          ##
##  email     marciomartinsred@gmail.com          ##
## Github     MarcioFCMartins                     ##

ordered_core_names <- c(
    "BF04-B", "BF05-D", "BF06-A",
    "SEDMARII ARRABIDA VC01.02", "SEDMARII ARRABIDA VC02", "SEDMARII ARRABIDA VC03",
    "BF01-C", "BF02-A", "BF03-E"
)


# Load core data ----------------------------------------------------------

load_core_data <- function() {
    data <- read_xlsx(
        here("data/analysis/sediment-cores.xlsx"),
        sheet = "sediment_cores",
        skip = 3,
        na = "NA"
    )

    data <- data |>
        dplyr::mutate(
            core_id = factor(core_id, levels = ordered_core_names),
            site = factor(site, levels = c("Sado", "Ria Formosa")),
            across(latitude:mar_error, \(x) as.numeric(x))
        )

    return(data)

}

# Load analysis ready data ------------------------------------------------
load_sediment_data <- function() {
    data <- read_xlsx(
        here("data/analysis/sediment-data.xlsx"),
        sheet = "sediment_properties",
        skip = 3,
        na = "NA"
    )

    data <- data |>
        dplyr::mutate(
            # Sites will be analysed from North to South, so order factors the same way
            core_id = factor(
                core_id,
                levels = ordered_core_names
            ),
            site = factor(site, levels = c("Sado", "Ria Formosa")),
            sample_id = as.character(sample_id),
            across(depth_min:MS1, \(x) as.numeric(x))
        )

    return(data)
}

# Load eDNA data in long format -------------------------------------------
load_taxonomy_data <- function() {
    data <- readRDS(here("data/analysis/taxonomy-classification-long-format.rds"))

    return(data)
}

# Aesthetics used for core plots ------------------------------------------
core_aes <- list(
    "linetype" = setNames(
        c(
            "solid", "dashed", "dotted",
            "solid", "dashed", "dotted",
            "solid", "dashed", "dotted"
        ),
        ordered_core_names
    ),
    "color" = setNames(
        c(
            "#d44450", "#d44450", "#d44450",
            "#540303", "#540303", "#540303",
            "#2d6283", "#2d6283", "#2d6283"
        ),
        ordered_core_names
    )
)

# Final core labels -------------------------------------------------------
plot_labels <- c(
    "BF04-B" = "BF04-B",
    "BF05-D" = "BF05-D",
    "BF06-A" = "BF06-A",
    "SEDMARII ARRABIDA VC01.02" = "VC01.02",
    "SEDMARII ARRABIDA VC02" = "VC02",
    "SEDMARII ARRABIDA VC03" = "VC03",
    "BF01-C" = "BF01-C",
    "BF02-A" = "BF02-A",
    "BF03-E" = "BF03-E",
    "Ria Formosa" = "Ria Formosa",
    "Sado"   = "Sado"
)


# Aesthetics used for site labels -----------------------------------------
site_aes <- list(
    "color" = setNames(
        c("#2d6283", "#b50606"),
        c("Sado", "Ria Formosa")
    )
)


# Function to calculate stock up to chosen X ------------------------------
calculate_stocks <- function(data, groups, x, y, max_x) {
    x_data <- x
    y_data <- y
    
    stocks <- data |>
        filter(!is.na(.data[[y_data]])) |>
        group_by(pick(all_of(groups))) |>
        nest() |>
        mutate(
            stock = map_dbl(
                .x = data,
                .f = \(df) {
                    MESS::auc(
                        x = df[, x_data, drop = T], 
                        y = df[, y_data, drop = T], 
                        from = 0, 
                        to = max_x, 
                        type = "linear",
                        rule = 2
                    )
                }
            )
        )
    
    return(stocks)
}


# Function to calculate cumulative stocks along a column -----------------

calculate_cumulative_stocks <- function(data, groups, x, y) {
    x_data <- x
    y_data <- y
    
    stocks <- data |>
        filter(!is.na(.data[[y_data]])) |>
        group_by(pick(all_of(groups))) |>
        nest() |>
        mutate(
            data = map(
                .x = data,
                .f = \(df) {
                    cum_stocks <- numeric(length = nrow(df))
                    for(i in 1:nrow(df)){
                        cum_stocks[i] <- MESS::auc(
                            x = df[, x_data, drop = T], 
                            y = df[, y_data, drop = T], 
                            from = 0, 
                            to = df[i, x_data, drop = T], 
                            type = "linear",
                            rule = 2
                        )
                    }
                    
                    df$cum_stock <- cum_stocks
                    
                    return(df)
                }
            )
        )
    
    stocks <- unnest(stocks, cols = data) |>
        ungroup()
    
    return(stocks)
}



# Function to calculate sample weights  -----------------------------------
# Calculate the height of the core fraction represented by a sample to assign weights
# Weight_n = (Depth_n - Depth_n-1)/2 + (Depth_n+1 - Depth_n)/2
#   Except for the first and last samples
#   where the first and last parts (respectively) are not divided by 2
calculate_sample_weights <- function(depth_middle, depth_max) {
    top_gap <- depth_middle - lag(depth_middle, default = 0)
    bottom_gap <- lead(depth_middle, default = max(depth_max)) - depth_middle
    weights <- case_when(
        depth_middle == 0.5 ~ top_gap + bottom_gap/2,
        depth_middle == max(depth_middle) ~ top_gap/2+bottom_gap,
        TRUE ~ top_gap/2 + bottom_gap/2
    )
    
    return(weights)
}


# Paste value +- error ----------------------------------------------------
paste_with_error <- function(value, error, digits = 2) {
    
    values <- matrix(
        c(value, error),
        ncol = 2)
    
    apply(
        values,
        1,
        function(x){
            value_ <- x[1]
            error_ <- x[2]
            if(is.na(value_)) return("")
            
            if(is.na(error_)) return(round(value_, digits))
            
            return(paste0(
                round(value_, digits),
                " ± ",
                round(error_, digits)
            ))
        }
    )
    
}

# ggplot2 theme -----------------------------------------------------------
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
