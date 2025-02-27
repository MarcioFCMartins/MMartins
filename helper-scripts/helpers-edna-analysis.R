library(tidyverse)
library(ggplot2)
library(patchwork)
library(janitor)
library(stringr)
library(ggsankey)
library(readxl)
library(vegan)
library(ggrepel)

# Function to order taxonomic factor levels to improve readability in ggsankey
# Levels are ordered based on observed number of sequences, but ensuring that
# the order of the parent taxonomic groups is maintained. I.e. levels are first
# ordered based on the "parent" taxonomic level and only then by sequence count.
# This minimizes the crossing of flow lines in the diagram and improves
# readability.
# @param df Name of data.frame with taxonomic information
# @param cols Character vector with names of columns containing taxonomic levels
# @param value Character name of the column holding the counts
fct_hierarchical_sort <- function(df, cols, value) {
    factor_levels <- c()
    previous_levels <- NULL
    current_levels <- NULL

    for (i in 1:length(cols)) {
        if (i == 1) {
            col1 <- cols[i]

            order <- df |>
                group_by(.data[[col1]]) |>
                summarise(total = sum(.data[[value]])) |>
                arrange(total) |>
                as.data.frame()

            current_levels <- order[, 1]
            previous_levels <- data.frame(
                "groups" = current_levels,
                "p_order" = c(1:length(current_levels))
            )

            factor_levels <- c(factor_levels, current_levels)
        } else {
            col1 <- cols[i - 1]
            col2 <- cols[i]

            order <- df |>
                group_by(.data[[col1]], .data[[col2]]) |>
                summarise(total = sum(.data[[value]]))

            order <- merge(
                order,
                previous_levels,
                by.x = col1,
                by.y = "groups"
            ) |>
                arrange(p_order, total) |>
                as.data.frame()

            current_levels <- order[, 2]
            previous_levels <- data.frame(
                "groups" = current_levels,
                "p_order" = c(1:length(current_levels))
            )

            factor_levels <- c(factor_levels, current_levels)
        }
    }

    return(unique(factor_levels))
}

# Function to assign a "common" name to all observations belonging
# to arbitrary "interest group" levels.
# @param taxa Data.frame with taxonomic groups where every column
# is a taxonomic level, with specificity increasing from left to right
# @param group_names Data.frame with 2 columns: taxon and new_name
taxa_to_common_group <- function(taxa, group_names) {
    # Return the highest level taxon for each observation
    # that is found in the interest taxon data.frame

    # Find the most specific taxonomic level matching an interest group
    highest_level_matching_taxon <- apply(
        taxa,
        1,
        \(x) ifelse(
            any(x %in% group_names$taxon),
            x[max(which(x %in% group_names$taxon))],
            NA
        )
    )

    # Get index for the target interest group
    new_group_indexes <- sapply(
        highest_level_matching_taxon,
        \(x) ifelse(
            !is.na(x),
            which(group_names$taxon %in% x),
            NA
        ),
        USE.NAMES = FALSE
    )

    new_groups <- group_names$new_name[new_group_indexes]

    return(new_groups)
}

# Filter taxonomy matrix by minimum boot confidence level
# @param tax Taxonomy classification table output by dada2
# @param boot Bootstrap confidence values for each taxonomic level - matrix output by dada2
# @param boot_conf Minimum confidence level to keep a taxonomic classification
# @param tax_levels Character vector with names of taxonomic levels (found in columns of tax)
tax_filter_min_confidence <- function(tax, boot, boot_conf = 50, tax_levels = NULL) {
    # Taxonomic levels must be provided
    stopifnot(!is.null(tax_levels))

    boot_reject <- boot[, tax_levels] < boot_conf
    tax[, tax_levels][boot_reject] <- NA
    # Check if any NAs was stored as string
    tax[, tax_levels][tax[, tax_levels] == "NA"] <- NA

    return(tax)
}

tax_matrix_to_long <- function(tax) {
    # Pivot all samples into long format and add sample information
    tax_long <- pivot_longer(
        tax,
        cols = 1:which(names(tax) == "sequence") - 1,
        names_to = "dna_lab_id"
    ) |>
        # Because sample names were stored in columns, an X was added to samples with numerical IDs
        mutate(dna_lab_id = str_replace(dna_lab_id, "X", ""))
}

plot_filtering_diagnostics <- function(sequence_count_track, sequence_mat) {
    track_long <- sequence_count_track |>
        pivot_longer(
            cols = input:nonchim
        ) |>
        mutate(
            name = factor(name, levels = names(sequence_evol))
        )

    unique_per_sample <- data.frame(
        "dna_lab_id" = rownames(sequence_mat),
        "unique_count" = rowSums(sequence_mat > 0)
    ) |>
        left_join(samples)

    (
        ggplot(track_long) +
            geom_line(
                aes(
                    x = name,
                    y = log10(value),
                    group = sample_id
                )
            ) +
            labs(title = "Evolution of sequence number per step") +
            theme_bw()
    ) + (
        (
            sequence_evol |>
                mutate(total_loss = (input - nonchim) / input) |>
                ggplot() +
                geom_point(aes(x = top_depth_cm, y = total_loss)) +
                labs(title = "Discarded sequences vs depth") +
                theme_bw()
        ) / (
            ggplot(unique_per_sample) +
                geom_point(aes(x = depth, y = unique_count)) +
                labs(title = "Unique sequence N vs depth") +
                theme_bw()
        )
    ) /
        (
            (
                sequence_evol |>
                    mutate(is_blank = ifelse(is.na(core_id), T, F)) |>
                    ggplot() +
                    geom_boxplot(aes(x = is_blank, y = nonchim)) +
                    labs(title = "N seq reads") +
                    theme_bw()
            ) + (
                ggplot(unique_per_sample) +
                    geom_boxplot(aes(x = blank, y = unique_count)) +
                    labs(title = "N seq uniques") +
                    theme_bw()
            )
        )
}

plot_classification_success_per_level <- function(tax, tax_levels = NULL) {
    # Taxonomic levels must be provided
    stopifnot(!is.null(tax_levels))

    last_sample_column <- which(names(tax) == "sequence") - 1

    nodes <- tax |>
        rowwise() |>
        mutate(total = sum(c_across(1:last_sample_column))) |>
        ungroup() |>
        select(sequence:total) |>
        mutate(
            across(all_of(tax_levels),
                .fns = \(x) ifelse(is.na(x), "NA", "Known")
            )
        ) |>
        ggsankey::make_long(!!tax_levels, value = total)

    ggplot(nodes, aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = factor(node),
        value = value
    )) +
        geom_sankey() +
        labs(x = "Taxonomic group") +
        scale_fill_manual(
            name = "Classification",
            values = c("Known" = "darkgreen", "NA" = "red")
        ) +
        theme_sankey(base_size = 16)
}

plot_relative_taxonomic_classifications <- function(tax,
                                                    tax_levels,
                                                    target_level,
                                                    top_n = NULL) {
    # Taxonomic levels must be provided
    stopifnot(!is.null(tax_levels))

    last_sample_column <- which(names(tax) == "sequence") - 1
    first_tax_level_column <- which(names(tax) == "sequence") + 1

    grouping_levels <- tax_levels[1:(which(tax_levels == target_level))]

    tax_long <- tax |>
        group_by(core_id, depth, across(all_of(grouping_levels))) |>
        summarise(total = sum(value)) |>
        ungroup()

    if (!is.null(top_n)) {
        tax_long$tax_level <- forcats::fct_lump_n(tax_long[[target_level]], top_n, w = tax_long$total)
    } else {
        tax_long$tax_level <- tax_long[[target_level]]
    }

    tax_long |>
        ggplot() +
        geom_col(
            aes(x = depth, y = total, fill = tax_level),
            position = "fill"
        ) +
        facet_grid(
            cols = vars(core_id),
            scales = "free_x",
            drop = TRUE
        ) +
        coord_flip() +
        scale_x_reverse()
}



# Perform NMDS to view clustering of smaples ------------------------------
# library(vegan)

# # https://jkzorz.github.io/2020/04/04/NMDS-extras.html
# nmds_loadings <- envfit(tax_mds, tax_vegan_rel)
#
#
# nmds_loadings_ggplot <- data.frame(
#     nmds_loadings$vectors$arrows,
#     "r" = nmds_loadings$vectors$r,
#     "p" = nmds_loadings$vectors$pvals
# ) |>
#     rownames_to_column("OTU") |>
#     filter(r > 0.2) |>
#     rowwise() |>
#     mutate(
#         theta = atan2(NMDS2, NMDS1),
#         x = r * cos(theta),
#         y = r * sin(theta)
#     ) |>
#     mutate(
#         OTU = str_replace(OTU, "(\\d+)", ""),
#         OTU = str_replace_all(OTU, "\\.+", " "),
#         OTU = str_trim(OTU),
#         OTU = factor(OTU, levels = unique(OTU))
#     )
#
# nmds_points <- tax_mds$points |>
#     as_tibble(rownames = "samples") |>
#     mutate(core_id = toupper(substr(samples, 1, 4))) |>
#     left_join(cores, by = "core_id")
#
#
# group_polygons <- nmds_points |>
#     group_by(sites_notes) |>
#     # chull will say which rows comprise the convex polygon
#     # and slice will extract those rows
#     slice(chull(MDS1, MDS2))
#
# site_labels <- c(
#     "Ponta do Adoxe" = "Nearby (PA)",
#     "Ria Formosa"    = "Donor (RF)",
#     "Arrábida 10yr"  = "Restored-10yr (AR10)",
#     "Arrábida 3yr"   = "Restored-3yr"
# )
#
# ggplot() +
#     geom_point(
#         data = nmds_points,
#         aes(x = MDS1, y = MDS2, fill = sites_notes, shape = sites_notes),
#     ) +
#     geom_polygon(
#         data = group_polygons,
#         aes(x = MDS1, y = MDS2, fill = sites_notes),
#         alpha = .15
#     ) +
#     geom_segment(
#         data = nmds_loadings_ggplot,
#         aes(x = 0, y = 0, xend = x, yend = y, color = OTU),
#         key_glyph = "rect"
#     ) +
#     geom_text_repel(
#         data = nmds_loadings_ggplot,
#         aes(x = x, y = y, label = OTU, color = OTU),
#         max.overlaps = 30
#     ) +
#     scale_shape_manual(
#         labels = site_labels,
#         values = c(21:24),
#         name = "Samples meadows"
#     ) +
#     scale_fill_discrete(
#         labels = site_labels
#     ) +
#     scale_color_manual(
#         values = group_colors
#     ) +
#     labs(
#         color = "Biological group",
#         fill = "Samples meadows"
#     )
