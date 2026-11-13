# Study-characteristics figures used in the manuscript and supplement.
rm(list = ls()); gc()
req <- c("readr","dplyr","stringr","tidyr","ggplot2","ragg","UpSetR","grid")
to_install <- req[!vapply(req, requireNamespace, logical(1), quietly=TRUE)]
if(length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only=TRUE))
out_dir <- file.path("analysis","outputs","manuscript","Figures")
for (folder in c("Main","Supplementary")) dir.create(file.path(out_dir,folder),recursive=TRUE,showWarnings=FALSE)
dat <- read_csv("analysis/inputs/Study_Characteristics.csv",show_col_types=FALSE) |> filter(!is.na(id))

palette_vals <- c(
    "Threshold / Cutpoint" = "#6BA3D6",
    "Heuristic Rule" = "#87B5E1",
    "Linear / GLM" = "#A6CEE3",
    "Non-linear / Mixed Statistical" = "#B2DF8A",
    "Classical ML" = "#FDBF6F",
    "Probabilistic Temporal" = "#FF7F00",
    "Hybrid / Ensemble" = "#9467BD",
    "Deep Learning" = "#E31A1C"
)

p_year_method <- dat |>
    filter(!is.na(method_best)) |>
    count(year, method_best) |>
    ggplot(aes(x = factor(year), y = n, fill = method_best)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = palette_vals, drop = FALSE) +
    labs(x = "Publication year", y = "Number of studies", fill = "Modelling approach") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(out_dir, "Supplementary", "Figure_S1_600dpi.png"),
       p_year_method, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

save_upset <- function(
        data, set_col, file_name, location = "Supplementary",
        sets_max = 6, n_intersects = 30, sets_label = "Studies per set",
        width_in = 10, height_in = 8,
        set_size_num_size = 5.5,
        set_size_pad = 1.40,
        text_scale = c(1.9, 1.6, 1.8, 1.5, 1.8, 2.0),
        point_size = 3.2,
        line_size = 0.9,
        mainbar_label_shift_in = 0
) {

    long <- data |>
        tidyr::separate_rows({{ set_col }}, sep = ";\\s*") |>
        dplyr::mutate(v = stringr::str_trim({{ set_col }})) |>
        dplyr::filter(!is.na(v), v != "") |>
        dplyr::distinct(id, v)

    wide <- long |>
        dplyr::mutate(present = 1L) |>
        tidyr::pivot_wider(names_from = v, values_from = present, values_fill = 0L) |>
        as.data.frame(check.names = FALSE)

    set_names <- setdiff(names(wide), "id")
    freq <- colSums(wide[, set_names, drop = FALSE])
    ord <- names(sort(freq, decreasing = TRUE))
    sets_for_plot <- rev(ord[seq_len(min(length(ord), sets_max))])

    set_size_limit <- ceiling(max(freq[sets_for_plot], na.rm = TRUE) * set_size_pad / 5) * 5

    png_file <- file.path(out_dir, location, file_name)

    ragg::agg_png(
        filename = png_file,
        width = width_in,
        height = height_in,
        units = "in",
        res = 600,
        background = "white"
    )

    on.exit(dev.off(), add = TRUE)

    up <- UpSetR::upset(
        wide,
        sets = sets_for_plot,
        keep.order = TRUE,
        nintersects = n_intersects,
        order.by = "freq",
        mb.ratio = c(0.65, 0.35),
        point.size = point_size,
        line.size = line_size,
        mainbar.y.label = "Number of studies",
        sets.x.label = sets_label,
        show.numbers = "yes",
        number.angles = 0,
        set_size.show = TRUE,
        set_size.numbers_size = set_size_num_size,
        set_size.angles = 0,
        set_size.scale_max = set_size_limit,
        text.scale = text_scale
    )
    
    # Position the upper y-axis title without moving the panels or axes.
    label_index <- which(up$Main_bar$layout$name == "ylab-l")
    label <- up$Main_bar$grobs[[label_index]]$children[[1]]
    label$x <- label$x + grid::unit(mainbar_label_shift_in, "inches")
    up$Main_bar$grobs[[label_index]]$children[[1]] <- label
    print(up)

}

save_upset(
    dat, age_group, "Figure_S3_600dpi.png",
    mainbar_label_shift_in = 696 / 600,
    sets_max = 10,
    sets_label = "Studies per age group",
    width_in = 10,
    height_in = 8,
    set_size_num_size = 7,
    set_size_pad = 1.45,
    text_scale = c(1.9, 1.6, 1.8, 1.5, 1.8, 2.0),
    point_size = 3.2,
    line_size = 0.9
)

save_upset(
    dat, outcomes, "Figure_S2_600dpi.png",
    mainbar_label_shift_in = 719 / 600,
    sets_max = 10,
    sets_label = "Studies per outcome",
    width_in = 10,
    height_in = 8,
    set_size_num_size = 7,
    set_size_pad = 1.45,
    text_scale = c(1.9, 1.6, 1.8, 1.5, 1.8, 2.0),
    point_size = 3.2,
    line_size = 0.9
)

save_upset(
    dat, device_brand, "Figure_2_600dpi.png", location = "Main",
    mainbar_label_shift_in = 1838 / 600,
    sets_max = 30,
    n_intersects = 30,
    sets_label = "Studies per device",
    width_in = 12,
    height_in = 12,
    set_size_num_size = 9,
    set_size_pad = 1.50,
    text_scale = c(2, 1.3, 1.6, 1.4, 2, 2.5),
    point_size = 2.8,
    line_size = 0.8
)

message("Study-characteristics figures exported.")
