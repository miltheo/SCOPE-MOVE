# =====================================================================
# Study characteristics figures
# Outputs: publication year, countries, UpSet plots, sample size, health, sampling rate
# Author: Millen James Theophilus (GitHub: miltheo)
# =====================================================================

rm(list = ls()); gc()

# -----------------------------
# Paths
# -----------------------------
in_dir  <- file.path("analysis", "inputs")
out_dir <- file.path("analysis", "outputs", "study_characteristics")

# in_dir  <- file.path("..", "analysis", "inputs")
# out_dir <- file.path("..", "analysis", "outputs", "study_characteristics")

csv_path <- file.path(in_dir, "Study_characteristics.csv")
stopifnot(file.exists(csv_path))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)                         # make base output dir
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)    # make tables dir
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)   # make figures dir

# -----------------------------
# Packages
# -----------------------------
req <- c("readr","dplyr","stringr","tidyr","ggplot2","purrr","ragg","forcats",
         "sf","rnaturalearth","UpSetR","grid","ggrepel","scales")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))

# -----------------------------
# Load
# -----------------------------
dat <- readr::read_csv(csv_path, show_col_types = FALSE) |>
    dplyr::filter(!is.na(id))

# -----------------------------
# Publication year figures
# -----------------------------
p_year_all <- dat |>
    count(year) |>
    ggplot(aes(x = factor(year), y = n)) +
    geom_col() +
    labs(x = "Publication year", y = "Number of studies") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "figures", "year_bar_all.png"),
       p_year_all, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

p_year_thr <- dat |>
    mutate(model_group = ifelse(method_best == "Threshold / Cutpoint", "Threshold / Cutpoint", "ML and other models")) |>
    count(year, model_group) |>
    ggplot(aes(x = factor(year), y = n, fill = model_group)) +
    geom_col(position = "stack") +
    labs(x = "Publication year", y = "Number of studies", fill = "Model type") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")

ggsave(file.path(out_dir, "figures", "year_bar_threshold_vs_nontraditional.png"),
       p_year_thr, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

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

ggsave(file.path(out_dir, "figures", "year_bar_by_method.png"),
       p_year_method, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

# -----------------------------
# Countries (bar + bubble map)
# -----------------------------
dat_country <- dat |>
    separate_rows(country, sep = ";") |>
    mutate(country = str_trim(country)) |>
    filter(!is.na(country), country != "") |>
    count(country, name = "n_studies", sort = TRUE)

readr::write_csv(dat_country, file.path(out_dir, "tables", "country_counts.csv"))

p_country_bar <- dat_country |>
    slice_max(n_studies, n = 10) |>
    ggplot(aes(x = reorder(country, n_studies), y = n_studies)) +
    geom_col() +
    geom_text(aes(label = n_studies), hjust = -0.2, size = 4.2) +
    coord_flip() +
    labs(title = "Top countries by number of studies", x = "Country", y = "Number of studies") +
    theme_minimal(base_size = 14)

ggsave(file.path(out_dir, "figures", "country_bar.png"),
       p_country_bar, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

map_data <- world |>
    left_join(dat_country, by = c("name" = "country")) |>
    filter(!is.na(n_studies)) |>
    mutate(
        centroid = sf::st_centroid(geometry),
        lon = sf::st_coordinates(centroid)[,1],
        lat = sf::st_coordinates(centroid)[,2]
    )

p_country_bubble <- ggplot(data = world) +
    geom_sf(fill = "gray95", color = "gray80") +
    geom_point(data = map_data, aes(x = lon, y = lat, size = n_studies), alpha = 0.7) +
    scale_size_continuous(range = c(2, 12), name = "Number of studies") +
    labs(title = "Number of studies by country") +
    theme_minimal(base_size = 14)

ggsave(file.path(out_dir, "figures", "country_bubble.png"),
       p_country_bubble, width = 14, height = 8, dpi = 600, device = ragg::agg_png)

# -----------------------------
# UpSet helper
# -----------------------------
save_upset <- function(data, set_col, file_name, sets_max = 6, n_intersects = 30, sets_label = "Studies per set") {
    long <- data |>
        separate_rows({{ set_col }}, sep = ";\\s*") |>
        mutate(v = str_trim({{ set_col }})) |>
        filter(!is.na(v), v != "") |>
        distinct(id, v)
    
    wide <- long |>
        mutate(present = 1L) |>
        tidyr::pivot_wider(names_from = v, values_from = present, values_fill = 0L) |>
        as.data.frame()
    
    set_names <- setdiff(names(wide), "id")
    freq <- colSums(wide[, set_names, drop = FALSE])
    ord <- names(sort(freq, decreasing = TRUE))
    ord <- ord[seq_len(min(length(ord), sets_max))]
    sets_for_plot <- rev(ord)
    
    up <- UpSetR::upset(wide,
                        sets = sets_for_plot,
                        keep.order = TRUE,
                        nintersects = n_intersects,
                        order.by = "freq",
                        mb.ratio = c(0.65, 0.35),
                        point.size = 3,
                        mainbar.y.label = "Number of studies",
                        sets.x.label = sets_label,
                        text.scale = c(1.8, 1.5, 1.4, 1.2, 1.8, 2))
    
    g <- grid::grid.grabExpr(print(up), wrap.grobs = TRUE)
    
    ggsave(file.path(out_dir, "figures", file_name),
           plot = g, width = 10, height = 10, dpi = 600, device = ragg::agg_png, units = "in")
}

save_upset(dat, age_group, "age_group_upset.png", sets_max = 4, sets_label = "Studies per age group")
save_upset(dat, outcomes, "outcomes_upset.png", sets_max = 6, sets_label = "Studies per outcome")
save_upset(dat, device_brand, "devices_upset.png", sets_max = 4, sets_label = "Studies per device")

# -----------------------------
# Sample size figures
# -----------------------------
dat2 <- dat |>
    mutate(
        sample_size = suppressWarnings(as.numeric(na_if(sample_size, "n.r."))),
        females = suppressWarnings(as.numeric(na_if(females, "n.r.")))
    )

p_sample_age <- dat2 |>
    filter(is.finite(sample_size) & sample_size > 0) |>
    separate_rows(age_group, sep = ";\\s*") |>
    mutate(age_group = str_trim(age_group)) |>
    ggplot(aes(x = forcats::fct_infreq(age_group), y = sample_size)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    scale_y_log10() +
    labs(title = "Sample size distribution by age group", x = "Age group", y = "Sample size") +
    theme_minimal(base_size = 14) +
    coord_flip()

ggsave(file.path(out_dir, "figures", "sample_size_by_age_group.png"),
       p_sample_age, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

p_sample_fem <- dat2 |>
    filter(is.finite(sample_size) & sample_size > 0, is.finite(females) & females > 0) |>
    pivot_longer(cols = c(sample_size, females), names_to = "metric", values_to = "value") |>
    ggplot(aes(x = metric, y = value)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6) +
    geom_jitter(width = 0.15, alpha = 0.5) +
    scale_y_log10() +
    labs(title = "Sample size and female subgroup distribution", x = "Metric", y = "Count") +
    theme_minimal(base_size = 14)

ggsave(file.path(out_dir, "figures", "sample_size_and_females.png"),
       p_sample_fem, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

# -----------------------------
# Health status pie
# -----------------------------
palette_health <- c(
    "Apparently Healthy" = "#91B3BC",
    "Clinical" = "#C2B9B0",
    "Mix" = "#A3A3A3",
    "Wheelchair users" = "#B3A1C9",
    "Not Reported" = "#D79999"
)

dat_health <- dat2 |>
    mutate(health_clean = dplyr::case_when(is.na(health) | health == "n.r." ~ "Not Reported", TRUE ~ health)) |>
    count(health_clean, name = "n") |>
    mutate(percent = n / sum(n),
           label = paste0(health_clean, "\n", round(percent * 100, 1), "%"),
           ypos = cumsum(percent) - 0.5 * percent)

p_health <- ggplot(dat_health, aes(x = "", y = percent, fill = health_clean)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    ggrepel::geom_text_repel(aes(y = ypos, label = label),
                             size = 5, nudge_x = 1, show.legend = FALSE,
                             segment.color = "gray60", segment.size = 0.4,
                             direction = "y", hjust = 0) +
    scale_fill_manual(values = palette_health) +
    labs(title = "Health status of participants across studies") +
    theme_void(base_size = 14) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(out_dir, "figures", "health_status_pie.png"),
       p_health, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

# -----------------------------
# Sampling rate pie
# -----------------------------
palette_sampling <- c(
    "1-20 Hz"   = "#5B84B1FF",
    "21-50 Hz"  = "#9DC3E6",
    "51-80 Hz"  = "#88B04B",
    "81-100 Hz" = "#C2B280",
    ">100 Hz"   = "#D07C7C",
    "Variable"  = "#A593C2",
    "n.r."      = "#BFBFBF"
)

dat_sr <- dat |>
    mutate(sampling_rate_cat = ifelse(is.na(sampling_rate_cat), "n.r.", sampling_rate_cat)) |>
    count(sampling_rate_cat, name = "n") |>
    mutate(percent = n / sum(n),
           label = paste0(sampling_rate_cat, "\n", round(percent * 100, 1), "%"),
           ypos = cumsum(percent) - 0.5 * percent)

p_sr <- ggplot(dat_sr, aes(x = "", y = percent, fill = sampling_rate_cat)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    ggrepel::geom_text_repel(aes(y = ypos, label = label),
                             size = 4.5, nudge_x = 1, show.legend = FALSE,
                             segment.color = "gray60", segment.size = 0.4,
                             direction = "y", hjust = 0) +
    scale_fill_manual(values = palette_sampling) +
    labs(title = "Sampling rates used across included studies") +
    theme_void(base_size = 14) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(out_dir, "figures", "sampling_rate_pie.png"),
       p_sr, width = 10, height = 10, dpi = 600, device = ragg::agg_png)

cat("Done. Outputs saved to: ", normalizePath(out_dir), "\n")
