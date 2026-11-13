# =====================================================================
# QUADAS-2 risk of bias figure (domain-level stacked bars)
# Outputs: summary table and figure
# Author: Millen James Theophilus (GitHub: miltheo)
# =====================================================================

rm(list = ls()); gc()

# -----------------------------
# Paths
# -----------------------------
in_dir  <- file.path("analysis", "inputs")
out_dir <- file.path("analysis", "outputs", "manuscript")


csv_path <- file.path(in_dir, "Quality_Assessment.csv")
stopifnot(file.exists(csv_path))


dir.create(file.path(out_dir,"Tables"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(out_dir,"Figures","Main"),recursive=TRUE,showWarnings=FALSE)

# -----------------------------
# Packages
# -----------------------------
req <- c("readr","dplyr","tidyr","ggplot2","ragg")
to_install <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))

# -----------------------------
# Load and reshape
# -----------------------------
dat <- readr::read_csv(csv_path, show_col_types = FALSE)

long <- dat |>
    select(
        `Domain 1: Patient Selection/Study Design`,
        `Domain 2: Index Measure`,
        `Domain 3: Criterion Measure`,
        `Domain 4: Flow and Timing`
    ) |>
    rename(
        `Patient selection/study design` = `Domain 1: Patient Selection/Study Design`,
        `Index measure` = `Domain 2: Index Measure`,
        `Criterion measure` = `Domain 3: Criterion Measure`,
        `Flow/timing` = `Domain 4: Flow and Timing`
    ) |>
    pivot_longer(everything(), names_to = "Domain", values_to = "Rating") |>
    mutate(
        Rating = if_else(grepl("(?i)low", Rating), "Low", "High"),
        Rating = factor(Rating, levels = c("Low","High")),
        Domain = factor(Domain, levels = rev(c("Patient selection/study design","Index measure","Criterion measure","Flow/timing")))
    )

summary_tbl <- long |>
    count(Domain, Rating, name = "n") |>
    group_by(Domain) |>
    mutate(
        Percent = 100 * n / sum(n),
        label = paste0(sprintf("%.1f", Percent), "%\n(N=", n, ")")
    ) |>
    ungroup()

readr::write_csv(summary_tbl, file.path(out_dir, "Tables", "Quality_Assessment_Summary.csv"))

# -----------------------------
# Plot
# -----------------------------
quadas_cols <- c(Low = "#009E73", High = "#D55E00")

p <- ggplot(summary_tbl, aes(x = Percent, y = Domain, fill = Rating)) +
    geom_col(width = 0.62, colour = "grey15", linewidth = 0.35) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 7.5,
              fontface = "bold",
              lineheight = 0.9,
              colour = "white") +
    scale_fill_manual(values = quadas_cols,
                      breaks = c("Low","High"),
                      labels = c("Low risk of bias","High risk of bias")) +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20),
                       labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.015))) +
    labs(title = "QUADAS-2 Risk of Bias by Domain",
         x = "Percentage of studies", y = NULL, fill = NULL) +
    theme_minimal(base_size = 25, base_family = "Arial") +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 22, margin = margin(b = 10)),
        axis.title.x = element_text(size = 22, margin = margin(t = 10)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 22, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 22),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.5, colour = "grey85"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(14, 18, 14, 18)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(file.path(out_dir, "Figures", "Main", "Figure_7_600dpi.png"),
       p, width=16, height=8, units="in", dpi=600, device=ragg::agg_png, bg="white")
