# ======================================================================
# SCOPE-MOVE v1.0.3
# - Code for the shiny web app, SCOPE-MOVE.
# - Code version 4.6 22Jan26
# - Author: Millen J. Theophilus
# =================

rm(list = ls()); gc()

# ---------------------------
# PACKAGES
# ---------------------------
library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(readr)
library(metafor)
library(plotly)
library(DT)
library(scales)
library(clubSandwich)

# ---------------------------
# PATHS
# ---------------------------
# setwd("C:/GitHub/SCOPE-MOVE/app/") # Comment out before deploying to shinyapps.io

forest_path <- "data/REML_data_long.csv"
master_path <- "data/Extraction_models_master_sheet.csv"

# ---------------------------
# HELPERS (meta-analysis, proportions on logit scale)
# ---------------------------
logit_safe <- function(p, eps = 1e-6) qlogis(pmax(pmin(p, 1 - eps), eps))
inv_logit  <- function(x) plogis(x)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
safe_n <- function(x) suppressWarnings(as.numeric(x))

# ---------------------------
# HELPERS (pooled estimate with fallback + descriptive breakdown)
# ---------------------------
fit_metric_minimal <- function(dfm) rma.uni(yi = dfm$yi, vi = dfm$vi_use, method = "REML")

pooled_standardized_minimal <- function(fit) {
    est <- as.numeric(fit$b[1]); se <- as.numeric(fit$se[1])
    c(est = inv_logit(est), lb = inv_logit(est - 1.96 * se), ub = inv_logit(est + 1.96 * se))
}

clamp_prob <- function(p, eps = 1e-3) pmin(pmax(p, eps), 1 - eps)

fit_metric_manuscript <- function(dfm) {
    if (nrow(dfm) < 3) return(NULL)
    dfm <- dfm %>% mutate(study_id = factor(study_id), val_id = factor(val_id))
    base_mods <- c("c_n_classes","c_k_folds","miss_n_classes","miss_k_folds")
    add_mods  <- c("env_re","prot_re","dom_re")
    X <- dfm %>% dplyr::select(any_of(c(base_mods, add_mods)))
    nonconst <- vapply(X, \(v) length(unique(v[!is.na(v)])) > 1, logical(1))
    kept <- names(X)[nonconst]
    fml <- if (length(kept)) as.formula(paste("~", paste(kept, collapse = " + "))) else ~ 1
    m <- try(metafor::rma.mv(yi = yi, V = vi_use, mods = fml,
                             random = list(~1 | study_id, ~1 | val_id),
                             data = dfm, method = "REML",
                             control = list(optimizer = "optim")), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    list(model = m, kept_mods = kept, clusters = dplyr::n_distinct(dfm$study_id))
}

pooled_standardized_manuscript <- function(fit, dfm, weight_by_n = TRUE, n_col = "n_participants", eps = 1e-3) {
    X <- as.matrix(fit$model$X)
    beta <- as.numeric(fit$model$beta)
    if (!is.null(colnames(X)) && !is.null(names(fit$model$beta))) X <- X[, names(fit$model$beta), drop = FALSE]
    eta <- as.numeric(X %*% beta)
    p_i <- clamp_prob(plogis(eta), eps = eps)
    w <- if (weight_by_n && n_col %in% names(dfm) && any(is.finite(dfm[[n_col]]))) dfm[[n_col]] else rep(1, nrow(dfm))
    w <- w / sum(w, na.rm = TRUE)
    est <- sum(w * p_i, na.rm = TRUE)
    vc_type <- if (fit$clusters >= 6) "CR2" else "CR1S"
    V <- try(clubSandwich::vcovCR(fit$model, type = vc_type, cluster = dfm$study_id), silent = TRUE)
    if (inherits(V, "try-error") || any(!is.finite(V))) V <- try(stats::vcov(fit$model), silent = TRUE)
    if (inherits(V, "try-error") || any(!is.finite(V))) return(c(est = est, lb = NA, ub = NA))
    J <- colSums((w * p_i * (1 - p_i)) * X, na.rm = TRUE)
    se <- sqrt(drop(J %*% V %*% J))
    if (!is.finite(se) || se < 1e-8) return(c(est = est, lb = NA, ub = NA))
    lb <- max(est - 1.96 * se, 0); ub <- min(est + 1.96 * se, 1)
    c(est = est, lb = lb, ub = ub)
}

# ---------------------------
# HELPERS (tokenisation for multi-valued cells)
# ---------------------------
split_tokens <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    lapply(strsplit(x, ";", fixed = TRUE), \(v) unique(trimws(v[nzchar(trimws(v))])))
}
tokens_to_choices <- function(tok_list) sort(unique(unlist(tok_list, use.names = FALSE)))

# ---------------------------
# LOAD DATA
# ---------------------------
df_all    <- read_csv(forest_path, show_col_types = FALSE)
df_master <- read_csv(master_path, show_col_types = FALSE)

# ---------------------------
# RESTRICT TO CLASSIFIER METRICS (prototype scope)
# ---------------------------
keep_metrics <- c("F1", "Sensitivity", "Specificity")
df_all <- df_all %>% dplyr::filter(metric %in% keep_metrics)
df_all <- df_all %>% dplyr::mutate(env_re = factor(env_re), prot_re = factor(prot_re), dom_re = factor(dom_re))

# ---------------------------
# STANDARDISE FOREST ROWS (effects + labels)
# ---------------------------
df_all <- df_all %>%
    mutate(
        val_id       = trimws(as.character(val_id)),
        metric       = factor(metric, levels = keep_metrics),
        domain       = factor(domain),
        method_group = factor(recode(factor(method_group), "Threshold" = "Traditional", "Other" = "Non-traditional"),
                              levels = c("Traditional", "Non-traditional")),
        row_lab      = paste0(study_id, " - ", method, " - ", val_id),
        yi           = logit_safe(est),
        vi_use       = as.numeric(vi_use)
    )

# ---------------------------
# BUILD MASTER LOOKUPS (age/device + DOI)
# ---------------------------
df_master <- df_master %>% mutate(val_id = trimws(as.character(val_id)))

align_df <- df_master %>% transmute(val_id,
                                    age_group_m   = trimws(as.character(age_group)),
                                    test_device_m = trimws(as.character(test_device)))

doi_df <- df_master %>%
    transmute(val_id, doi_raw = trimws(as.character(doi))) %>%
    mutate(
        doi_raw = sub("^https?://(dx\\.)?doi\\.org/", "", doi_raw),
        doi_url = ifelse(nzchar(doi_raw), paste0("https://doi.org/", doi_raw), NA_character_)
    )

# ---------------------------
# ALIGN + TOKENISE + TOOLTIP TEXT
# ---------------------------
df_all <- df_all %>%
    mutate(age_group = trimws(as.character(age_group)), test_device = trimws(as.character(test_device))) %>%
    left_join(align_df, by = "val_id") %>%
    mutate(
        age_group        = coalesce(age_group_m, age_group),
        test_device      = coalesce(test_device_m, test_device),
        age_tokens       = split_tokens(age_group),
        device_tokens    = split_tokens(test_device),
        age_group_show   = age_group,
        test_device_show = test_device
    ) %>%
    select(-age_group_m, -test_device_m) %>%
    left_join(doi_df %>% select(val_id, doi_url), by = "val_id") %>%
    mutate(
        hover_text = paste0(
            "Study: ", study_id, "\n",
            "Metric: ", metric, "\n",
            "Estimate: ", sprintf("%.3f", est), "\n",
            "95% CI: [", sprintf("%.3f", ci_l), ", ", sprintf("%.3f", ci_u), "]\n",
            "Protocol: ", test_protocol, "\n",
            "Environment: ", test_environment, "\n",
            "Validation type: ", validation_type, "\n",
            "Age group: ", age_group_show, "\n",
            "Device: ", test_device_show, "\n",
            "N participants: ", n_participants, "\n",
            "<b><span style='color:#cc0000;'>Click to open DOI</span></b>"
        )
    )

# ---------------------------
# HOME PANEL OBJECT
# ---------------------------
home_stats <- list(
    n_models        = sum(df_all$is_the_model_published_previously == "No", na.rm = TRUE),
    n_validations   = length(unique(df_all$val_id)),
    n_studies       = length(unique(df_all$id)),
    n_domains       = length(levels(df_all$domain)),
    n_devices       = length(unique(unlist(df_all$device_tokens))),
    n_participants  = sum(safe_n(df_all$n_participants), na.rm = TRUE)
)

# ---------------------------
# DATASET TAB TABLE (study-level view)
# ---------------------------
minimal_df <- df_master %>%
    select(
        study_id = study, val_id, `Is the model published previously?`,
        age_group, method, domain = movement_behaviour,
        test_outcomes = model_test_outcomes, test_criterion,
        test_device, test_protocol, test_environment, validation_type,
        n_participants, f1_mean, sens_mean, spec_mean,
        doi
    ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

# ---------------------------
# FOREST PLOTTER
# ---------------------------
okabe_ito <- c("#CC79A7","#E69F00","#56B4E9")
domain_levels <- c("Sleep","Activity type","Activity intensity")
domain_pal <- setNames(okabe_ito[seq_along(domain_levels)], domain_levels)

pretty_mods <- function(mods) {
    if (is.null(mods) || !length(mods)) return("(intercept only)")
    map <- c(
        c_n_classes    = "Number of classes (centred)",
        c_k_folds      = "Number of CV folds (centred)",
        miss_n_classes = "Missing number of classes indicator",
        miss_k_folds   = "Missing number of CV folds indicator",
        env_re         = "Test environment",
        prot_re        = "Test protocol",
        dom_re         = "Movement behaviour domain"
    )
    out <- unname(ifelse(mods %in% names(map), map[mods], mods))
    paste(out, collapse = ", ")
}

make_forest_plot <- function(df, pooled_val = NA_real_, dark = FALSE) {
    df <- df %>% arrange(est) %>% mutate(row_lab = fct_inorder(row_lab))
    split_at <- NA_real_
    if (is.finite(pooled_val)) {
        below_idx <- which(df$est < pooled_val)
        if (length(below_idx) > 0 && length(below_idx) < nrow(df)) split_at <- max(below_idx) + 0.5
    }
    
    vcol <- if (isTRUE(dark)) "#f2f3f5" else "grey40"
    hcol <- "grey50"
    
    ggplot(df, aes(x = est, y = row_lab)) +
        geom_pointrange(
            aes(xmin = ci_l, xmax = ci_u, colour = domain, text = hover_text, key = val_id),
            size = 0.3, linewidth = 0.3, alpha = 0.9
        ) +
        scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(x = "Estimate (percentage)", y = NULL, colour = "Domain") +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank(),
            legend.title.position = "top",
            legend.title = element_text(colour = if (isTRUE(dark)) "#f2f3f5" else "#111111"),
            legend.text  = element_text(colour = if (isTRUE(dark)) "#f2f3f5" else "#111111"),
            axis.text.y  = element_text(margin = margin(r = 10, unit = "pt")),
            plot.margin  = margin(t = 8, r = 10, b = 8, l = 10)
        ) +
        { if (is.finite(pooled_val)) geom_vline(xintercept = pooled_val, linetype = 2, linewidth = 0.5, colour = vcol) } +
        { if (is.finite(split_at))  geom_hline(yintercept = split_at, linetype = "dashed", linewidth = 0.3, colour = hcol) } +
        scale_colour_manual(name = "Domains", values = domain_pal, breaks = domain_levels, drop = FALSE)
}

# ---------------------------
# UI CHOICES
# ---------------------------
metric_choices <- levels(df_all$metric)
domain_choices <- levels(df_all$domain)
method_choices <- levels(df_all$method_group)
age_choices    <- tokens_to_choices(df_all$age_tokens)
device_choices <- tokens_to_choices(df_all$device_tokens)
default_metric <- if ("F1" %in% metric_choices) "F1" else metric_choices[1]

# ---------------------------
# SIDEBAR DEFINITIONS
# ---------------------------
metric_help <- list(
    "F1"          = "Harmonic mean of precision and sensitivity (recall). Higher is better.",
    "Sensitivity" = "True positive rate (recall). Proportion of true positives correctly identified. Higher is better.",
    "Specificity" = "True negative rate. Proportion of true negatives correctly identified. Higher is better."
)

method_help <- list(
    "Traditional"     = "Threshold, heuristic rule-based, and general linear model approaches. Typically simpler and more interpretable.",
    "Non-traditional" = "Classical machine learning, deep learning, probabilistic temporal models, and ensembles. Typically more complex and less interpretable."
)

# ---------------------------
# INSTRUCTIONS
# ---------------------------
domain_defs <- list(
    "Activity Intensity" = "Time spent in physical activity intensity categories (e.g., sedentary, light, moderate, vigorous), benchmarked against indirect calorimetry and expressed via MET-based thresholds.",
    "Activity Type"      = "Discrete activity classes (e.g., walking, running, cycling, sedentary), benchmarked against direct or video observation.",
    "Sleep/Wake"         = "Binary sleep versus wake classification, benchmarked against polysomnography (PSG)."
)

protocol_defs <- list(
    "Structured"   = "A highly controlled protocol (often laboratory-based), where activities and timing are prescribed with limited participant autonomy.",
    "Unstructured" = "A free-living or naturalistic protocol, where behaviour is not prescribed and participants follow self-selected routines.",
    "Hybrid"       = "A mixed design combining limited structure and participant autonomy, where participants follow self-selected routines within constrains set by the researchers (timing and/or activity choice)."
)

environment_defs <- list(
    "Laboratory"  = "Data collected in a controlled research setting (lab, clinic, metabolic room).",
    "Free-living" = "Data collected in an external environment (home, community park, school playground).",
    "Hybrid"      = "A mixed design combining laboratory with free-living components (lab + home)."
)

validation_defs <- list(
    "Apparent"         = "Performance estimated on the same data used for model training (optimistic; not recommended for generalisation).",
    "Hold-out"         = "Train/test split within a dataset, where a subset is withheld for testing.",
    "Cross-validation" = "Repeated internal resampling (e.g., k-fold, leave-one-subject-out) to estimate internal generalisation.",
    "External"         = "Testing on an independent dataset collected separately from the training data using a different sample of participants (dataset may originate from the same lab, research group, or device)."
)

# ---------------------------
# CSS (single-source via variables, de-duplicated)
# - Light theme aligned to #FFFDEE
# ---------------------------
theme_css <- "
:root{
  --bg: #FFFDEE;
  --panel: #FFFDEE;

  --txt: #111111;
  --muted: #4b4b4b;

  --card: #FFFEF6;
  --border: #E9E1B8;

  --kv_bg: #FFFFFF;
  --kv_border: #EDE4BF;

  --pill_bg: #FFF9DC;
  --pill_border: #E9E1B8;

  --input_bg: #FFFFFF;
  --input_txt: #111111;
  --input_border: #D7CEA0;

  --dt_bg: #FFFFFF;
  --dt_hdr: #FFF7CC;
  --dt_alt: #FFFEF2;
  --dt_hover: #FFF2B8;
  --dt_border: #E9E1B8;
  --dt_txt: #111111;

  --hover_bg: rgba(255,253,238,0.98);
  --hover_txt: #111111;
}

:root[data-theme='dark'],
:root.theme-dark{
  --bg: #0b0f14;
  --panel: #0b0f14;
  --txt: #e6e6e6;
  --muted: #b8c0cc;

  --card: #121826;
  --border: #2a3443;

  --kv_bg: #0f1522;
  --kv_border: #2a3443;

  --pill_bg: #0f1522;
  --pill_border: #2a3443;

  --input_bg: #0b0f14;
  --input_txt: #f2f3f5;
  --input_border: #2a3443;

  --dt_bg: #0f1522;
  --dt_hdr: #121826;
  --dt_alt: #0c121d;
  --dt_hover: #141c2a;
  --dt_border: #2a3443;
  --dt_txt: #f2f3f5;

  --hover_bg: rgba(20,24,32,0.95);
  --hover_txt: #ffffff;
}

@media (prefers-color-scheme: dark){
  :root:not(.theme-dark):not(.theme-light){
    --bg: #0b0f14;
    --panel: #0b0f14;
    --txt: #e6e6e6;
    --muted: #b8c0cc;

    --card: #121826;
    --border: #2a3443;

    --kv_bg: #0f1522;
    --kv_border: #2a3443;

    --pill_bg: #0f1522;
    --pill_border: #2a3443;

    --input_bg: #0b0f14;
    --input_txt: #f2f3f5;
    --input_border: #2a3443;

    --dt_bg: #0f1522;
    --dt_hdr: #121826;
    --dt_alt: #0c121d;
    --dt_hover: #141c2a;
    --dt_border: #2a3443;
    --dt_txt: #f2f3f5;

    --hover_bg: rgba(20,24,32,0.95);
    --hover_txt: #ffffff;
  }
}

:root.theme-light{}

html, body{
  min-height: 100%;
  background: var(--bg) !important;
  color: var(--txt) !important;
}

.container-fluid{
  min-height: 100vh;
  background: transparent !important;
}

.tab-content, .tab-pane{
  background: transparent !important;
}

.tab-content{
  padding: 2px 8px 8px 8px;
}

.nav-tabs{
  margin-bottom: 10px;
}

.nav-tabs > li > a{
  color: var(--txt) !important;
  background: transparent !important;
  border: 1px solid transparent !important;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover{
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  color: var(--txt) !important;
}

.side-card, .inst-card, .pooled-card{
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 10px;
}

.side-card{
  padding: 12px 14px;
  margin-top: 10px;
}

.inst-wrap{
  max-width: 1100px;
  margin: 10px auto 30px auto;
}

.inst-card{
  padding: 16px 18px;
  margin-bottom: 14px;
}

.pooled-card{
  padding: 10px 12px;
}

.inst-card p, .side-card p, .pooled-card p{
  line-height: 1.5;
  margin: 0 0 10px 0;
}

.inst-card ul, .inst-card ol,
.side-card ul, .side-card ol{
  margin: 0 0 10px 0;
  padding-left: 20px;
}

.inst-card li, .side-card li{
  margin: 0 0 6px 0;
  line-height: 1.45;
}

.side-card h4{
  margin-top: 2px;
  margin-bottom: 8px;
}

.inst-card h4{
  margin-top: 0;
}

.inst-grid{
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 14px;
}

@media (max-width: 900px){
  .inst-grid{ grid-template-columns: 1fr; }
}

.inst-note{
  color: var(--muted);
}

.inst-kv dt{
  font-weight: 600;
  margin-top: 8px;
}

.inst-kv dd{
  margin-left: 0;
  margin-bottom: 6px;
  line-height: 1.45;
  color: var(--muted);
}

.forest-caption{
  font-size: 12px;
  color: var(--muted);
  margin-top: 4px;
  margin-bottom: 10px;
}

.pooled-title{
  margin: 0 0 6px 0;
  font-weight: 700;
}

.pooled-sub{
  color: var(--muted);
  margin: 0 0 10px 0;
}

.pooled-grid{
  display: grid;
  grid-template-columns: 1fr 1fr 1fr;
  gap: 8px 12px;
}

@media (max-width: 900px){
  .pooled-grid{ grid-template-columns: 1fr; }
}

.pooled-kv{
  background: var(--kv_bg);
  border: 1px solid var(--kv_border);
  border-radius: 10px;
  padding: 8px 10px;
}

.pooled-k{
  color: var(--muted);
  font-size: 12px;
  margin: 0 0 4px 0;
}

.pooled-v{
  font-size: 16px;
  font-weight: 700;
  margin: 0;
}

.pooled-small{
  font-size: 12px;
  color: var(--muted);
  margin-top: 8px;
  line-height: 1.35;
}

.pooled-pill{
  display: inline-block;
  padding: 2px 8px;
  border-radius: 999px;
  border: 1px solid var(--pill_border);
  background: var(--pill_bg);
  font-size: 12px;
  margin-right: 6px;
  margin-bottom: 6px;
  color: var(--txt);
}

.forest-grid{
  display: grid;
  grid-template-columns: minmax(340px, 26vw) 1fr;
  gap: 14px;
  align-items: start;
  height: calc(100vh - 165px);
}


@media (max-width: 1050px){
  .forest-grid{ grid-template-columns: 1fr; height: auto; }
  .forest-sidebar{ max-height: 520px; }
  .forest-main{ height: calc(100vh - 260px); }
}

.forest-sidebar{
  height: 100%;
  overflow: auto;
  padding-right: 2px;
}

.forest-main{
  height: 100%;
  display: flex;
  flex-direction: column;
  min-width: 0;
  padding-bottom: 10px;
}

.forest-plotwrap{
  flex: 1 1 auto;
  min-height: 420px;
  overflow: auto;
}

.forest-pooledwrap{
  flex: 0 0 auto;
  max-height: 210px;
  overflow: auto;
  margin-top: 10px;
  padding-bottom: 10px;
  margin-bottom: 10px;
}

.selectize-input, .form-control, .selectize-dropdown{
  background: var(--input_bg) !important;
  color: var(--input_txt) !important;
  border-color: var(--input_border) !important;
}

.selectize-dropdown-content .option{
  color: var(--input_txt) !important;
}

.radio label, .checkbox label{
  color: var(--txt) !important;
}

.dataTables_wrapper{
  color: var(--dt_txt) !important;
}

table.dataTable{
  background: var(--dt_bg) !important;
  color: var(--dt_txt) !important;
}

table.dataTable thead th{
  background: var(--dt_hdr) !important;
  color: var(--dt_txt) !important;
  border-bottom: 1px solid var(--dt_border) !important;
}

table.dataTable tbody td{
  background: var(--dt_bg) !important;
  color: var(--dt_txt) !important;
  border-top: 1px solid var(--dt_border) !important;
}

table.dataTable.display tbody tr.odd  > td{ background: var(--dt_bg) !important; }
table.dataTable.display tbody tr.even > td{ background: var(--dt_alt) !important; }
table.dataTable.hover   tbody tr:hover > td{ background: var(--dt_hover) !important; }

.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select{
  background: var(--input_bg) !important;
  color: var(--input_txt) !important;
  border: 1px solid var(--input_border) !important;
}

.dataTables_wrapper .dataTables_filter label,
.dataTables_wrapper .dataTables_length label,
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate{
  color: var(--dt_txt) !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button{
  color: var(--dt_txt) !important;
  border: 1px solid var(--dt_border) !important;
  background: var(--dt_bg) !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button.current{
  background: var(--dt_hdr) !important;
  border-color: var(--dt_border) !important;
}

.plot-container, .svg-container{
  background: transparent !important;
}

.modal-content{
  background: var(--card) !important;
  color: var(--txt) !important;
  border: 1px solid var(--border) !important;
  border-radius: 10px !important;
}

.modal-header{
  border-bottom: 1px solid var(--border) !important;
}
.modal-footer{
  border-top: 1px solid var(--border) !important;
}

.modal-title{
  color: var(--txt) !important;
}
.modal-header .close{
  color: var(--txt) !important;
  opacity: 0.9;
  text-shadow: none !important;
}

.modal-backdrop.in{ opacity: 0.65; }
:root.theme-dark .modal-backdrop.in{ opacity: 0.75; }
@media (prefers-color-scheme: dark){
  :root:not(.theme-dark):not(.theme-light) .modal-backdrop.in{ opacity: 0.75; }
}

.modal-content a{
  color: var(--txt) !important;
  text-decoration: underline;
}
"

# ---------------------------
# ADDITIONAL CSS (Home, Instructions, Footer)
# ---------------------------
theme_css <- paste0(theme_css, "

/* ===== Landing layout ===== */
.hero{
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 18px 18px;
  margin: 10px auto 14px auto;
  max-width: 1100px;
}

.hero-title{
  font-size: 22px;
  font-weight: 800;
  margin: 0 0 6px 0;
}

.hero-sub{
  color: var(--muted);
  margin: 0 0 12px 0;
  line-height: 1.45;
}

.hero-actions{
  display: flex;
  gap: 10px;
  flex-wrap: wrap;
  margin-top: 10px;
}

.btn-soft{
  border-radius: 10px !important;
  border: 1px solid var(--border) !important;
  background: var(--kv_bg) !important;
  color: var(--txt) !important;
}

.btn-primary-soft{
  border-radius: 10px !important;
  border: 1px solid var(--border) !important;
  background: var(--dt_hdr) !important;
  color: var(--txt) !important;
}

/* ===== Tiles ===== */
.tile-grid{
  max-width: 1100px;
  margin: 0 auto 14px auto;
  display: grid;
  grid-template-columns: repeat(6, 1fr);
  gap: 12px;
}

@media (max-width: 1050px){
  .tile-grid{ grid-template-columns: repeat(2, 1fr); }
}

.tile{
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 12px 12px;
  min-height: 86px;
}

.tile-k{
  color: var(--muted);
  font-size: 12px;
  margin: 0 0 4px 0;
}

.tile-v{
  font-size: 20px;
  font-weight: 800;
  margin: 0;
}

.tile-s{
  color: var(--muted);
  font-size: 12px;
  margin: 6px 0 0 0;
  line-height: 1.35;
}

/* ===== Feature cards ===== */
.feature-grid{
  max-width: 1100px;
  margin: 0 auto 18px auto;
  display: grid;
  grid-template-columns: 1.1fr 0.9fr;
  gap: 12px;
}

@media (max-width: 1050px){
  .feature-grid{ grid-template-columns: 1fr; }
}

.feature{
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 14px 14px;
}

.feature h4{
  margin: 0 0 8px 0;
}

.stepper{
  display: grid;
  gap: 8px;
  margin: 0;
  padding: 0;
  list-style: none;
}

.step{
  background: var(--kv_bg);
  border: 1px solid var(--kv_border);
  border-radius: 12px;
  padding: 10px 10px;
}

.step b{
  display: inline-block;
  margin-right: 6px;
}

/* ===== Accordion (progressive disclosure) ===== */
.details{
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 0;
  overflow: hidden;
}

.details summary{
  cursor: pointer;
  padding: 12px 14px;
  font-weight: 800;
  background: var(--dt_hdr);
  border-bottom: 1px solid var(--border);
}

.details .details-body{
  padding: 12px 14px;
}

.details .details-body p,
.details .details-body li{
  color: var(--txt);
}

.details .details-body .muted{
  color: var(--muted);
}

/* Muted body copy inside selected instruction sections */
.details .details-body.muted-body{
  color: var(--muted);
}
.details .details-body.muted-body p,
.details .details-body.muted-body li{
  color: var(--muted) !important;
}
.details .details-body.muted-body b,
.details .details-body.muted-body strong{
  color: var(--txt) !important;   /* keep emphasis readable */
}


/* ===== Home: better responsive proportions ===== */
.feature-grid{
  grid-template-columns: 1.25fr 0.75fr;  /* give Quick start enough width */
  gap: 14px;
}

@media (max-width: 1100px){
  .feature-grid{ grid-template-columns: 1fr; }
}

/* ===== Home: breakdown bars (replace basic plot) ===== */
.breakdown{
  display: grid;
  gap: 10px;
}

.break-row{
  background: var(--kv_bg);
  border: 1px solid var(--kv_border);
  border-radius: 12px;
  padding: 10px 10px;
}

.break-top{
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 10px;
  margin-bottom: 8px;
}

.break-dom{
  font-weight: 800;
}

.break-n{
  color: var(--muted);
  font-size: 12px;
  white-space: nowrap;
}

.hbar{
  height: 12px;
  border-radius: 999px;
  overflow: hidden;
  border: 1px solid var(--kv_border);
  background: rgba(127,127,127,0.08);
  display: flex;
}

.hseg{
  height: 100%;
}

.hlab{
  display: flex;
  gap: 10px;
  flex-wrap: wrap;
  margin-top: 8px;
  font-size: 12px;
  color: var(--muted);
}

.dot{
  display: inline-block;
  width: 10px;
  height: 10px;
  border-radius: 999px;
  margin-right: 6px;
  transform: translateY(1px);
}

/* ===== Instructions spacing ===== */
.details{
  margin-bottom: 16px;  /* more space between instruction sections */
}

/* Reserve space so footer never overlaps Plotly or tab content */
.tab-content{
  padding-bottom: 84px; /* approx footer height plus breathing room */
}

.app-footer{
  position: relative;
  z-index: 2;
}

.plotly, .plot-container, .svg-container{
  z-index: 1;
}

/* ===== Floating theme toggle ===== */
.theme-fab{
  position: fixed;
  right: 16px;
  bottom: 16px;
  z-index: 9999;
  width: 44px;
  height: 44px;
  border-radius: 999px;
  border: 1px solid var(--border);
  background: var(--card);
  color: var(--txt);
  display: grid;
  place-items: center;
  cursor: pointer;
  box-shadow: 0 6px 18px rgba(0,0,0,0.18);
}

.theme-fab:hover{ filter: brightness(0.98); }

.theme-fab .ico{
  font-size: 18px;
  line-height: 1;
}

.theme-fab .tip{
  position: absolute;
  right: 52px;
  bottom: 8px;
  background: var(--card);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 6px 8px;
  font-size: 12px;
  color: var(--muted);
  white-space: nowrap;
  opacity: 0;
  transform: translateX(6px);
  transition: all 120ms ease;
  pointer-events: none;
}

.theme-fab:hover .tip{
  opacity: 1;
  transform: translateX(0px);
}

@media (max-width: 420px){
  .theme-fab{ right: 10px; bottom: 10px; }
}


")




# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
    tags$head(
        tags$title("SCOPE-MOVE"),
        tags$style(HTML(theme_css)),
        tags$script(HTML("
              function reportWin(){ Shiny.setInputValue('win_h', window.innerHeight, {priority:'event'}); }
              window.addEventListener('load', reportWin);
              window.addEventListener('resize', reportWin);
            
              function systemPref(){
                return (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : 'light';
              }
            
              function setFab(mode){
                var tip = document.getElementById('theme_fab_tip');
                var ico = document.getElementById('theme_fab_ico');
                if(!tip || !ico) return;
            
                var label = (mode === 'system') ? 'System' : (mode === 'light' ? 'Light' : 'Dark');
                tip.textContent = 'Theme: ' + label;
            
                if(mode === 'dark')  ico.textContent = '🌙';
                if(mode === 'light') ico.textContent = '☀';
                if(mode === 'system') ico.textContent = '🖥';
              }
            
              function applyTheme(mode){
                document.documentElement.classList.remove('theme-dark','theme-light');
                if(mode === 'dark')  document.documentElement.classList.add('theme-dark');
                if(mode === 'light') document.documentElement.classList.add('theme-light');
            
                var eff = (mode === 'system') ? systemPref() : mode;
                Shiny.setInputValue('theme_effective', eff, {priority:'event'});
            
                try{ localStorage.setItem('scope_move_theme', mode); }catch(e){}
                setFab(mode);
              }
            
              function getSavedTheme(){
                try{
                  var v = localStorage.getItem('scope_move_theme');
                  if(v === 'light' || v === 'dark' || v === 'system') return v;
                }catch(e){}
                return 'system';
              }
            
              function cycleTheme(cur){
                if(cur === 'system') return 'light';
                if(cur === 'light') return 'dark';
                return 'system';
              }
            
              Shiny.addCustomMessageHandler('set_theme', function(mode){ applyTheme(mode); });
            
              window.addEventListener('load', function(){
                var mode = getSavedTheme();
                applyTheme(mode);
            
                var btn = document.getElementById('theme_fab');
                if(btn){
                  btn.addEventListener('click', function(){
                    var saved = getSavedTheme();
                    applyTheme(cycleTheme(saved));
                  });
                }
              });
            
              var mq = window.matchMedia('(prefers-color-scheme: dark)');
              if(mq && mq.addEventListener){
                mq.addEventListener('change', function(){
                  var saved = getSavedTheme();
                  if(saved === 'system'){ applyTheme('system'); }
                });
              }
            "))
    ),
    
    titlePanel("SCOPE-MOVE: SCOPing Explorer of accelerometer-based prediction models for 24-hour MOVEment behaviour analysis"),
    
    tabsetPanel(id = "tabs",
                
                tabPanel("Home",
                         tags$div(class = "hero",
                                  tags$div(class = "hero-title", "SCOPE-MOVE"),
                                  tags$p(class = "hero-sub",
                                         "An evidence-based explorer of criterion validation performance for wrist-worn accelerometer prediction models across 24-hour movement behaviour domains."
                                  ),
                                  tags$div(class = "hero-actions",
                                           actionButton("go_forest", "Open Forest Plot", class = "btn-soft"),
                                           actionButton("go_instructions", "Read Instructions", class = "btn-soft"),
                                           actionButton("go_dataset", "Browse Dataset", class = "btn-soft")
                                  ),
                                  tags$p(class = "hero-sub",
                                         tags$br(),
                                         tags$span(class = "footer-text",
                                                   "Tip: start with F1 for an overall view, then inspect sensitivity and specificity for domain-specific limitations."
                                         )
                                  )
                         ),
                         
                         tags$div(class = "tile-grid",
                                  tags$div(class = "tile",
                                           tags$p(class = "tile-k", "Unique prediction models"),
                                           tags$p(class = "tile-v", home_stats$n_models),
                                           tags$p(class = "tile-s", "Models not previously published (as coded in extraction).")
                                  ),
                                  tags$div(class = "tile",
                                           tags$p(class = "tile-k", "Validations with metrics"),
                                           tags$p(class = "tile-v", home_stats$n_validations),
                                           tags$p(class = "tile-s", "Validation rows with F1, sensitivity, or specificity.")
                                  ),
                                  tags$div(class = "tile",
                                           tags$p(class = "tile-k", "Studies"),
                                           tags$p(class = "tile-v", home_stats$n_studies),
                                           tags$p(class = "tile-s", "Unique study identifiers in the extraction sheet.")
                                  ),
                                  tags$div(class = "tile",
                                           tags$p(class = "tile-k", "Domains"),
                                           tags$p(class = "tile-v", home_stats$n_domains),
                                           tags$p(class = "tile-s", "Sleep-wake, activity type, activity intensity.")
                                  ),
                                  tags$div(class = "tile",
                                           tags$p(class = "tile-k", "Device brands"),
                                           tags$p(class = "tile-v", home_stats$n_devices),
                                           tags$p(class = "tile-s", "Tokenised from multi-valued device cells.")
                                  ),
                                  tags$div(class = "tile",
                                           tags$p(class = "tile-k", "Participants (summed)"),
                                           tags$p(class = "tile-v", format(home_stats$n_participants, big.mark = ",")),
                                           tags$p(class = "tile-s", "May double count across validations within the same study.")
                                  )
                         ),
                         
                         tags$div(class = "feature-grid",
                                  
                                  tags$div(class = "feature",
                                           tags$h4("At a glance"),
                                           tags$p(class = "inst-note",
                                                  "Validation rows by movement behaviour domain and prediction model group."
                                           ),
                                           uiOutput("home_breakdown"),
                                           tags$br(),
                                           tags$p(class = "inst-note",
                                                  "Interpretation is descriptive. Use the Forest Plot filters to define comparable subsets before drawing conclusions."
                                           )
                                  ),
                                  
                                  tags$div(class = "feature",
                                           tags$h4("Quick start workflow"),
                                           tags$ul(class = "stepper",
                                                   tags$li(class = "step", tags$b("1."), "Open Forest Plot and choose a metric (F1 recommended first)."),
                                                   tags$li(class = "step", tags$b("2."), "Filter by domain, model group, age group, and device brand."),
                                                   tags$li(class = "step", tags$b("3."), "Hover points for metadata and click to open DOI."),
                                                   tags$li(class = "step", tags$b("4."), "Use Dataset for context and to compare study design features.")
                                           ),
                                           tags$h4(style = "margin-top: 14px;", "Resources"),
                                           tags$p(class = "inst-note",
                                                  tags$a("Repository", href = "https://github.com/miltheo/SCOPE-MOVE", target = "_blank", rel = "noopener"),
                                                  " | Version 1.0.3 | GPL-3.0"
                                           ),
                                           
                                           tags$p(tags$b("Manuscript DOI: "),
                                                  tags$a("DOI link will be added here", href = "https://doi.org/XXXX", target = "_blank", rel = "noopener")),
                                           tags$p(class = "inst-note",
                                                     "If you are using SCOPE-MOVE in your work, please cite the manuscript and the app version."
                                           ),
                                           tags$p(tags$b("Author and maintainer: "), "Millen J. Theophilus")
                                  )
                         )
                ),
                
                
                tabPanel("Forest Plot",
                         
                         tags$div(class = "forest-grid",
                                  
                                  tags$div(class = "forest-sidebar",
                                           
                                           # div(class = "side-card", style = "text-align: center;",
                                           #     h4("Appearance"),
                                           #     radioButtons(
                                           #         "theme_mode", NULL,
                                           #         choices = c("System" = "system", "Light" = "light", "Dark" = "dark"),
                                           #         selected = "system",
                                           #         inline = TRUE
                                           #     )
                                           # ),
                                           
                                           div(class = "side-card",
                                               h4("Quick guide"),
                                               tags$ol(
                                                   tags$li(tags$b("Select a performance metric"), " (F1, sensitivity, specificity). Use F1 for an overall performance assessment and sensitivity or specificity to include more studies and validations."),
                                                   tags$li(tags$b("Apply filters"), " for domain, prediction model group, age group, and wearable brand."),
                                                   tags$li(tags$b("Interpret the plot"), " where each point is a validation and whiskers show its 95% CI."),
                                                   tags$li(tags$b("Row labels"), " are in the format first author and year (study) \u2013 prediction model method \u2013 unique validation ID."),
                                                   tags$li(tags$b("Vertical dashed line"), " represents the pooled standardised estimate for the filtered validations, using the selected pooling method."),
                                                   tags$li(tags$b("Horizontal dashed line"), " separates validations that exceed the pooled standardised estimate for the filtered subset."),
                                                   tags$li(tags$b("Pooling method"), " can be set to a simple random-effects meta-regression model (default) or a manuscript-aligned meta-regression with covariates (see Instructions)."),
                                                   tags$li(tags$b("Click a point"), " to open the DOI link.")
                                               ),
                                               p(class = "inst-note",
                                                 "The plot will resize based on the filters selected.")
                                           ),
                                           
                                           div(class = "side-card",
                                               selectInput("metric", "Validation performance metric:", choices = metric_choices, selected = default_metric),
                                               uiOutput("metric_def"),
                                               checkboxGroupInput("domain", "24-hour movement behaviour domain:", choices = domain_choices, selected = domain_choices),
                                               checkboxGroupInput("method_group", "Prediction model group:", choices = method_choices, selected = method_choices),
                                               uiOutput("method_def"),
                                               selectInput("age_group", "Age group:", choices = c("All" = "ALL", age_choices), selected = "ALL"),
                                               selectInput("test_device", "Wearable device brand:", choices = c("All" = "ALL", device_choices), selected = "ALL")
                                           ),
                                           
                                           div(class = "side-card",
                                               h4("Pooled estimate"),
                                               radioButtons(
                                                   "pool_method", "Pooled line method:",
                                                   choices = c(
                                                       "Simple meta-regression (no covariates)" = "simple",
                                                       "Manuscript meta-regression" = "manu"
                                                   ),
                                                   selected = "simple",
                                                   inline = FALSE
                                               )
                                           )
                                  ),
                                  
                                  tags$div(class = "forest-main",
                                           tags$div(class = "forest-plotwrap",
                                                    uiOutput("forest_plot_ui")
                                           ),
                                           tags$div(class = "forest-pooledwrap",
                                                    uiOutput("pooled_box")
                                           )
                                  )
                         )
                ),
                
                tabPanel("Dataset",
                         div(style = "height: calc(100vh - 140px); overflow: auto;", br(), DTOutput("dataset_table"))
                ),
                
                tabPanel("Instructions",
                         tags$div(class = "inst-wrap",
                                  
                                  tags$div(class = "hero",
                                           tags$div(class = "hero-title", "Instructions"),
                                           tags$p(class = "hero-sub",
                                                  "This page explains what is displayed, how to navigate the Forest Plot, and how to interpret validation metrics and pooled estimates."
                                           )
                                  ),
                                  
                                  tags$details(class = "details", open = TRUE,
                                               tags$summary("How to use SCOPE-MOVE"),
                                               tags$div(class = "details-body muted-body",
                                                        tags$ol(
                                                            tags$li(tags$b("Select a metric"), " (F1, sensitivity, specificity)."),
                                                            tags$li(tags$b("Filter validations"), " by domain, prediction model group, age group, and device brand."),
                                                            tags$li(tags$b("Inspect validations"), " using hover text; whiskers show 95% CI for each validation estimate."),
                                                            tags$li(tags$b("Open DOI"), " by clicking a point to review the source paper."),
                                                            tags$li(tags$b("Use Dataset"), " to examine study-level metadata and extraction context.")
                                                        ),
                                                        tags$p(class = "muted",
                                                               "Tokenisation is used for multi-valued age and device cells. Rows are retained if any token matches the selected value."
                                                        )
                                               )
                                  ),
                                  
                                  tags$details(class = "details",
                                               tags$summary("Movement behaviour domains"),
                                               tags$div(class = "details-body",
                                                        tags$p(class = "muted",
                                                               "Domains align with the scoping review extraction framework and the criterion reference used in each validation."
                                                        ),
                                                        tags$dl(class = "inst-kv",
                                                                tags$dt("Activity intensity"), tags$dd(domain_defs[["Activity Intensity"]]),
                                                                tags$dt("Activity type"), tags$dd(domain_defs[["Activity Type"]]),
                                                                tags$dt("Sleep-wake"), tags$dd(domain_defs[["Sleep/Wake"]])
                                                        )
                                               )
                                  ),
                                  
                                  tags$details(class = "details",
                                               tags$summary("Performance metrics and model groups"),
                                               tags$div(class = "details-body",
                                                        tags$div(class = "inst-grid",
                                                                 tags$div(
                                                                     tags$h5("Performance metrics"),
                                                                     tags$dl(class = "inst-kv",
                                                                             tags$dt("F1"), tags$dd("Harmonic mean of precision and sensitivity (macro-averaged across classes). Higher is better."),
                                                                             tags$dt("Sensitivity"), tags$dd("True positive rate (recall). Higher is better."),
                                                                             tags$dt("Specificity"), tags$dd("True negative rate. Higher is better.")
                                                                     )
                                                                 ),
                                                                 tags$div(
                                                                     tags$h5("Prediction model groups"),
                                                                     tags$dl(class = "inst-kv",
                                                                             tags$dt("Traditional"), tags$dd(method_help[["Traditional"]]),
                                                                             tags$dt("Non-traditional"), tags$dd(method_help[["Non-traditional"]])
                                                                     )
                                                                 )
                                                        )
                                               )
                                  ),
                                  
                                  tags$details(class = "details",
                                               tags$summary("Pooled estimates"),
                                               tags$div(class = "details-body muted-body",
                                                        tags$ul(
                                                            tags$li("Shown only when the filtered subset has at least 3 validation rows."),
                                                            tags$li(tags$b("Simple meta-regression (no covariates):"),
                                                                    tags$br(),
                                                                    tags$span("A conventional random-effects meta-regression (REML) with no covariates, pooled across the selected subset.")
                                                            ),
                                                            tags$li(tags$b("Manuscript meta-regression:"),
                                                                    tags$br(),
                                                                    tags$span("A two-level participant-weighted random-effects meta-regression (REML) with a study- and validation-level random intercept inference used to compute a standardised pooled estimate for the selected subset."),
                                                                    tags$br(),
                                                                    tags$span("Meta-regression covariates were centred number of behaviour classes and validation folds with missingness indicators, test environment (3 level: free-living, laboratory, hybrid), test protocol (3 level: structured, unstructured, hybrid), and behaviour domain (3 level: activity intensity, activity type, sleep/wake)."),
                                                                    tags$br(),
                                                                    tags$span("Any covariate that is constant (or effectively constant due to missingness) is removed from the model formula.")
                                                            )
                                                        )
                                               )
                                  ),
                                  
                                  tags$details(class = "details",
                                               tags$summary("How are the plotted estimates defined and how is the variance computed?"),
                                               tags$div(class = "details-body muted-body",
                                                                 tags$div(
                                                                     tags$ul(
                                                                         tags$li("Each plotted point is the extracted validation performance ", tags$b("metric"), " for that validation. The ", tags$b("metrics"), " of interest were F1, sensitivity, and specificity (definitions above)."),
                                                                         tags$li("The ", tags$b("metrics"), " were extracted as the unweighted mean and standard deviation (SD) across behaviour classes, when reported."),
                                                                         tags$li("When mean and SD were not reported and class-specific performance was reported (e.g. separately for sitting vs walking vs running), metrics were computed via macro-averaging across the classes to obtain an unweighted mean and SD."),
                                                                         tags$li("When the confusion matrix was provided with validation counts for each behaviour class, metrics were computed and macro-averaged across classes via the 'caret' package in R to obtain an unweighted mean and SD."),
                                                                         tags$li("Extracted SDs were then converted to sampling variances and divided by cross-validation (CV) folds when SDs reflected CV or by number of behaviour classes when SDs reflected class variation; when SDs were unavailable, a lower-bounded behaviour domain-specific median constant variance was used to stabilise weights."),
                                                                         tags$li("The constructed variance was then used to build a 95% confidence interval (CI)."),
                                                                         tags$li("Each point therefore is the reported or computed ", tags$b("metric"), " for a specific validation and the whisker is the computed 95% CI for that specific estimate."),
                                                                         tags$li("For meta-analysis, ", tags$b("metrics"), " are analysed on the logit scale and back-transformed to proportions for display."),
                                                                         tags$li("Extraction rules, transformations, and any further imputation decisions are documented in the manuscript.")
                                                                 )
                                                        )
                                               )
                                  ),
                                  
                                  tags$details(class = "details",
                                               tags$summary("Hover text fields"),
                                               tags$div(class = "details-body",
                                                        tags$p(class = "muted", "Hover a point to view validation metadata."),
                                                        tags$div(class = "inst-grid",
                                                                 tags$div(
                                                                     tags$h5("Protocol"),
                                                                     tags$dl(class = "inst-kv",
                                                                             tags$dt("Structured"), tags$dd(protocol_defs[["Structured"]]),
                                                                             tags$dt("Unstructured"), tags$dd(protocol_defs[["Unstructured"]]),
                                                                             tags$dt("Hybrid"), tags$dd(protocol_defs[["Hybrid"]])
                                                                     )
                                                                 ),
                                                                 tags$div(
                                                                     tags$h5("Environment"),
                                                                     tags$dl(class = "inst-kv",
                                                                             tags$dt("Laboratory"), tags$dd(environment_defs[["Laboratory"]]),
                                                                             tags$dt("Free-living"), tags$dd(environment_defs[["Free-living"]]),
                                                                             tags$dt("Hybrid"), tags$dd(environment_defs[["Hybrid"]])
                                                                     )
                                                                 )
                                                        ),
                                                        tags$div(class = "inst-grid",
                                                                 tags$div(
                                                                     tags$h5("Validation type"),
                                                                     tags$dl(class = "inst-kv",
                                                                             tags$dt("Hold-out"), tags$dd(validation_defs[["Hold-out"]]),
                                                                             tags$dt("Cross-validation"), tags$dd(validation_defs[["Cross-validation"]]),
                                                                             tags$dt("External"), tags$dd(validation_defs[["External"]]),
                                                                             tags$dt("Apparent"), tags$dd(validation_defs[["Apparent"]])
                                                                     )
                                                                 ),
                                                                 tags$div(
                                                                     tags$h5("Other fields"),
                                                                     tags$dl(class = "inst-kv",
                                                                             tags$dt("Estimate and 95% CI"), tags$dd("Extracted or computed for the specific validation."),
                                                                             tags$dt("Age group and device"), tags$dd("Taken from the extraction sheet; tokenised on ';'."),
                                                                             tags$dt("N participants"), tags$dd("Participants contributing to that validation (as extracted)."),
                                                                             tags$dt("DOI"), tags$dd("Clicking a point opens the DOI link.")
                                                                     )
                                                                 )
                                                        )
                                               )
                                  ),
                                  
                                  tags$details(class = "details",
                                               tags$summary("Pooled estimate line and interpretation cautions"),
                                               tags$div(class = "details-body",
                                                        tags$ul(
                                                            tags$li("Shown only when the filtered subset includes at least 3 validation rows."),
                                                            tags$li("The dashed vertical line summarises the selected subset and can mask heterogeneity across behaviour classes, protocols, and populations."),
                                                            tags$li("Not all studies report all metrics. Missingness reflects reporting, not exclusion by the app.")
                                                        ),
                                                        tags$p(class = "muted",
                                                               "Use stratified filtering and interpret within-domain subsets when comparing model groups."
                                                        )
                                               )
                                  )
                         )
                )
                
    ),
    
    tags$div(
        id = "theme_fab",
        class = "theme-fab",
        `aria-label` = "Toggle theme",
        tags$span(class = "ico", id = "theme_fab_ico", "\u263C"),  # sun symbol
        tags$span(class = "tip", id = "theme_fab_tip", "Theme: System")
    )
    
    
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
    
    # observeEvent(input$theme_mode, { session$sendCustomMessage("set_theme", input$theme_mode) }, ignoreInit = FALSE)
    
    output$metric_def <- renderUI({
        txt <- metric_help[[as.character(input$metric)]]
        if (is.null(txt)) return(NULL)
        tags$div(class = "forest-caption", tags$b("Metric definition: "), txt)
    })
    
    output$method_def <- renderUI({
        sel <- as.character(input$method_group)
        if (!length(sel)) return(NULL)
        blocks <- lapply(sel, \(k) tags$li(tags$b(paste0(k, ": ")), method_help[[k]] %||% ""))
        tags$div(class = "forest-caption", tags$b("Model group definitions:"), tags$ul(style = "margin-bottom: 0;", blocks))
    })
    
    filtered_data <- reactive({
        df <- df_all %>% filter(metric %in% input$metric, domain %in% input$domain, method_group %in% input$method_group)
        if (input$age_group != "ALL")   df <- df[vapply(df$age_tokens,    \(v) input$age_group   %in% v, logical(1)), ]
        if (input$test_device != "ALL") df <- df[vapply(df$device_tokens, \(v) input$test_device %in% v, logical(1)), ]
        df
    })
    
    plot_h <- reactive({
        df <- filtered_data()
        n <- nrow(df)
        row_px <- 18
        pad_px <- 140
        content <- max(420, n * row_px + pad_px)
        win_h <- as.numeric(input$win_h %||% 800)
        avail <- max(420, win_h - 260)
        max(content, avail)
    })
    
    output$forest_plot_ui <- renderUI({ plotlyOutput("forest_plot", height = paste0(plot_h(), "px")) })
    
    output$forest_plot <- renderPlotly({
        df <- filtered_data()
        pooled_val <- NA_real_
        if (nrow(df) >= 3) {
            if (input$pool_method == "manu") {
                fit <- fit_metric_manuscript(df)
                if (!is.null(fit)) pooled_val <- pooled_standardized_manuscript(fit, df)["est"]
            } else {
                pooled_val <- pooled_standardized_minimal(fit_metric_minimal(df))["est"]
            }
        }
        
        is_dark <- isTRUE(input$theme_effective == "dark")
        hover_bg  <- if (is_dark) "rgba(20,24,32,0.95)" else "rgba(255,253,238,0.98)"
        hover_txt <- if (is_dark) "#ffffff" else "#111111"
        plot_txt  <- if (is_dark) "#f2f3f5" else "#111111"
        
        ggplotly(make_forest_plot(df, pooled_val, dark = is_dark), tooltip = "text", source = "forest") %>%
            layout(
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor  = "rgba(0,0,0,0)",
                hoverlabel = list(bgcolor = hover_bg, font = list(color = hover_txt)),
                font = list(color = plot_txt),
                margin = list(l = 250, r = 25, t = 10, b = 45),
                yaxis = list(
                    title = list(text = "Model Validations", standoff = 35, font = list(color = plot_txt)),
                    tickfont = list(color = plot_txt),
                    automargin = TRUE
                ),
                xaxis = list(
                    title = list(font = list(color = plot_txt)),
                    tickfont = list(color = plot_txt)
                ),
                legend = list(font = list(color = plot_txt))
            )
    })
    
    output$pooled_box <- renderUI({
        df <- filtered_data()
        if (nrow(df) < 3) {
            return(div(class = "pooled-card",
                       tags$h4(class = "pooled-title", "Pooled estimate"),
                       tags$p(class = "pooled-sub", "Not enough rows for meta-analysis (N < 3).")))
        }
        
        n_rows <- nrow(df)
        n_stud <- dplyr::n_distinct(df$study_id)
        n_part <- sum(safe_n(df$n_participants), na.rm = TRUE)
        n_part_rep <- sum(is.finite(safe_n(df$n_participants)))
        
        if (input$pool_method == "manu") {
            fit <- fit_metric_manuscript(df)
            if (is.null(fit)) {
                return(div(class = "pooled-card",
                           tags$h4(class = "pooled-title", "Pooled estimate"),
                           tags$p(class = "pooled-sub", "Model failed to converge for this subset.")))
            }
            pooled <- pooled_standardized_manuscript(fit, df)
            method_lab <- "Manuscript meta-regression"
            mods_lab <- pretty_mods(fit$kept_mods)
            clust_lab <- fit$clusters
            est_ci_lab <- if (is.finite(pooled["lb"])) sprintf("%.1f%% (%.1f to %.1f%%)", 100*pooled["est"], 100*pooled["lb"], 100*pooled["ub"]) else sprintf("%.1f%% (CI n.a.)", 100*pooled["est"])
        } else {
            pooled <- pooled_standardized_minimal(fit_metric_minimal(df))
            method_lab <- "Simple REML (no covariates)"
            mods_lab <- "None"
            clust_lab <- n_stud
            est_ci_lab <- sprintf("%.1f%% (%.1f to %.1f%%)", 100*pooled["est"], 100*pooled["lb"], 100*pooled["ub"])
        }
        
        div(class = "pooled-card",
            tags$h4(class = "pooled-title", "Pooled estimate"),
            tags$p(class = "pooled-sub", paste0("Dashed line = pooled standardised estimate using ", method_lab, ".")),
            div(class = "pooled-grid",
                div(class = "pooled-kv", tags$p(class = "pooled-k", "Estimate (95% CI)"), tags$p(class = "pooled-v", est_ci_lab)),
                div(class = "pooled-kv", tags$p(class = "pooled-k", "Participants (summed)"), tags$p(class = "pooled-v", format(n_part, big.mark = ","))),
                div(class = "pooled-kv", tags$p(class = "pooled-k", "Validation rows, Studies"), tags$p(class = "pooled-v", paste0(n_rows, ", ", clust_lab)))
            ),
            tags$div(class = "pooled-small",
                     tags$div(class = "pooled-pill", paste0("Rows with participants reported: ", n_part_rep, "/", n_rows)),
                     tags$div(class = "pooled-pill", paste0("Metric: ", as.character(input$metric))),
                     tags$div(class = "pooled-pill", paste0("Domains: ", length(input$domain))),
                     tags$div(class = "pooled-pill", paste0("Model groups: ", length(input$method_group)))),
            tags$div(class = "pooled-small", tags$b("Covariates: "), mods_lab, tags$br())
        )
    })
    
    observeEvent(plotly::event_data("plotly_click", source = "forest"), {
        ed <- plotly::event_data("plotly_click", source = "forest")
        url <- df_all$doi_url[match(ed$key, df_all$val_id)]
        if (is.na(url) || !nzchar(url)) return(NULL)
        showModal(modalDialog(title = "DOI link, click to open in another tab.",
                              tags$a(url, href = url, target = "_blank", rel = "noopener"),
                              easyClose = TRUE, footer = modalButton("Close")))
    })
    
    output$dataset_table <- renderDT({
        datatable(minimal_df,
                  options = list(pageLength = 20, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE, filter = "top")
    })
    
    # Jump to key tabs from Home
    observeEvent(input$go_forest,        updateTabsetPanel(session, "tabs", selected = "Forest Plot"))
    observeEvent(input$go_instructions,  updateTabsetPanel(session, "tabs", selected = "Instructions"))
    observeEvent(input$go_dataset,       updateTabsetPanel(session, "tabs", selected = "Dataset"))
    
    
    output$home_breakdown <- renderUI({
        d <- df_all %>%
            dplyr::distinct(val_id, domain, method_group) %>%
            dplyr::count(domain, method_group, name = "n")
        
        dom_tot <- d %>% dplyr::group_by(domain) %>% dplyr::summarise(n_dom = sum(n), .groups = "drop")
        d2 <- d %>% dplyr::left_join(dom_tot, by = "domain") %>% dplyr::mutate(p = ifelse(n_dom > 0, n / n_dom, 0))
        
        # fixed colours for model groups (subtle, consistent)
        col_trad <- "#0072B2"  # Okabe-Ito blue
        
        col_non  <- "#009E73"  # Okabe-Ito bluish green
        
        blocks <- lapply(domain_levels, function(dom){
            dd <- d2 %>% dplyr::filter(domain == dom)
            n_dom <- dd$n_dom[1] %||% 0
            
            n_tr <- dd$n[dd$method_group == "Traditional"] %||% 0
            n_nt <- dd$n[dd$method_group == "Non-traditional"] %||% 0
            
            p_tr <- dd$p[dd$method_group == "Traditional"] %||% 0
            p_nt <- dd$p[dd$method_group == "Non-traditional"] %||% 0
            
            tags$div(class = "break-row",
                     tags$div(class = "break-top",
                              tags$div(class = "break-dom", dom),
                              tags$div(class = "break-n", paste0("N = ", n_dom))
                     ),
                     tags$div(class = "hbar",
                              tags$div(class = "hseg", style = paste0("width:", 100*p_tr, "%; background:", col_trad, ";")),
                              tags$div(class = "hseg", style = paste0("width:", 100*p_nt, "%; background:", col_non, ";"))
                     ),
                     tags$div(class = "hlab",
                              tags$span(tags$span(class = "dot", style = paste0("background:", col_trad, ";")), paste0("Traditional: ", n_tr)),
                              tags$span(tags$span(class = "dot", style = paste0("background:", col_non, ";")), paste0("Non-traditional: ", n_nt))
                     )
            )
        })
        
        tags$div(class = "breakdown", blocks)
    })
    
}

# ---------------------------
# RUN APP
# ---------------------------
shinyApp(ui = ui, server = server)