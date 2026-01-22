# ======================================================================
# SCOPE-MOVE v1.0.1
# - Code for the shiny web app, SCOPE-MOVE.
# - Code version 4.5 21Jan26
# - Author: Millen J. Theophilus
# =================

rm(list = ls()); gc()                                                                           # clean session

# ---------------------------
# PACKAGES
# ---------------------------
library(shiny)                                                                                  # app framework
library(dplyr)                                                                                  # data wrangling
library(ggplot2)                                                                                # plotting
library(forcats)                                                                                # factor helpers
library(readr)                                                                                  # csv import
library(metafor)                                                                                # meta-analysis
library(plotly)                                                                                 # interactive plots
library(DT)                                                                                     # tables
library(scales)                                                                                 # axis formatting
library(clubSandwich)

## Packages --------------------------------------------------------------------
# packages <- c("shiny", "dplyr", "ggplot2", "forcats","readr","metafor","plotly","DT","scales","clubSandwich")
# 
# for (pkg in packages) {
#     if (!requireNamespace(pkg, quietly = TRUE)) {
#         install.packages(pkg)
#     }
#     library(pkg, character.only = TRUE)
# }
# ---------------------------
# PATHS
# ---------------------------
# setwd("C:/GitHub/SCOPE-MOVE/app/data/") # Comment out before deploying to shinyapps.io

forest_path <- "data/REML_data_long.csv"
master_path <- "data/Extraction_models_master_sheet.csv"

# ---------------------------
# HELPERS (meta-analysis, proportions on logit scale)
# ---------------------------
logit_safe <- function(p, eps = 1e-6) qlogis(pmax(pmin(p, 1 - eps), eps))                       # safe logit transform
inv_logit  <- function(x) plogis(x)                                                            # inverse logit transform

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
safe_n <- function(x) suppressWarnings(as.numeric(x))                                            # parse N safely


# ---------------------------
# HELPERS (pooled estimate with fallback + descriptive breakdown)
# ---------------------------
fit_metric_minimal <- function(dfm) rma.uni(yi = dfm$yi, vi = dfm$vi_use, method = "REML")      # random-effects model

pooled_standardized_minimal <- function(fit) {                                                  # pooled est + 95% CI on proportion scale
    est <- as.numeric(fit$b[1]); se <- as.numeric(fit$se[1])
    c(est = inv_logit(est), lb = inv_logit(est - 1.96 * se), ub = inv_logit(est + 1.96 * se))
}

clamp_prob <- function(p, eps = 1e-3) pmin(pmax(p, eps), 1 - eps)                              # avoid 0/1 in gradients

fit_metric_manuscript <- function(dfm) {                                                        # rma.mv meta-regression core
    if (nrow(dfm) < 3) return(NULL)                                                             # need enough rows
    dfm <- dfm %>%                                                                              # ensure factors for rma.mv
        mutate(study_id = factor(study_id), val_id = factor(val_id))
    
    base_mods <- c("c_n_classes","c_k_folds","miss_n_classes","miss_k_folds")                   # always candidates
    add_mods  <- c("env_re","prot_re","dom_re")                                                 # manuscript covariates
    X <- dfm %>% dplyr::select(any_of(c(base_mods, add_mods)))                                  # candidate design cols
    
    nonconst <- vapply(X, \(v) length(unique(v[!is.na(v)])) > 1, logical(1))                    # drop constants
    kept <- names(X)[nonconst]                                                                  # keep informative mods
    fml <- if (length(kept)) as.formula(paste("~", paste(kept, collapse = " + "))) else ~ 1     # build formula
    
    m <- try(metafor::rma.mv(yi = yi, V = vi_use, mods = fml,                                   # fit rma.mv
                             random = list(~1 | study_id, ~1 | val_id),
                             data = dfm, method = "REML",
                             control = list(optimizer = "optim")), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)                                                  # fail-safe
    
    list(model = m, kept_mods = kept, clusters = dplyr::n_distinct(dfm$study_id))               # return fit info
}

pooled_standardized_manuscript <- function(fit, dfm, weight_by_n = TRUE, n_col = "n_participants", eps = 1e-3) {
    X <- as.matrix(fit$model$X)                                                                 # design matrix
    beta <- as.numeric(fit$model$beta)                                                          # coefficients
    if (!is.null(colnames(X)) && !is.null(names(fit$model$beta))) {                             # align cols if named
        X <- X[, names(fit$model$beta), drop = FALSE]
    }
    eta <- as.numeric(X %*% beta)                                                               # linear predictors
    p_i <- clamp_prob(plogis(eta), eps = eps)                                                    # predicted probs
    
    if (weight_by_n && n_col %in% names(dfm) && any(is.finite(dfm[[n_col]]))) {                 # participant weights
        w <- dfm[[n_col]]
    } else {
        w <- rep(1, nrow(dfm))
    }
    w <- w / sum(w, na.rm = TRUE)                                                               # normalise weights
    est <- sum(w * p_i, na.rm = TRUE)                                                           # standardised mean
    
    vc_type <- if (fit$clusters >= 6) "CR2" else "CR1S"                                         # robust type
    V <- try(clubSandwich::vcovCR(fit$model, type = vc_type, cluster = dfm$study_id), silent = TRUE)
    if (inherits(V, "try-error") || any(!is.finite(V))) {                                       # fallback if needed
        V <- try(stats::vcov(fit$model), silent = TRUE)
    }
    if (inherits(V, "try-error") || any(!is.finite(V))) return(c(est = est, lb = NA, ub = NA))  # no CI
    
    J <- colSums((w * p_i * (1 - p_i)) * X, na.rm = TRUE)                                       # delta gradient
    se <- sqrt(drop(J %*% V %*% J))                                                             # robust SE
    if (!is.finite(se) || se < 1e-8) return(c(est = est, lb = NA, ub = NA))                     # degenerate
    
    lb <- max(est - 1.96 * se, 0)                                                               # CI lower (clamped)
    ub <- min(est + 1.96 * se, 1)                                                               # CI upper (clamped)
    c(est = est, lb = lb, ub = ub)
}


# ---------------------------
# HELPERS (tokenisation for multi-valued cells)
# ---------------------------
split_tokens <- function(x) {                                                                   # split ';' cells into clean tokens
    x <- ifelse(is.na(x), "", as.character(x))
    lapply(strsplit(x, ";", fixed = TRUE), \(v) unique(trimws(v[nzchar(trimws(v))])))
}
tokens_to_choices <- function(tok_list) sort(unique(unlist(tok_list, use.names = FALSE)))      # unique tokens for dropdowns

# ---------------------------
# LOAD DATA
# ---------------------------
df_all    <- read_csv(forest_path, show_col_types = FALSE)                                      # forest plot rows (per validation)
df_master <- read_csv(master_path, show_col_types = FALSE)                                      # master metadata

# ---------------------------
# RESTRICT TO CLASSIFIER METRICS (prototype scope)
# ---------------------------
keep_metrics <- c("F1", "Sensitivity", "Specificity")                                           # classifier-only metrics
df_all <- df_all %>% filter(metric %in% keep_metrics)                                           # drop non-classifier rows

df_all <- df_all %>% mutate(env_re = factor(env_re), prot_re = factor(prot_re), dom_re = factor(dom_re))  # lock references

# ---------------------------
# STANDARDISE FOREST ROWS (effects + labels)
# ---------------------------
df_all <- df_all %>%
    mutate(
        val_id       = trimws(as.character(val_id)),                                            # join key
        metric       = factor(metric, levels = keep_metrics),                                   # UI field (ordered)
        domain       = factor(domain),                                                          # UI field
        method_group = factor(recode(factor(method_group), "Threshold" = "Traditional", "Other" = "Non-traditional"),
                              levels = c("Traditional", "Non-traditional")),                    # UI field
        row_lab      = paste0(study_id, " - ", method, " - ", val_id),                           # y labels
        yi           = logit_safe(est),                                                         # effect size (logit)
        # vi_use       = ((est - ci_l) / 1.96)^2                                                  # variance from CI width
        vi_use       = as.numeric(vi_use)
    )

# ---------------------------
# BUILD MASTER LOOKUPS (age/device + DOI)
# ---------------------------
df_master <- df_master %>% mutate(val_id = trimws(as.character(val_id)))                        # join key

align_df <- df_master %>%
    transmute(
        val_id,
        age_group_m   = trimws(as.character(age_group)),                                        # may be ';' separated
        test_device_m = trimws(as.character(test_device))                                       # may be ';' separated
    )

doi_df <- df_master %>%
    transmute(val_id, doi_raw = trimws(as.character(doi))) %>%
    mutate(
        doi_raw = sub("^https?://(dx\\.)?doi\\.org/", "", doi_raw),                              # strip doi prefix
        doi_url = ifelse(nzchar(doi_raw), paste0("https://doi.org/", doi_raw), NA_character_)   # safe DOI URL
    )

# ---------------------------
# ALIGN + TOKENISE + TOOLTIP TEXT
# ---------------------------
df_all <- df_all %>%
    mutate(
        age_group   = trimws(as.character(age_group)),
        test_device = trimws(as.character(test_device))
    ) %>%
    left_join(align_df, by = "val_id") %>%
    mutate(
        age_group        = coalesce(age_group_m, age_group),                                    # prefer master
        test_device      = coalesce(test_device_m, test_device),                                # prefer master
        age_tokens       = split_tokens(age_group),                                             # list-column
        device_tokens    = split_tokens(test_device),                                           # list-column
        age_group_show   = age_group,                                                           # tooltip
        test_device_show = test_device                                                          # tooltip
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
    n_validations = length(unique(df_all$val_id)),
    n_studies = length(unique(df_all$id)),
    n_domains = length(levels(df_all$domain)),
    n_devices = length(unique(unlist(df_all$device_tokens))),
    n_participants = sum(safe_n(df_all$n_participants), na.rm = TRUE)
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
# FOREST PLOTTER (spacing adjustments)
# ---------------------------
# okabe_ito <- c("#CC79A7","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#000000")
okabe_ito <- c("#CC79A7","#E69F00","#56B4E9")

domain_levels <- c("Sleep","Activity type","Activity intensity") 
domain_pal <- setNames(okabe_ito[seq_along(domain_levels)], domain_levels)

pretty_mods <- function(mods) {                                                                  # map moderator codes to readable labels
    if (is.null(mods) || !length(mods)) return("(intercept only)")                               # handle empty
    map <- c(                                                                                    # label map
        c_n_classes     = "Number of classes (centred)",
        c_k_folds       = "Number of CV folds (centred)",
        miss_n_classes  = "Missing number of classes indicator",
        miss_k_folds    = "Missing number of CV folds indicator",
        env_re          = "Test environment",
        prot_re         = "Test protocol",
        dom_re          = "Movement behaviour domain"
    )
    out <- unname(ifelse(mods %in% names(map), map[mods], mods))                                  # replace where possible
    paste(out, collapse = ", ")                                                                  # join nicely
}


make_forest_plot <- function(df, pooled_val = NA_real_) {
    df <- df %>% arrange(est) %>% mutate(row_lab = fct_inorder(row_lab))
    split_at <- NA_real_
    if (is.finite(pooled_val)) {
        below_idx <- which(df$est < pooled_val)
        if (length(below_idx) > 0 && length(below_idx) < nrow(df)) split_at <- max(below_idx) + 0.5
    }
    
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
            panel.grid.minor = element_blank(),
            legend.title.position = "top",
            axis.text.y  = element_text(margin = margin(r = 10, unit = "pt")),
            plot.margin  = margin(t = 8, r = 10, b = 8, l = 10)
        ) +
        { if (is.finite(pooled_val)) geom_vline(xintercept = pooled_val, linetype = 2, linewidth = 0.5) } +
        { if (is.finite(split_at))  geom_hline(yintercept = split_at, linetype = "dashed", linewidth = 0.3, colour = "grey50") } +
        scale_colour_manual(
            name   = "Domains",
            values = domain_pal,
            breaks = domain_levels,
            drop   = FALSE
        )
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
# SIDEBAR DEFINITIONS (classifier-only)
# ---------------------------
metric_help <- list(
    "F1"          = "Harmonic mean of precision and sensitivity (recall). Higher is better.",
    "Sensitivity" = "True positive rate (recall). Proportion of true positives correctly identified. Higher is better.",
    "Specificity" = "True negative rate. Proportion of true negatives correctly identified. Higher is better."
)

method_help <- list(
    "Traditional"     = "Threshold, heuristic rule-based, and general linear model approaches. Typically simpler and more interpretable.",
    "Non-traditional" = "Classical machine learning, deep learning, probabilistic temporal models, and ensembles."
)

# ---------------------------
# INSTRUCTIONS (your expanded tab content)
# ---------------------------
domain_defs <- list(
    "Activity Intensity" = "Time spent in physical activity intensity categories (e.g., sedentary, light, moderate, vigorous), benchmarked against indirect calorimetry and expressed via MET-based thresholds.",
    "Activity Type"      = "Discrete activity classes (e.g., walking, running, cycling, sedentary), benchmarked against direct or video observation.",
    "Sleep/Wake"         = "Binary sleep versus wake classification, benchmarked against polysomnography (PSG)."
)

protocol_defs <- list(
    "Structured"      = "A highly controlled protocol (often laboratory-based), where activities and timing are prescribed with limited participant autonomy.",
    "Unstructured"    = "A free-living or naturalistic protocol, where behaviour is not prescribed and participants follow self-selected routines.",
    "Hybrid"          = "A mixed design combining limited structure and participant autonomy, where participants follow self-selected routines within constrains set by the researchers (timing and/or activity choice)."
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
# UI
# ---------------------------
ui <- fluidPage(
  
    # HTML metadata
    tags$head(
      tags$title("SCOPE-MOVE")
    ),
    
    # CSS: fill viewport, pooled-box font size, instructions styling
    tags$head(tags$style(HTML("
        html, body { height: 100%; }
        .container-fluid { height: 100%; }
        .tab-content { padding-bottom: 8px; padding-top: 2px; padding-right: 8px; padding-left: 8px; }
        .nav-tabs { margin-bottom: 10px; }
        .side-card { background: #fafafa; border: 1px solid #e6e6e6; border-radius: 10px; padding: 12px 14px; margin-top: 12px; }
        .side-card h4 { margin-top: 2px; margin-bottom: 8px; }
        .side-card ul { margin-bottom: 0; padding-left: 18px; }
        .inst-wrap { max-width: 1100px; margin: 10px auto 30px auto; }
        .inst-card { background: #fafafa; border: 1px solid #e6e6e6; border-radius: 10px; padding: 16px 18px; margin-bottom: 14px; }
        .inst-card h4 { margin-top: 0; }
        .inst-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 14px; }
        .inst-kv dt { font-weight: 600; margin-top: 8px; }
        .inst-kv dd { margin-left: 0; margin-bottom: 6px; color: #444; }
        .inst-note { color: #555; }
        @media (max-width: 900px) { .inst-grid { grid-template-columns: 1fr; } }
        
        .pooled-card { background: #fafafa; border: 1px solid #e6e6e6; border-radius: 10px; padding: 10px 12px; }
        .pooled-title { margin: 0 0 6px 0; font-weight: 700; }
        .pooled-sub { color: #555; margin: 0 0 10px 0; }
        .pooled-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 8px 12px; }
        .pooled-kv { background: #ffffff; border: 1px solid #eeeeee; border-radius: 10px; padding: 8px 10px; }
        .pooled-k { color: #666; font-size: 12px; margin: 0 0 4px 0; }
        .pooled-v { font-size: 16px; font-weight: 700; margin: 0; }
        .pooled-small { font-size: 12px; color: #555; margin-top: 8px; line-height: 1.35; }
        .pooled-pill { display: inline-block; padding: 2px 8px; border-radius: 999px; border: 1px solid #e6e6e6; background: #fff; font-size: 12px; margin-right: 6px; margin-bottom: 6px; }
        @media (max-width: 900px) { .pooled-grid { grid-template-columns: 1fr; } }
        
        /* Forest Plot tab: stable 2-column layout */
        .forest-grid { display: grid; grid-template-columns: minmax(340px, 26vw) 1fr; gap: 14px; align-items: start; height: calc(100vh - 165px); }
        
        /* stack on small screens */
        @media (max-width: 1050px){ .forest-grid{ grid-template-columns: 1fr; height: auto; } .forest-sidebar{ max-height: 520px; } .forest-main{ height: calc(100vh - 260px); } }

        
        /* Sidebar: scroll if needed, no dead space */
        .forest-sidebar{ height: 100%; overflow: auto; padding-right: 2px; }
        
        /* Right side: plot fills, pooled stays visible */
        .forest-main{ height: 100%; display: flex; flex-direction: column; min-width: 0; padding-bottom: 10px; }
        
        .forest-plotwrap{ flex: 1 1 auto; min-height: 420px; overflow: auto;  /* enables scrolling when plot is taller */
        }

        
        /* Pooled block is capped and scrolls internally */
        .forest-pooledwrap{ flex: 0 0 auto; max-height: 210px;     /* tune: 170 to 260 */
          overflow: auto; margin-top: 10px; padding-bottom: 10px; margin-bottom: 10px; }
        
        /* Make sidebar cards tighter and consistent */
        .side-card{ margin-top: 10px; }
    ")),
    
    tags$script(HTML("
      function reportWin(){
        Shiny.setInputValue('win_h', window.innerHeight, {priority: 'event'});
      }
      window.addEventListener('load', reportWin);
      window.addEventListener('resize', reportWin);
    "))
    
    ),
    
    titlePanel("SCOPE-MOVE: SCOPing Explorer of accelerometer-based prediction models for 24-hour MOVEment behaviour analysis"),
    
    tabsetPanel(
        
        # TAB 1: Home
        tabPanel("Home",
                 div(class = "inst-wrap",
                     
                     div(class = "inst-card",
                         h2("Welcome to SCOPE-MOVE"),
                         p("SCOPE-MOVE is an interactive dashboard for exploring validation performance of methods using raw (sample-level) wrist accelerometry data for 24-hour movement behaviour analysis."),
                         p(class = "inst-note",
                           "This app is an output of the associated scoping review and meta-analytical synthesis. Please read the Instructions tab before interpreting pooled estimates.")
                     ),
                     
                     div(class = "inst-grid",
                         
                         div(class = "inst-card",
                             h4("Quick start"),
                             tags$ol(
                                 tags$li(tags$b("Step 1:"), " Read the ", tags$b("Instructions"), " tab to understand 24-hour movement behaviour domains, criterion measures, protocols, environments, metrics, and validation types."),
                                 tags$li(tags$b("Step 2:"), " Go to ", tags$b("Forest Plot"), " and choose a metric (F1, sensitivity, specificity)."),
                                 tags$li(tags$b("Step 3:"), " Apply filters, then inspect individual validations using hover text."),
                                 tags$li(tags$b("Step 4:"), " Click a point to open the DOI and review the source paper or go to the ", tags$b("Dataset"), " for context.")
                             )
                         ),
                         
                         div(class = "inst-card",
                             h4("At a glance"),
                             tags$ul(
                                 tags$li(tags$b("Validations in app: "), home_stats$n_validations),
                                 tags$li(tags$b("Studies in app: "), home_stats$n_studies),
                                 tags$li(tags$b("24-hour Movement Behaviour Domains: "), home_stats$n_domains),
                                 tags$li(tags$b("Wearable brands: "), home_stats$n_devices),
                                 tags$li(tags$b("Participants (summed across validations): "), format(home_stats$n_participants, big.mark = ","))
                             ),
                             p(class = "inst-note",
                               "Validations are unique validations with metrics reported and can be more than one per study."),
                             p(class = "inst-note",
                               "Participant totals may double count across multiple validations from the same study. Use the Dataset tab for validation and study level details.")
                         )
                     ),
                     
                     div(class = "inst-card",
                         h4("Associated manuscript and resources"),
                         tags$ul(
                             tags$li(tags$b("Manuscript DOI: "),
                                     tags$a("DOI link will be added here", href = "https://doi.org/XXXX", target = "_blank", rel = "noopener")),
                             tags$li(tags$b("Repository: "),
                                     tags$a("github.com/miltheo/SCOPE-MOVE", href = "https://github.com/miltheo/SCOPE-MOVE", target = "_blank", rel = "noopener")),
                             tags$li(tags$b("App Version: "), "1.0.1"),
                             tags$li(tags$b("License: "), "GNU General Public License v3.0 (GPL-3.0)"),
                             tags$li(tags$b("License file: "),
                                     tags$a("LICENSE (GitHub repository root)",
                                            href = "https://github.com/miltheo/SCOPE-MOVE/blob/main/LICENSE",
                                            target = "_blank", rel = "noopener")),
                             tags$li(tags$b("Author and maintainer: "), "Millen J. Theophilus (GitHub: miltheo)"),
                         ),
                         p(class = "inst-note",
                           "If you are using SCOPE-MOVE in your work, please cite the manuscript and the app version.")
                     )
                 )
        ),
        
        
        # TAB 2: Forest plot
        
        tabPanel("Forest Plot",
                 
                 tags$div(class = "forest-grid",
                          
                          # LEFT: sidebar (scrollable)
                          tags$div(class = "forest-sidebar",
                                   div(class = "side-card",
                                       h4("Quick guide"),
                                       tags$ol(
                                           tags$li(tags$b("Select a performance metric"), " (F1, sensitivity, specificity). Use F1 for an overall performance assessment and sensitivity or specificity to include more studies and validations."),
                                           tags$li(tags$b("Apply filters"), " for domain, prediction model group, age group, and wearable brand."),
                                           tags$li(tags$b("Interpret the plot"), " where each point is a validation and whiskers show its 95% CI."),
                                           tags$li(tags$b("Row labels"), " are in the format first author and year (study) \u2013 prediction model method \u2013 unique validation ID."),
                                           tags$li(tags$b("Vertical dashed line"), " represents the pooled standardised estimate for the filtered validations, using the selected pooling method."),
                                           tags$li(tags$b("Horizontal dashed line"), " separates validations that exceed the pooled standardised estimate for the filtered subset."),
                                           tags$li(tags$b("Pooling method"), " can be set to a simple random-effects meta-regression model (default) or a manuscript-aligned meta-regression with moderators (see Instructions)."),
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
                                               "Simple meta-regression (no moderators)" = "simple",
                                               "Manuscript meta-regression" = "manu"
                                           ),
                                           selected = "simple",
                                           inline = FALSE
                                       )
                                   )
                          ),
                          
                          # RIGHT: plot + pooled box (stable stack)
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
        
        # TAB 3: Dataset
        tabPanel("Dataset",
                 div(style = "height: calc(100vh - 140px); overflow: auto;", br(), DTOutput("dataset_table"))
        ),
        
        # TAB 4: Instructions (kept)
        tabPanel("Instructions",
                 div(class = "inst-wrap",
                     
                     div(class = "inst-card",
                         h3("Instructions"),
                         p("Read this page for an overview on how to use this app and what everything means. Read the manuscript (future publication) for further details, including study selection and data extraction."),
                         p(class = "inst-note",
                           "SCOPE-MOVE is an interactive dashboard for exploring validation performance of methods (prediction models) using raw (sample-level) wrist accelerometry data for 24-hour movement behaviour analysis (specified below)."),
                         p(class = "inst-note",
                           "The primary tool is located in ", tags$b("Forest Plot"), ", and allows the user to filter and inspect criterion validation performance of the numerous prediction models using an interactive forest plot and random effects meta-regression analysis."),
                         p(class = "inst-note",
                           "Use the ", tags$b("Dataset"), " to inspect the extracted study metadata.")
                     ),
                     
                     div(class = "inst-card",
                         h4("24-hour movement behaviour domains"),
                         p(class = "inst-note", "Behaviour domains in this app align with the scoping review extraction framework and criterion references."),
                         p(class = "inst-note", "Each classification within the below behaviour domains are referred to as behaviour classes."),
                         tags$dl(class = "inst-kv",
                                 tags$dt("Activity Intensity"),
                                 tags$dd(domain_defs[["Activity Intensity"]]),
                                 tags$dt("Activity Type"),
                                 tags$dd(domain_defs[["Activity Type"]]),
                                 tags$dt("Sleep/Wake"),
                                 tags$dd(domain_defs[["Sleep/Wake"]])
                         )
                     ),
                     
                     div(class = "inst-grid",
                         
                         div(class = "inst-card",
                             h4("How to use the Forest Plot tab"),
                             tags$ol(
                                 tags$li(tags$b("Select a performance metric"), " (F1, sensitivity, specificity). Use F1 for an overall performance assessment and sensitivity or specificity to include more studies and validations."),
                                 tags$li(tags$b("Apply filters"), " for domain, prediction model group, age group, and wearable brand."),
                                 tags$li(tags$b("Interpret the plot"), " where each point is a validation and whiskers show its 95% CI."),
                                 tags$li(tags$b("Row labels"), " are in the format first author and year (study) \u2013 prediction model method \u2013 unique validation ID."),
                                 tags$li(tags$b("Vertical dashed line"), " represents the pooled standardised estimate for the filtered validations, using the selected pooling method."),
                                 tags$li(tags$b("Horizontal dashed line"), " separates validations that exceed the pooled standardised estimate for the filtered subset."),
                                 tags$li(tags$b("Pooling method"), " can be set to a simple random-effects meta-regression model (default) or a manuscript-aligned meta-regression with moderators (see Pooled estimate)."),
                                 tags$li(tags$b("Click a point"), " to open the DOI link.")
                             ),
                             p(class = "inst-note",
                               "Age group and device brand are matched using tokenisation: cells with multiple values are split on ';' and rows are retained if any token matches.")
                         ),
                         
                         div(class = "inst-card",
                             h4("Pooled estimate"),
                             tags$ul(
                                 tags$li("Shown only when the filtered subset has at least 3 validation rows."),
                                 tags$li(tags$b("Simple meta-regression (no moderators):"), 
                                         tags$br(),
                                         tags$span("A conventional random-effects meta-regression (REML) with no moderators, pooled across the selected subset.")
                                         ),
                                 tags$li(tags$b("Manuscript meta-regression:"),
                                         tags$br(),
                                         tags$span("A two-level participant-weighted random-effects meta-regression (REML) with a study- and validation-level random intercept inference used to compute a standardised pooled estimate for the selected subset."),
                                         tags$br(),
                                         tags$span("Meta-regression covariates (moderators) were centred number of behaviour classes and validation folds with missingness indicators, test environment (3 level: free-living, laboratory, hybrid), test protocol (3 level: structured, unstructured, hybrid), and behaviour domain (3 level: activity intensity, activity type, sleep/wake)."),
                                         tags$br(),
                                         tags$span("Any moderator that is constant (or effectively constant due to missingness) is removed from the model formula.")
                                         ),

                                 ),
                         ),
                     ),
                     
                     div(class = "inst-card",
                         h4("How are the plotted estimates defined and what is a metric?"),
                         tags$ul(
                                 tags$li("Each plotted point is the extracted validation performance ", tags$b("metric"), " for that validation. The metrics of interest were F1, sensitivity, and specificity (definitions below)."),
                                 tags$li("The metrics are extracted as the unweighted mean and standard deviation (SD) across behaviour classes, when reported."),
                                 tags$li("When mean and SD are not reported and class-specific performance is reported (e.g. separately for sitting vs walking vs running), metrics are computed via macro-averaging across the classes to obtain an unweighted mean and SD."),
                                 tags$li("When the confusion matrix is provided with validation counts for each behaviour class, metrics are computed and macro-averaged across classes via the 'caret' package in R to obtain an unweighted mean and SD."),
                                 tags$li("Extracted SDs were then converted to sampling variances and divided by cross-validation (CV) folds when SDs reflected CV or by number of behaviour classes when SDs reflected class variation; when SDs were unavailable, a lower-bounded behaviour domain-specific median constant variance was used to stabilise weights."),
                                 tags$li("The constructed variance was then used to build a 95% confidence interval (CI)."),
                                 tags$li("Each point therefore is the reported or computed ", tags$b("metric"), " for a specific validation and the whisker is the computed 95% CI for that specific estimate."),
                                 tags$li("For meta-analysis, metrics are analysed on the logit scale and back-transformed to proportions for display."),
                                 tags$li("Extraction rules, transformations, and any further imputation decisions are documented in the manuscript.")
                             )
                     ),
                     
                     div(class = "inst-card",
                         h4("Key definitions"),
                         div(class = "inst-grid",
                             
                             div(
                                 h5("Performance metrics"),
                                 tags$dl(class = "inst-kv",
                                         tags$dt("F1"),
                                         tags$dd("Harmonic mean of precision (positive predictive value) and sensitivity, averaged across classes. Higher is better."),
                                         tags$dt("Sensitivity"),
                                         tags$dd("True positive rate (recall). Proportion of true positives correctly identified. Higher is better."),
                                         tags$dt("Specificity"),
                                         tags$dd("True negative rate. Proportion of true negatives correctly identified. Higher is better.")
                                 )
                             ),
                             
                             div(
                                 h5("Prediction model groups"),
                                 tags$dl(class = "inst-kv",
                                         tags$dt("Traditional"),
                                         tags$dd("Threshold, heuristic rule-based, and general linear model approaches."),
                                         tags$dt("Non-traditional"),
                                         tags$dd("Classical machine learning, deep learning, probabilistic or temporal models, and ensembles.")
                                 )
                             )
                         )
                     ),
                    
                     div(class = "inst-card",
                         h4("What the hover text fields mean"),
                         p(class = "inst-note", "Hover over any point in the forest plot to see validation metadata. Fields are defined below."),
                         div(class = "inst-grid",
                             
                             div(
                                 h5("Protocol"),
                                 tags$dl(class = "inst-kv",
                                         tags$dt("Structured"),
                                         tags$dd(protocol_defs[["Structured"]]),
                                         tags$dt("Unstructured"),
                                         tags$dd(protocol_defs[["Unstructured"]]),
                                         tags$dt("Hybrid"),
                                         tags$dd(protocol_defs[["Hybrid"]])
                                 )
                             ),
                             
                             div(
                                 h5("Environment"),
                                 tags$dl(class = "inst-kv",
                                         tags$dt("Laboratory"),
                                         tags$dd(environment_defs[["Laboratory"]]),
                                         tags$dt("Free-living"),
                                         tags$dd(environment_defs[["Free-living"]]),
                                         tags$dt("Hybrid"),
                                         tags$dd(environment_defs[["Hybrid"]])
                                 )
                             )
                         ),
                         
                         div(class = "inst-grid",
                             
                             div(
                                 h5("Validation type"),
                                 tags$dl(class = "inst-kv",
                                         tags$dt("Hold-out"),
                                         tags$dd(validation_defs[["Hold-out"]]),
                                         tags$dt("Cross-validation"),
                                         tags$dd(validation_defs[["Cross-validation"]]),
                                         tags$dt("External"),
                                         tags$dd(validation_defs[["External"]]),
                                         tags$dt("Apparent"),
                                         tags$dd(validation_defs[["Apparent"]])
                                 )
                             ),
                             
                             div(
                                 h5("Other fields"),
                                 tags$dl(class = "inst-kv",
                                         tags$dt("Estimate and 95% CI"),
                                         tags$dd("Performance for that validation and its uncertainty interval as extracted from the source paper."),
                                         tags$dt("Age group and device"),
                                         tags$dd("Values are taken from the review extraction sheet. Multi-valued cells are matched via tokenisation (';')."),
                                         tags$dt("N participants"),
                                         tags$dd("Participants contributing to that validation (as extracted)."),
                                         tags$dt("DOI"),
                                         tags$dd("Clicking a point opens the DOI link.")
                                 )
                             )
                         )
                     ),
                     
                     div(class = "inst-card",
                         h4("Interpretation cautions"),
                         tags$ul(
                             tags$li("Pooled estimates summarise the selected subset and may mask heterogeneity across behaviour class definitions, protocols, and populations."),
                             tags$li("Not all studies report all metrics; missingness reflects source reporting, not exclusion by the app."),
                             tags$li("Comparisons across method groups are descriptive within the filtered subset unless you explicitly stratify and interpret within-behaviour domain.")
                         )
                     )
                 )
        )
    )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
    
    output$metric_def <- renderUI({
        txt <- metric_help[[as.character(input$metric)]]
        if (is.null(txt)) return(NULL)
        tags$div(style = "font-size: 12px; color: #555; margin-top: 4px; margin-bottom: 10px;",
                 tags$b("Metric definition: "), txt)
    })
    
    output$method_def <- renderUI({
        sel <- as.character(input$method_group)
        if (!length(sel)) return(NULL)
        blocks <- lapply(sel, \(k) tags$li(tags$b(paste0(k, ": ")), method_help[[k]] %||% ""))
        tags$div(style = "font-size: 12px; color: #555; margin-top: 4px; margin-bottom: 10px;",
                 tags$b("Model group definitions:"), tags$ul(style = "margin-bottom: 0;", blocks))
    })
    
    filtered_data <- reactive({
        df <- df_all %>% filter(metric %in% input$metric, domain %in% input$domain, method_group %in% input$method_group)
        if (input$age_group != "ALL")   df <- df[vapply(df$age_tokens,    \(v) input$age_group   %in% v, logical(1)), ]
        if (input$test_device != "ALL") df <- df[vapply(df$device_tokens, \(v) input$test_device %in% v, logical(1)), ]
        df
    })
    
    plot_h <- reactive({
        df <- filtered_data()                                                           # current filtered rows
        n  <- nrow(df)                                                                  # row count
        row_px   <- 18                                                                  # tune: 16 to 22
        pad_px   <- 140                                                                 # tune: plot padding/legend
        content  <- max(420, n * row_px + pad_px)                                       # content-driven height
        
        win_h <- as.numeric(input$win_h %||% 800)                                       # window height from JS
        avail <- max(420, win_h - 260)                                                  # tune: header+tabs+pooled
        
        max(content, avail)                                                             # fill window when small, grow when large
    })
    
    output$forest_plot_ui <- renderUI({
        plotlyOutput("forest_plot", height = paste0(plot_h(), "px"))
    })

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
        
        p <- ggplotly(make_forest_plot(df, pooled_val), tooltip = "text", source = "forest")
        
        p %>%
            layout(
                hoverlabel = list(bgcolor = "white"),
                margin = list(l = 250, r = 25, t = 10, b = 45),
                yaxis = list(
                    title = list(text = "Model Validations", standoff = 35),
                    automargin = TRUE
                )
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
            # ci_lab <- if (is.finite(pooled["lb"])) sprintf("%.1f%% to %.1f%%", 100*pooled["lb"], 100*pooled["ub"]) else "Not available"
            ci_num <- if (is.finite(pooled["lb"])) sprintf("%.1f to %.1f", 100*pooled["lb"], 100*pooled["ub"]) else "n.a."
            est_ci_lab <- if (ci_num != "n.a.") sprintf("%.1f%% (%.1f to %.1f%%)", 100*pooled["est"], 100*pooled["lb"], 100*pooled["ub"]) else sprintf("%.1f%% (CI n.a.)", 100*pooled["est"])
        } else {
            pooled <- pooled_standardized_minimal(fit_metric_minimal(df))
            method_lab <- "Simple REML (no moderators)"
            mods_lab <- "None"
            clust_lab <- n_stud
            ci_num <- if (is.finite(pooled["lb"])) sprintf("%.1f to %.1f", 100*pooled["lb"], 100*pooled["ub"]) else "n.a."
            est_ci_lab <- if (ci_num != "n.a.") sprintf("%.1f%% (%.1f to %.1f%%)", 100*pooled["est"], 100*pooled["lb"], 100*pooled["ub"]) else sprintf("%.1f%% (CI n.a.)", 100*pooled["est"])
        }
        
        est_lab <- sprintf("%.1f%%", 100*pooled["est"])
        
        div(class = "pooled-card",
            tags$h4(class = "pooled-title", "Pooled estimate"),
            tags$p(class = "pooled-sub", paste0("Dashed line = pooled standardised estimate using ", method_lab, ".")),
            
            div(class = "pooled-grid",
                div(class = "pooled-kv",
                    tags$p(class = "pooled-k", "Estimate (95% CI)"),
                    tags$p(class = "pooled-v", est_ci_lab)
                ),
                div(class = "pooled-kv",
                    tags$p(class = "pooled-k", "Participants (summed)"),
                    tags$p(class = "pooled-v", format(n_part, big.mark = ","))
                ),
                div(class = "pooled-kv",
                    tags$p(class = "pooled-k", "Validation rows, Studies"),
                    tags$p(class = "pooled-v", paste0(n_rows, ", ", clust_lab))
                )
            ),
            
            
            tags$div(class = "pooled-small",
                     tags$div(class = "pooled-pill", paste0("Rows with participants reported: ", n_part_rep, "/", n_rows)),
                     tags$div(class = "pooled-pill", paste0("Metric: ", as.character(input$metric))),
                     tags$div(class = "pooled-pill", paste0("Domains: ", length(input$domain))),
                     tags$div(class = "pooled-pill", paste0("Model groups: ", length(input$method_group)))
            ),
            
            tags$div(class = "pooled-small",
                     tags$b("Moderators: "), mods_lab,
                     tags$br()
            )
        )
    })
    
    observeEvent(plotly::event_data("plotly_click", source = "forest"), {
        ed  <- plotly::event_data("plotly_click", source = "forest")
        url <- df_all$doi_url[match(ed$key, df_all$val_id)]
        if (is.na(url) || !nzchar(url)) return(NULL)
        showModal(modalDialog(
            title = "DOI link, click to open in another tab.",
            tags$a(url, href = url, target = "_blank", rel = "noopener"),
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
    })
    
    output$dataset_table <- renderDT({
        datatable(minimal_df, options = list(pageLength = 20, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE, filter = "top")
    })
}

# ---------------------------
# RUN APP
# ---------------------------
shinyApp(ui = ui, server = server)
