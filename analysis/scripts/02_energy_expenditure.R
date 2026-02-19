# =====================================================================
# Energy expenditure meta-analysis (association models)
# Outcome: MAPE (proportion; lower is better)
# Outputs: pooled table, method contrast table, forest-style plot
# Author: Millen James Theophilus (GitHub: miltheo)
# =====================================================================

rm(list = ls()); gc()

# -----------------------------
# Paths
# -----------------------------
in_dir  <- file.path("analysis", "inputs")
out_dir <- file.path("analysis", "outputs", "energy_expenditure")

# uncomment these 3 lines if you want to run locally
# in_dir  <- file.path("..", "analysis", "inputs") 
# out_dir <- file.path("..", "analysis", "outputs", "energy_expenditure")

csv_path <- file.path(in_dir, "Extraction_Energy Expenditure Models with Validation IDs.csv")
stopifnot(file.exists(csv_path))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)                         # make base output dir
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)    # make tables dir
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)   # make figures dir

# -----------------------------
# Settings (set as per study results, options available for sensitivity analysis)
# -----------------------------
weight_mode <- "fold"                 # equal | fold | fold_or_external
weight_by_participants <- TRUE
use_two_level_random <- TRUE
include_env_prot_domain <- TRUE
standardize_over <- "within_group"    # within_group | overall

forest_res <- 300
forest_width_px <- 4000
forest_row_px <- 18
forest_base_h_px <- 2000

# -----------------------------
# Packages
# -----------------------------
req <- c("readr","dplyr","stringr","tidyr","ggplot2","purrr","ragg","forcats","scales",
         "janitor","metafor","clubSandwich","glue","tibble","tidyselect")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))

# -----------------------------
# Helpers
# -----------------------------
na_tokens <- c("NA","NaN","","n.r.","n.r","n.a.","n.a","n/a","N/A","NR","nr")

classify_method_group <- function(method_chr) {
    s <- stringr::str_to_lower(dplyr::coalesce(method_chr, ""))
    is_linear <- stringr::str_detect(s, "linear|glm|heuristic|threshold|cut.?point|rule")
    dplyr::if_else(is_linear, "traditional", "non-traditional")
}

coalesce_col <- function(df, candidates, default = NA) {
    have <- candidates[candidates %in% names(df)]
    if (length(have)) df[[have[1]]] else default
}

first_author_year <- function(df) {
    if ("study" %in% names(df)) return(df$study)
    fa <- coalesce_col(df, c("first_author","author","firstAuthor"))
    yr <- coalesce_col(df, c("year","pub_year","publication_year"))
    if (!all(is.na(fa)) && !all(is.na(yr))) return(glue::glue("{fa} {yr}"))
    idc <- coalesce_col(df, c("id"))
    if (!all(is.na(idc))) return(glue::glue("Row {idc}"))
    rep(NA_character_, nrow(df))
}

align_beta_to_X <- function(beta, X) {
    b <- as.numeric(beta)
    Xn <- colnames(X)
    if (!is.null(names(beta)) && length(names(beta)) == length(b) && all(names(beta) %in% Xn)) {
        b2 <- rep(0, length(Xn)); names(b2) <- Xn
        b2[names(beta)] <- b
        return(as.numeric(b2))
    }
    if (length(b) == ncol(X)) return(b)
    stop("Could not align beta to X.")
}

wcolsum <- function(X, w) as.numeric(crossprod(as.numeric(w), X))

# -----------------------------
# Load and clean
# -----------------------------
dat0 <- readr::read_csv(csv_path, show_col_types = FALSE, na = na_tokens) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(id))

if (!"method" %in% names(dat0)) {
    if ("model_type" %in% names(dat0)) dat0 <- dplyr::rename(dat0, method = model_type) else dat0$method <- NA_character_
}

if (!"study" %in% names(dat0)) stop("Column 'study' is required (FirstAuthor Year).")

mape_mean_col <- dplyr::case_when(
    "mape_mean" %in% names(dat0) ~ "mape_mean",
    "mape" %in% names(dat0) ~ "mape",
    TRUE ~ NA_character_
)
if (is.na(mape_mean_col)) stop("Could not find MAPE mean column.")

mape_sd_col <- if ("mape_sd" %in% names(dat0)) "mape_sd" else NA_character_

is_pct_like <- function(x) is.finite(x) & x > 1.5
m_raw <- suppressWarnings(as.numeric(dat0[[mape_mean_col]]))
m_prop <- ifelse(is_pct_like(m_raw), m_raw / 100, m_raw)
dat0[[mape_mean_col]] <- m_prop

if (!is.na(mape_sd_col)) {
    sd_raw <- suppressWarnings(as.numeric(dat0[[mape_sd_col]]))
    dat0[[mape_sd_col]] <- ifelse(is_pct_like(sd_raw), sd_raw / 100, sd_raw)
}

dat0 <- dat0 |>
    mutate(
        study_id = factor(study),
        row_id = dplyr::row_number(),
        val_id = dplyr::coalesce(as.character(coalesce_col(dat0, c("val_id","validation_id"), NA)), as.character(row_id)),
        k_folds = suppressWarnings(as.numeric(k_folds)),
        n_participants = suppressWarnings(as.numeric(n_participants)),
        sd_origin = stringr::str_to_lower(dplyr::coalesce(sd_origin, NA_character_)),
        test_environment = factor(dplyr::coalesce(test_environment, NA_character_)),
        test_protocol = factor(dplyr::coalesce(test_protocol, NA_character_)),
        method_group = classify_method_group(method)
    )

d_ee <- dat0 |>
    transmute(
        val_id, id, study, study_id, row_id, method, method_group,
        sd_origin, k_folds, n_participants, test_environment, test_protocol,
        metric = "MAPE",
        mean = suppressWarnings(as.numeric(.data[[mape_mean_col]])),
        sd = if (!is.na(mape_sd_col)) suppressWarnings(as.numeric(.data[[mape_sd_col]])) else NA_real_
    ) |>
    filter(is.finite(mean)) |>
    mutate(method_group = factor(method_group, levels = c("non-traditional","traditional")))

stopifnot(nrow(d_ee) > 0)

# -----------------------------
# Variances and moderators
# -----------------------------
d_ee <- d_ee |>
    mutate(
        vi_fold = dplyr::case_when(
            sd_origin == "cv_folds" & is.finite(sd) & is.finite(k_folds) & k_folds > 0 ~ (sd^2) / k_folds,
            TRUE ~ NA_real_
        ),
        vi_ext = dplyr::case_when(
            sd_origin == "external_test" & is.finite(sd) ~ (sd^2),
            TRUE ~ NA_real_
        )
    )

vi_raw <- switch(
    weight_mode,
    equal = rep(NA_real_, nrow(d_ee)),
    fold = d_ee$vi_fold,
    fold_or_external = dplyr::coalesce(d_ee$vi_fold, d_ee$vi_ext),
    stop("weight_mode must be one of: equal, fold, fold_or_external")
)

const_vi <- if (any(is.finite(vi_raw))) stats::median(vi_raw, na.rm = TRUE) else 0.0025
const_vi <- max(const_vi, 1e-5)

d_ee <- d_ee |>
    mutate(
        vi_use = ifelse(is.finite(vi_raw), vi_raw, const_vi),
        vi_use = pmax(vi_use, 1e-6),
        vi_source = dplyr::case_when(
            is.finite(vi_fold) ~ "cv_folds",
            weight_mode == "fold_or_external" & is.finite(vi_ext) ~ "external",
            TRUE ~ "constant"
        ),
        miss_k_folds = as.integer(!is.finite(k_folds))
    )

mean_k <- mean(d_ee$k_folds, na.rm = TRUE)
d_ee <- d_ee |>
    mutate(
        k_folds_imp = dplyr::coalesce(k_folds, mean_k),
        c_k_folds = k_folds_imp - mean_k
    )

if (include_env_prot_domain) d_ee <- d_ee |> mutate(env_re = factor(test_environment), prot_re = factor(test_protocol))

# -----------------------------
# Pooled model
# -----------------------------
fit_metric <- function(dfm) {
    if (nrow(dfm) < 3) return(NULL)
    base_mods <- c("c_k_folds","miss_k_folds")
    add_mods <- if (include_env_prot_domain) c("env_re","prot_re") else character(0)
    X <- dplyr::select(dfm, tidyselect::any_of(c(base_mods, add_mods)))
    nonconst <- vapply(X, function(v) length(unique(v[is.finite(v)])) > 1, logical(1))
    kept_mods <- colnames(X)[nonconst]
    fml <- if (length(kept_mods)) as.formula(paste("~", paste(kept_mods, collapse = " + "))) else ~ 1
    rand <- if (use_two_level_random) list(~1|study_id, ~1|row_id) else list(~1|study_id)
    m <- try(metafor::rma.mv(yi = mean, V = vi_use, mods = fml, random = rand,
                             data = dfm, method = "REML",
                             control = list(optimizer = "optim")), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    n_clusters <- dplyr::n_distinct(dfm$study_id)
    vc_type <- if (n_clusters >= 6) "CR2" else "CR1S"
    list(model = m, kept_mods = kept_mods, vcov_type = vc_type)
}

pooled_standardized <- function(fit, dfm) {
    X <- as.matrix(fit$model$X)
    b <- align_beta_to_X(fit$model$beta, X)
    eta <- as.numeric(X %*% b)
    
    w <- if (weight_by_participants && "n_participants" %in% names(dfm)) dfm$n_participants else rep(1, nrow(dfm))
    w <- as.numeric(w); w[!is.finite(w)] <- 0
    if (sum(w) == 0) w <- rep(1, length(w))
    w <- w / sum(w)
    
    est <- sum(w * eta, na.rm = TRUE)
    
    V_CR <- try(clubSandwich::vcovCR(fit$model, type = fit$vcov_type, cluster = dfm$study_id), silent = TRUE)
    if (inherits(V_CR, "try-error") || any(!is.finite(V_CR))) return(c(est = est, lb = NA_real_, ub = NA_real_))
    
    J <- wcolsum(X, w)
    se <- sqrt(drop(J %*% V_CR %*% J))
    c(est = est, lb = est - 1.96 * se, ub = est + 1.96 * se)
}

fit_all <- fit_metric(d_ee)
pooled <- if (is.null(fit_all)) c(est = NA_real_, lb = NA_real_, ub = NA_real_) else pooled_standardized(fit_all, d_ee)

out_tbl <- tibble::tibble(metric = "MAPE",
                          pooled_std_est = pooled["est"],
                          pooled_std_lb  = pooled["lb"],
                          pooled_std_ub  = pooled["ub"])
readr::write_csv(out_tbl |> mutate(across(starts_with("pooled_"), ~ round(100 * as.numeric(.x), 2))),
                 file.path(out_dir, "tables", "ee_meta_results_all.csv"))

# -----------------------------
# Method contrast (joint model, standardized only)
# -----------------------------
fit_joint <- function(dfm) {
    if (nrow(dfm) < 3 || dplyr::n_distinct(dfm$method_group[!is.na(dfm$method_group)]) < 2) return(NULL)
    mods <- c("c_k_folds","miss_k_folds")
    if (include_env_prot_domain) mods <- c(mods, "env_re","prot_re")
    fml <- as.formula(paste("~", paste(c(mods, "method_group"), collapse = " + ")))
    rand <- if (use_two_level_random) list(~1|study_id, ~1|row_id) else list(~1|study_id)
    m <- try(metafor::rma.mv(yi = mean, V = vi_use, mods = fml, random = rand,
                             data = dfm, method = "REML",
                             control = list(optimizer = "optim")), silent = TRUE)
    if (inherits(m, "try-error")) NULL else m
}

locate_method_cols <- function(X_names) {
    main <- which(stringr::str_detect(X_names, "^method_group") & !stringr::str_detect(X_names, ":"))
    inter <- which(stringr::str_detect(X_names, "method_group") & stringr::str_detect(X_names, ":"))
    list(main = main, inter = inter)
}

X_with_method <- function(X, method_dummy) {
    X_new <- X
    mc <- locate_method_cols(colnames(X))
    if (length(mc$main) == 1L) X_new[, mc$main] <- method_dummy
    if (length(mc$inter)) {
        for (j in mc$inter) {
            term <- colnames(X)[j]
            parts <- strsplit(term, ":", fixed = TRUE)[[1]]
            other <- parts[!stringr::str_detect(parts, "method_group")][1]
            if (!is.na(other) && other %in% colnames(X)) X_new[, j] <- method_dummy * X[, match(other, colnames(X))]
            else X_new[, j] <- if (method_dummy == 1) X[, j] else 0
        }
    }
    X_new
}

standardized_for_method <- function(fit, dfm, method_level = c("non-traditional","traditional")) {
    method_level <- match.arg(method_level)
    X <- as.matrix(fit$X)
    b <- align_beta_to_X(fit$beta, X)
    
    md <- if (method_level == "traditional") 1 else 0
    Xn <- X_with_method(X, md)
    eta <- as.numeric(Xn %*% b)
    
    w_raw <- if (weight_by_participants && "n_participants" %in% names(dfm)) dfm$n_participants else rep(1, nrow(dfm))
    w_raw <- as.numeric(w_raw); w_raw[!is.finite(w_raw)] <- 0
    
    if (standardize_over == "within_group") {
        mask <- dfm$method_group == method_level
        w <- ifelse(mask, w_raw, 0)
    } else w <- w_raw
    
    if (sum(w, na.rm = TRUE) == 0) w <- rep(1, length(w))
    w <- w / sum(w, na.rm = TRUE)
    
    est <- sum(w * eta, na.rm = TRUE)
    
    Vcr <- clubSandwich::vcovCR(fit, type = "CR2", cluster = dfm$study_id)
    J <- wcolsum(Xn, w)
    se <- sqrt(drop(J %*% Vcr %*% J))
    
    c(est = est, lb = est - 1.96 * se, ub = est + 1.96 * se, se = se)
}

contrast_methods <- function(fit, dfm) {
    a <- standardized_for_method(fit, dfm, "non-traditional")
    b <- standardized_for_method(fit, dfm, "traditional")
    
    X <- as.matrix(fit$X)
    Vcr <- clubSandwich::vcovCR(fit, type = "CR2", cluster = dfm$study_id)
    
    grad_std <- function(md, method_level) {
        Xn <- X_with_method(X, md)
        w_raw <- if (weight_by_participants && "n_participants" %in% names(dfm)) dfm$n_participants else rep(1, nrow(dfm))
        w_raw <- as.numeric(w_raw); w_raw[!is.finite(w_raw)] <- 0
        if (standardize_over == "within_group") {
            mask <- dfm$method_group == method_level
            w <- ifelse(mask, w_raw, 0)
        } else w <- w_raw
        if (sum(w, na.rm = TRUE) == 0) w <- rep(1, length(w))
        w <- w / sum(w, na.rm = TRUE)
        wcolsum(Xn, w)
    }
    
    J_o <- grad_std(0, "non-traditional")
    J_l <- grad_std(1, "traditional")
    
    diff <- as.numeric(a["est"] - b["est"])
    se   <- sqrt(max(drop((J_o - J_l) %*% Vcr %*% (J_o - J_l)), 0))
    p    <- 2 * pnorm(-abs(diff / se))
    
    tibble::tibble(
        other_std = a["est"], other_std_lb = a["lb"], other_std_ub = a["ub"],
        linear_std = b["est"], linear_std_lb = b["lb"], linear_std_ub = b["ub"],
        diff_std = diff, diff_std_lb = diff - 1.96 * se, diff_std_ub = diff + 1.96 * se,
        p_std = p
    )
}

df_cmp <- d_ee |> mutate(method_group = factor(method_group, levels = c("non-traditional","traditional")))
fit_cmp <- fit_joint(df_cmp)
if (!is.null(fit_cmp)) {
    cmp <- contrast_methods(fit_cmp, df_cmp)
    cmp_out <- cmp |>
        mutate(across(where(is.numeric) & !matches("^p_"), ~ round(100 * as.numeric(.x), 2)),
               across(matches("^p_"), ~ round(as.numeric(.x), 4)))
    readr::write_csv(cmp_out, file.path(out_dir, "tables", "ee_method_contrast_results_overall.csv"))
}

# -----------------------------
# Forest plot
# -----------------------------
build_plot_df <- function(d) {
    d |>
        mutate(
            se_use = sqrt(vi_use),
            est = mean,
            ci_l = pmax(est - 1.96 * se_use, 0),
            ci_u = est + 1.96 * se_use,
            auth_year = first_author_year(dplyr::cur_data_all()),
            method_lab = ifelse(is.na(method) | method == "", "Method n.r.", method),
            y_label = glue::glue("{auth_year} - {method_lab} - {val_id}")
        ) |>
        arrange(est) |>
        mutate(y_label = forcats::fct_rev(forcats::fct_inorder(y_label)))
}

dfp <- build_plot_df(d_ee)

p <- ggplot(dfp, aes(x = est, y = y_label)) +
    geom_vline(xintercept = as.numeric(pooled["est"]), linetype = 2, linewidth = 0.5, alpha = 0.85, na.rm = TRUE) +
    geom_pointrange(aes(xmin = ci_l, xmax = ci_u, shape = vi_source),
                    size = 0.3, linewidth = 0.3, alpha = 0.95) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_shape_manual(name = "Variance source",
                       values = c(cv_folds = 16, external = 15, constant = 1),
                       labels = c(cv_folds = "From CV folds", external = "External test SD", constant = "Constant")) +
    labs(title = "Forest-style plot: MAPE (Energy expenditure)",
         subtitle = paste0("Pooled standardised estimate [95% CI] = ",
                           scales::percent(pooled["est"], accuracy = 0.1), " [",
                           scales::percent(pooled["lb"], accuracy = 0.1), ", ",
                           scales::percent(pooled["ub"], accuracy = 0.1), "]"),
         x = "MAPE (proportion)", y = "Validation rows") +
    theme_minimal(base_size = 18, base_family = "Arial") +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.margin = margin(5, 30, 5, 5))

fpath <- file.path(out_dir, "figures", "forest_MAPE_all.png")
ragg::agg_png(fpath, width = forest_width_px, height = forest_base_h_px + forest_row_px * nrow(dfp), res = forest_res)
print(p); dev.off()

cat("Done. Outputs saved to: ", normalizePath(out_dir), "\n")
