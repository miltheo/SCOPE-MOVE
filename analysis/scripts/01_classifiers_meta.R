# =====================================================================
# Classifiers meta-analysis (F1, Sensitivity, Specificity)
# Outputs: pooled tables, method contrasts, forest-style plots, descriptives
# Author: Millen James Theophilus (GitHub: miltheo)
# =====================================================================

rm(list = ls()); gc()

# -----------------------------
# Paths
# -----------------------------

in_dir  <- file.path("analysis", "inputs")
out_dir <- file.path("analysis", "outputs", "classifiers")

# uncomment these 3 lines if you want to run locally
# in_dir  <- file.path("..", "analysis", "inputs") 
# out_dir <- file.path("..", "analysis", "outputs", "classifiers")

csv_path <- file.path(in_dir, "Extraction_Models_Master Sheet with Validation IDs.csv")
stopifnot(file.exists(csv_path))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Settings (set as per study results, options available for sensitivity analysis)
# -----------------------------
weight_mode <- "class"                # equal | fold | class | fold_or_external (class set for classifier results)
treat_external_as_fold <- FALSE
weight_by_participants <- TRUE
use_two_level_random <- TRUE
include_env_prot_domain <- TRUE
run_method_subgroups <- TRUE
run_domain_pooled <- TRUE
run_domain_method_contrasts <- TRUE
standardize_over <- "within_group"    # within_group | overall
eps_prob <- 1e-3

# Forest plot settings
forest_res <- 300
forest_width_px <- 2500
forest_row_px <- 45
forest_base_h_px <- 550

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

logit_safe <- function(p, eps = 1e-6) qlogis(pmax(pmin(p, 1 - eps), eps))
inv_logit  <- function(x) plogis(x)
clamp_prob <- function(p, eps = eps_prob) pmin(pmax(p, eps), 1 - eps)

var_logit_from_sd <- function(p, sd, k = 1) {
    p <- pmax(pmin(p, 1 - 1e-6), 1e-6)
    (sd^2 / p^2 / (1 - p)^2) / pmax(k, 1)
}

classify_method_group <- function(method_chr) {
    m <- stringr::str_to_lower(dplyr::coalesce(method_chr, ""))
    is_thresh <- stringr::str_detect(m, "threshold|linear|glm|cut.?point|rule|heuristic|if[- ]?else")
    dplyr::if_else(is_thresh, "traditional", "non-traditional")
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

# -----------------------------
# Forest style (match manuscript)
# -----------------------------
plt_style <- list(
    res    = forest_res,
    width  = forest_width_px,
    row_px = forest_row_px,
    base_h = forest_base_h_px,
    alpha  = 0.95,
    point_size = 0.35,
    line_width  = 0.35,
    x_label_accuracy = 1,
    x_limits = c(0, 1),
    shape_vals = c(cv_folds = 16, classes = 17, external = 15, constant = 1),
    shape_labs = c(cv_folds = "From CV folds",
                   classes  = "From classes",
                   external = "External test SD",
                   constant = "Constant"),
    okabe_ito = c(
        "#CC79A7", "#E69F00", "#56B4E9", "#009E73",
        "#F0E442", "#0072B2", "#D55E00", "#000000"
    ),
    domain_levels = c("Sleep","Activity type","Activity intensity","Energy expenditure")
)

make_domain_palette <- function(levels, style = plt_style) {
    lev <- style$domain_levels[style$domain_levels %in% levels]
    if (!length(lev)) lev <- levels
    pal <- style$okabe_ito[seq_along(lev)]
    stats::setNames(pal, lev)
}

# -----------------------------
# Load and clean
# -----------------------------
dat0 <- readr::read_csv(csv_path, show_col_types = FALSE, na = na_tokens) |>
    janitor::clean_names()

if (!"method" %in% names(dat0)) {
    if ("model_type" %in% names(dat0)) dat0 <- dplyr::rename(dat0, method = model_type) else dat0$method <- NA_character_
}

domain_col <- dplyr::case_when(
    "movement_behaviour" %in% names(dat0) ~ "movement_behaviour",
    "behaviour" %in% names(dat0) ~ "behaviour",
    "outcome" %in% names(dat0) ~ "outcome",
    TRUE ~ NA_character_
)
dat0$domain <- if (!is.na(domain_col)) factor(dat0[[domain_col]]) else factor("Unknown")

if (!"study" %in% names(dat0)) stop("Column 'study' is required (FirstAuthor Year).")

dat0 <- dat0 |>
    mutate(
        study_id = factor(study),
        val_id = dplyr::coalesce(as.character(coalesce_col(dat0, c("val_id","validation_id"), NA)), as.character(dplyr::row_number())),
        n_classes = suppressWarnings(as.numeric(n_classes)),
        k_folds = suppressWarnings(as.numeric(k_folds)),
        n_participants = suppressWarnings(as.numeric(n_participants)),
        sd_origin = stringr::str_to_lower(dplyr::coalesce(sd_origin, NA_character_)),
        metric_origin = stringr::str_to_lower(dplyr::coalesce(metric_origin, NA_character_)),
        test_environment = factor(dplyr::coalesce(test_environment, NA_character_)),
        test_protocol = factor(dplyr::coalesce(test_protocol, NA_character_))
    )

pct_cols <- c("f1_mean","f1_sd","sens_mean","sens_sd","spec_mean","spec_sd")
present <- intersect(pct_cols, names(dat0))
if (length(present)) {
    need_scale <- any(dplyr::coalesce(dat0$f1_mean,0) > 1, na.rm = TRUE) |
        any(dplyr::coalesce(dat0$sens_mean,0) > 1, na.rm = TRUE) |
        any(dplyr::coalesce(dat0$spec_mean,0) > 1, na.rm = TRUE)
    if (need_scale) dat0 <- dat0 |> mutate(across(all_of(present), ~ .x / 100))
}

dat0 <- dat0 |>
    mutate(
        f1_sd   = ifelse(sd_origin == "classes" & is.finite(f1_sd)   & f1_sd   == 0, NA_real_, f1_sd),
        sens_sd = ifelse(sd_origin == "classes" & is.finite(sens_sd) & sens_sd == 0, NA_real_, sens_sd),
        spec_sd = ifelse(sd_origin == "classes" & is.finite(spec_sd) & spec_sd == 0, NA_real_, spec_sd)
    )

# -----------------------------
# Long metric table
# -----------------------------
build_metric_df <- function(mean_col, sd_col, label) {
    if (!all(c(mean_col, sd_col) %in% names(dat0))) return(NULL)
    dat0 |>
        transmute(
            id, study, study_id, val_id, method, domain, sd_origin, metric_origin,
            n_classes, k_folds, n_participants, test_environment, test_protocol,
            metric = label,
            mean = suppressWarnings(as.numeric(.data[[mean_col]])),
            sd   = suppressWarnings(as.numeric(.data[[sd_col]]))
        ) |>
        filter(is.finite(mean))
}

d_all <- dplyr::bind_rows(
    build_metric_df("f1_mean","f1_sd","F1"),
    build_metric_df("sens_mean","sens_sd","Sensitivity"),
    build_metric_df("spec_mean","spec_sd","Specificity")
)
stopifnot(nrow(d_all) > 0)

# -----------------------------
# Variances and analysis columns
# -----------------------------
d_all <- d_all |>
    mutate(
        yi = logit_safe(mean),
        vi_fold = dplyr::case_when(
            sd_origin == "cv_folds" & is.finite(sd) & is.finite(k_folds) & k_folds > 0 ~ var_logit_from_sd(mean, sd, k = k_folds),
            TRUE ~ NA_real_
        ),
        vi_class = dplyr::case_when(
            sd_origin == "classes" & is.finite(sd) & is.finite(n_classes) & n_classes > 0 ~ var_logit_from_sd(mean, sd, k = n_classes),
            TRUE ~ NA_real_
        ),
        vi_ext = dplyr::case_when(
            sd_origin == "external_test" & is.finite(sd) ~ var_logit_from_sd(mean, sd, k = 1),
            TRUE ~ NA_real_
        )
    )

vi_raw <- switch(
    weight_mode,
    equal = rep(NA_real_, nrow(d_all)),
    fold = d_all$vi_fold,
    class = if (isTRUE(treat_external_as_fold)) dplyr::coalesce(d_all$vi_fold, d_all$vi_ext, d_all$vi_class) else dplyr::coalesce(d_all$vi_fold, d_all$vi_class),
    fold_or_external = dplyr::coalesce(d_all$vi_fold, d_all$vi_ext),
    stop("weight_mode must be one of: equal, fold, class, fold_or_external")
)

const_vi <- if (any(is.finite(vi_raw))) stats::median(vi_raw, na.rm = TRUE) else 0.25
const_vi <- max(const_vi, 0.05)

d_all <- d_all |>
    mutate(
        vi_use = ifelse(is.finite(vi_raw), vi_raw, const_vi),
        vi_use = pmax(vi_use, 1e-6),
        vi_source = dplyr::case_when(
            is.finite(vi_fold) ~ "cv_folds",
            isTRUE(treat_external_as_fold) & is.finite(vi_ext) & weight_mode %in% c("class","fold_or_external") ~ "external",
            weight_mode == "class" & is.finite(vi_class) ~ "classes",
            weight_mode == "fold_or_external" & is.finite(vi_ext) ~ "external",
            TRUE ~ "constant"
        ),
        method_group = classify_method_group(method),
        miss_n_classes = as.integer(!is.finite(n_classes)),
        miss_k_folds   = as.integer(!is.finite(k_folds))
    )

mean_n_classes <- mean(d_all$n_classes, na.rm = TRUE)
mean_k_folds   <- mean(d_all$k_folds,   na.rm = TRUE)

d_all <- d_all |>
    mutate(
        n_classes_imp = dplyr::coalesce(n_classes, mean_n_classes),
        k_folds_imp   = dplyr::coalesce(k_folds,   mean_k_folds),
        c_n_classes   = n_classes_imp - mean_n_classes,
        c_k_folds     = k_folds_imp   - mean_k_folds
    )

if (include_env_prot_domain) {
    d_all <- d_all |>
        mutate(
            env_re  = relevel(factor(test_environment), ref = levels(test_environment)[1]),
            prot_re = relevel(factor(test_protocol),    ref = levels(test_protocol)[1]),
            dom_re  = relevel(factor(domain),           ref = levels(domain)[1])
        )
}

# -----------------------------
# Model fitting and pooled prediction
# -----------------------------
fit_metric <- function(dfm) {
    if (nrow(dfm) < 3) return(NULL)
    base_mods <- c("c_n_classes","c_k_folds","miss_n_classes","miss_k_folds")
    add_mods  <- if (include_env_prot_domain) c("env_re","prot_re","dom_re") else character(0)
    X <- dplyr::select(dfm, tidyselect::any_of(c(base_mods, add_mods)))
    nonconst <- vapply(X, function(v) length(unique(v[is.finite(v)])) > 1, logical(1))
    kept_mods <- colnames(X)[nonconst]
    fml <- if (length(kept_mods)) as.formula(paste("~", paste(kept_mods, collapse = " + "))) else ~ 1
    rand <- if (use_two_level_random) list(~1|study_id, ~1|val_id) else list(~1|study_id)
    m <- try(metafor::rma.mv(yi = yi, V = vi_use, mods = fml, random = rand,
                             data = dfm, method = "REML",
                             control = list(optimizer = "optim")), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    n_clusters <- dplyr::n_distinct(dfm$study_id)
    vc_type <- if (n_clusters >= 6) "CR2" else "CR1S"
    list(model = m, kept_mods = kept_mods, vcov_type = vc_type, clusters = n_clusters)
}

pooled_standardized <- function(fit, dfm, weight_by_n = FALSE, n_col = "n_participants") {
    X <- as.matrix(fit$model$X)
    beta <- fit$model$beta
    if (!is.null(colnames(X)) && !is.null(names(beta))) X <- X[, names(beta), drop = FALSE]
    eta <- as.numeric(X %*% as.numeric(beta))
    p_i <- clamp_prob(plogis(eta), eps = eps_prob)
    if (weight_by_n && n_col %in% names(dfm)) {
        w <- dfm[[n_col]]; if (!any(is.finite(w))) w <- rep(1, nrow(dfm))
    } else w <- rep(1, nrow(dfm))
    w <- w / sum(w, na.rm = TRUE)
    est <- sum(w * p_i, na.rm = TRUE)
    
    V_CR <- try(clubSandwich::vcovCR(fit$model, type = fit$vcov_type, cluster = dfm$study_id), silent = TRUE)
    if (inherits(V_CR, "try-error") || any(!is.finite(V_CR))) return(c(est = est, lb = NA_real_, ub = NA_real_))
    
    J <- colSums((w * p_i * (1 - p_i)) * X, na.rm = TRUE)
    se <- sqrt(drop(J %*% V_CR %*% J))
    if (!is.finite(se) || se < 1e-8) return(c(est = est, lb = NA_real_, ub = NA_real_))
    
    lb <- max(est - 1.96 * se, 0)
    ub <- min(est + 1.96 * se, 1)
    c(est = est, lb = lb, ub = ub)
}

run_meta <- function(d, subgroup = NULL) {
    if (!is.null(subgroup)) d <- d |> dplyr::filter(method_group == subgroup)
    if (!nrow(d)) return(NULL)
    d |>
        dplyr::group_by(metric) |>
        dplyr::group_map(~{
            fit <- fit_metric(.x)
            if (is.null(fit)) {
                return(tibble::tibble(metric = .y$metric, k_rows = nrow(.x), clusters = NA_integer_,
                                      vcov_type = NA_character_, kept_mods = NA_character_,
                                      pooled_std_est = NA_real_, pooled_std_lb = NA_real_, pooled_std_ub = NA_real_))
            }
            std <- pooled_standardized(fit, .x, weight_by_n = weight_by_participants)
            tibble::tibble(metric = .y$metric,
                           k_rows = nrow(.x),
                           clusters = fit$clusters,
                           vcov_type = fit$vcov_type,
                           kept_mods = if (length(fit$kept_mods)) paste(fit$kept_mods, collapse = ", ") else "(intercept only)",
                           pooled_std_est = as.numeric(std["est"]),
                           pooled_std_lb  = as.numeric(std["lb"]),
                           pooled_std_ub  = as.numeric(std["ub"]))
        }) |>
        dplyr::bind_rows()
}

round_tbl <- function(x) x |> dplyr::mutate(across(dplyr::matches("pooled_"), ~ round(as.numeric(.x), 4)))

# -----------------------------
# Pooled results
# -----------------------------
res_all <- run_meta(d_all)
res_thr <- if (run_method_subgroups) run_meta(d_all, "traditional") else NULL
res_oth <- if (run_method_subgroups) run_meta(d_all, "non-traditional") else NULL

readr::write_csv(round_tbl(res_all), file.path(out_dir, "tables", "meta_results_all.csv"))
if (!is.null(res_thr)) readr::write_csv(round_tbl(res_thr), file.path(out_dir, "tables", "meta_results_traditional.csv"))
if (!is.null(res_oth)) readr::write_csv(round_tbl(res_oth), file.path(out_dir, "tables", "meta_results_nontraditional.csv"))

# -----------------------------
# Domain pooled
# -----------------------------
pooled_within_domain <- function(dom, met) {
    dfm <- d_all |> dplyr::filter(metric == met, as.character(domain) == as.character(dom))
    if (nrow(dfm) < 3) return(NULL)
    fit <- fit_metric(dfm); if (is.null(fit)) return(NULL)
    std <- pooled_standardized(fit, dfm, weight_by_n = weight_by_participants)
    tibble::tibble(
        domain = as.character(dom),
        metric = met,
        k_rows = nrow(dfm),
        clusters = dplyr::n_distinct(dfm$study_id),
        pooled_std_est = as.numeric(std["est"]),
        pooled_std_lb  = as.numeric(std["lb"]),
        pooled_std_ub  = as.numeric(std["ub"]),
        kept_mods = if (length(fit$kept_mods)) paste(fit$kept_mods, collapse = ", ") else "(intercept only)"
    )
}

res_dom <- NULL
if (isTRUE(run_domain_pooled)) {
    doms <- sort(unique(as.character(d_all$domain)))
    mets <- c("F1","Sensitivity","Specificity")
    res_dom <- purrr::map_dfr(doms, \(d) purrr::map_dfr(mets, \(m) pooled_within_domain(d, m)))
    if (nrow(res_dom)) readr::write_csv(round_tbl(res_dom), file.path(out_dir, "tables", "pooled_within_domain.csv"))
}

# -----------------------------
# Method contrasts (joint model, standardized only)
# -----------------------------
.coef_names <- function(fit) {
    nb <- names(fit$beta)
    if (!is.null(nb) && length(nb) == length(fit$beta)) return(nb)
    cb <- colnames(fit$X)
    if (!is.null(cb) && length(cb) == length(fit$beta)) return(cb)
    stop("Cannot determine coefficient names.")
}

.qform <- function(g, V) {
    if (!is.null(colnames(V))) {
        ok <- intersect(names(g), colnames(V))
        g2 <- rep(0, ncol(V)); names(g2) <- colnames(V)
        g2[ok] <- g[ok]
        as.numeric(drop(t(g2) %*% V %*% g2))
    } else {
        as.numeric(drop(t(g) %*% V %*% g))
    }
}

locate_method_cols <- function(X_names, method_tag = "method_group") {
    main  <- which(stringr::str_detect(X_names, paste0("^", method_tag)) & !stringr::str_detect(X_names, ":"))
    inter <- which(stringr::str_detect(X_names, method_tag) & stringr::str_detect(X_names, ":"))
    list(main = main, inter = inter)
}

X_with_method <- function(X, method_dummy) {
    X_new <- X
    mc <- locate_method_cols(colnames(X), "method_group")
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

fit_joint_metric <- function(df_metric, include_domain = TRUE) {
    if (nrow(df_metric) < 3) return(NULL)
    base_mods <- c("c_n_classes","c_k_folds","miss_n_classes","miss_k_folds")
    fac_mods <- character(0)
    if (include_env_prot_domain) {
        fac_mods <- c("env_re","prot_re")
        if (isTRUE(include_domain)) fac_mods <- c(fac_mods, "dom_re")
    }
    fml <- as.formula(paste("~", paste(c(base_mods, fac_mods, "method_group"), collapse = " + ")))
    rand <- if (use_two_level_random) list(~1|study_id, ~1|val_id) else list(~1|study_id)
    m <- try(metafor::rma.mv(yi = yi, V = vi_use, mods = fml, random = rand,
                             data = df_metric, method = "REML",
                             control = list(optimizer = "optim")), silent = TRUE)
    if (inherits(m, "try-error")) return(NULL)
    m
}

standardized_for_method <- function(fit, df_metric, method_level = c("non-traditional","traditional")) {
    method_level <- match.arg(method_level)
    X <- as.matrix(fit$X)
    bn <- .coef_names(fit)
    if (!is.null(colnames(X))) X <- X[, bn, drop = FALSE]
    beta <- as.numeric(fit$beta)
    
    md <- if (method_level == "traditional") 1 else 0
    X_new <- X_with_method(X, md)
    eta <- as.numeric(X_new %*% beta)
    p_i <- clamp_prob(plogis(eta), eps = eps_prob)
    
    w_raw <- if (weight_by_participants && "n_participants" %in% names(df_metric)) df_metric$n_participants else rep(1, nrow(df_metric))
    w_raw[!is.finite(w_raw)] <- 0
    
    if (standardize_over == "within_group") {
        mask <- df_metric$method_group == method_level
        w <- ifelse(mask, w_raw, 0)
    } else w <- w_raw
    
    if (sum(w, na.rm = TRUE) == 0) w <- rep(1, length(w))
    w <- w / sum(w, na.rm = TRUE)
    
    est <- sum(w * p_i, na.rm = TRUE)
    
    Vcr <- clubSandwich::vcovCR(fit, type = "CR2", cluster = df_metric$study_id)
    J <- colSums((w * p_i * (1 - p_i)) * X_new, na.rm = TRUE); names(J) <- bn
    se <- sqrt(max(.qform(J, Vcr), 0))
    if (!is.finite(se) || se < 1e-8) return(c(est = est, lb = NA_real_, ub = NA_real_))
    
    c(est = est, lb = max(est - 1.96 * se, 0), ub = min(est + 1.96 * se, 1))
}

contrast_methods <- function(fit, df_metric) {
    a <- standardized_for_method(fit, df_metric, "non-traditional")
    b <- standardized_for_method(fit, df_metric, "traditional")
    
    X <- as.matrix(fit$X)
    bn <- .coef_names(fit)
    if (!is.null(colnames(X))) X <- X[, bn, drop = FALSE]
    Vcr <- clubSandwich::vcovCR(fit, type = "CR2", cluster = df_metric$study_id)
    
    grad_std <- function(md, method_level) {
        Xn <- X_with_method(X, md)
        eta <- as.numeric(Xn %*% as.numeric(fit$beta))
        p_i <- clamp_prob(plogis(eta), eps = eps_prob)
        
        w_raw <- if (weight_by_participants && "n_participants" %in% names(df_metric)) df_metric$n_participants else rep(1, nrow(df_metric))
        w_raw[!is.finite(w_raw)] <- 0
        
        if (standardize_over == "within_group") {
            mask <- df_metric$method_group == method_level
            w <- ifelse(mask, w_raw, 0)
        } else w <- w_raw
        
        if (sum(w, na.rm = TRUE) == 0) w <- rep(1, length(w))
        w <- w / sum(w, na.rm = TRUE)
        
        J <- colSums((w * p_i * (1 - p_i)) * Xn, na.rm = TRUE)
        names(J) <- bn
        J
    }
    
    J_o <- grad_std(0, "non-traditional")
    J_t <- grad_std(1, "traditional")
    
    diff <- as.numeric(a["est"] - b["est"])
    se   <- sqrt(max(.qform(J_o - J_t, Vcr), 0))
    z    <- diff / se
    p    <- 2 * pnorm(-abs(z))
    lb   <- max(diff - 1.96 * se, -1)
    ub   <- min(diff + 1.96 * se,  1)
    
    tibble::tibble(
        other_std = a["est"], other_std_lb = a["lb"], other_std_ub = a["ub"],
        thresh_std = b["est"], thresh_std_lb = b["lb"], thresh_std_ub = b["ub"],
        diff_std = diff, diff_std_lb = lb, diff_std_ub = ub, p_std = p
    )
}

compare_methods_overall <- function(met) {
    dfm <- d_all |> dplyr::filter(metric == met) |> dplyr::mutate(method_group = factor(method_group, levels = c("non-traditional","traditional")))
    if (nrow(dfm) < 3 || dplyr::n_distinct(dfm$method_group[!is.na(dfm$method_group)]) < 2) return(NULL)
    fit <- fit_joint_metric(dfm, include_domain = TRUE); if (is.null(fit)) return(NULL)
    contrast_methods(fit, dfm) |> dplyr::mutate(metric = met)
}

compare_methods_within_domain <- function(dom, met) {
    dfm <- d_all |> dplyr::filter(metric == met, as.character(domain) == as.character(dom)) |>
        dplyr::mutate(method_group = factor(method_group, levels = c("non-traditional","traditional")))
    if (nrow(dfm) < 3 || dplyr::n_distinct(dfm$method_group[!is.na(dfm$method_group)]) < 2) return(NULL)
    fit <- fit_joint_metric(dfm, include_domain = FALSE); if (is.null(fit)) return(NULL)
    contrast_methods(fit, dfm) |> dplyr::mutate(domain = as.character(dom), metric = met)
}

mets <- c("F1","Sensitivity","Specificity")

res_cmp_overall <- purrr::map_dfr(mets, compare_methods_overall)
if (nrow(res_cmp_overall)) readr::write_csv(res_cmp_overall, file.path(out_dir, "tables", "method_contrast_results_overall.csv"))

res_cmp_domain <- NULL
if (isTRUE(run_domain_method_contrasts)) {
    doms <- sort(unique(as.character(d_all$domain)))
    res_cmp_domain <- purrr::map_dfr(doms, \(d) purrr::map_dfr(mets, \(m) compare_methods_within_domain(d, m)))
    if (nrow(res_cmp_domain)) readr::write_csv(res_cmp_domain, file.path(out_dir, "tables", "method_contrast_by_domain.csv"))
}

# -----------------------------
# Descriptives
# -----------------------------
desc_overall <- tibble::tibble(
    item  = c("Validations (rows)","Unique studies","Domains"),
    value = c(nrow(dat0), dplyr::n_distinct(dat0$id), dplyr::n_distinct(dat0$domain))
)

summ_metric_df <- function(metric_col) {
    sub <- dat0[!is.na(dat0[[metric_col]]), , drop = FALSE]
    pv  <- suppressWarnings(as.numeric(sub$n_participants))
    pv  <- pv[is.finite(pv)]
    per_study <- sub |>
        dplyr::group_by(id) |>
        dplyr::summarise(n = suppressWarnings(max(n_participants, na.rm = TRUE)), .groups = "drop") |>
        dplyr::filter(is.finite(n))
    qs <- if (length(pv)) stats::quantile(pv, c(.25,.5,.75), na.rm = TRUE) else c(NA,NA,NA)
    tibble::tibble(
        metric = metric_col,
        validations = nrow(sub),
        studies_reporting = nrow(per_study),
        participants_total_approx = sum(per_study$n, na.rm = TRUE),
        median_n_per_validation = qs[2],
        iqr_low = qs[1],
        iqr_high = qs[3]
    )
}

desc_by_metric <- dplyr::bind_rows(
    summ_metric_df("f1_mean"),
    summ_metric_df("sens_mean"),
    summ_metric_df("spec_mean")
)

desc_by_domain <- d_all |>
    dplyr::group_by(domain) |>
    dplyr::summarise(
        validations = dplyr::n(),
        studies = dplyr::n_distinct(id),
        median_n = stats::median(n_participants, na.rm = TRUE),
        iqr_low  = stats::quantile(n_participants, 0.25, na.rm = TRUE),
        iqr_high = stats::quantile(n_participants, 0.75, na.rm = TRUE),
        .groups = "drop"
    )

readr::write_csv(desc_overall,   file.path(out_dir, "tables", "descriptives_overall.csv"))
readr::write_csv(desc_by_metric, file.path(out_dir, "tables", "descriptives_by_metric.csv"))
readr::write_csv(desc_by_domain, file.path(out_dir, "tables", "descriptives_by_domain.csv"))

qc_boundary <- d_all |>
    dplyr::group_by(domain, method_group, metric) |>
    dplyr::summarise(
        all_ge_0999 = all(mean >= 0.999, na.rm = TRUE),
        all_le_0001 = all(mean <= 0.001, na.rm = TRUE),
        k = dplyr::n(),
        .groups = "drop"
    )
readr::write_csv(qc_boundary, file.path(out_dir, "tables", "qc_boundary_flags.csv"))

# -----------------------------
# Forest plots (patched to match manuscript)
# -----------------------------
build_plot_df <- function(d, metric_name) {
    d |>
        dplyr::filter(metric == metric_name) |>
        dplyr::mutate(
            se_use = sqrt(vi_use),
            est    = mean,
            ci_l   = inv_logit(yi - 1.96 * se_use),
            ci_u   = inv_logit(yi + 1.96 * se_use),
            auth_year = first_author_year(dplyr::cur_data_all()),
            method_lab = ifelse(is.na(method) | method == "", "Method n.r.", method),
            row_lab = glue::glue("{auth_year} - {method_lab} - {val_id}")
        ) |>
        dplyr::arrange(est) |>
        dplyr::mutate(row_lab = forcats::fct_inorder(row_lab))
}

get_pooled <- function(metric_name, res_tbl) {
    r <- res_tbl |> dplyr::filter(metric == metric_name)
    if (!nrow(r)) return(c(est = NA_real_, lb = NA_real_, ub = NA_real_))
    c(est = as.numeric(r$pooled_std_est), lb = as.numeric(r$pooled_std_lb), ub = as.numeric(r$pooled_std_ub))
}

get_pooled_domain <- function(metric_name, dom, res_tbl_domain) {
    r <- res_tbl_domain |> dplyr::filter(metric == metric_name, domain == dom)
    if (!nrow(r)) return(c(est = NA_real_, lb = NA_real_, ub = NA_real_))
    c(est = as.numeric(r$pooled_std_est), lb = as.numeric(r$pooled_std_lb), ub = as.numeric(r$pooled_std_ub))
}

plot_forest <- function(df, pooled, title_txt, file_name, style = plt_style) {
    
    dom_levels <- style$domain_levels[style$domain_levels %in% unique(as.character(df$domain))]
    if (!length(dom_levels)) dom_levels <- sort(unique(as.character(df$domain)))
    dom_pal <- make_domain_palette(dom_levels, style)
    
    df <- df |>
        dplyr::mutate(
            domain = factor(as.character(domain), levels = dom_levels),
            vi_source = factor(as.character(vi_source), levels = names(style$shape_vals))
        )
    
    have_ci <- is.finite(pooled["est"]) && is.finite(pooled["lb"]) && is.finite(pooled["ub"])
    subtitle_txt <- if (have_ci) {
        paste0("Pooled standardised estimate [95% CI] = ",
               scales::percent(pooled["est"], accuracy = 0.1), " [",
               scales::percent(pooled["lb"], accuracy = 0.1), ", ",
               scales::percent(pooled["ub"], accuracy = 0.1), "]")
    } else if (is.finite(pooled["est"])) {
        paste0("Pooled standardised estimate [95% CI] = ",
               scales::percent(pooled["est"], accuracy = 0.1), " [NA, NA]")
    } else {
        "Pooled estimate not available"
    }
    
    p <- ggplot(df, aes(x = est, y = row_lab)) +
        { if (is.finite(pooled["est"])) geom_vline(xintercept = pooled["est"], linetype = 2, linewidth = 0.5, colour = "grey40", na.rm = TRUE) } +
        geom_pointrange(aes(xmin = ci_l, xmax = ci_u, shape = vi_source, colour = domain),
                        size = style$point_size, linewidth = style$line_width, alpha = style$alpha) +
        scale_x_continuous(labels = scales::percent_format(accuracy = style$x_label_accuracy), limits = style$x_limits) +
        scale_shape_manual(
            name   = "Variance source",
            values = style$shape_vals,
            labels = style$shape_labs
        ) +
        scale_colour_manual(
            name   = "Movement behaviour\ndomain",
            values = dom_pal,
            breaks = dom_levels,
            drop   = FALSE
        ) +
        labs(title = title_txt, subtitle = subtitle_txt, x = "Proportion", y = "Validation rows") +
        theme_minimal(base_size = 12, base_family = "Arial") +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank(),
            legend.position    = "bottom",
            legend.box         = "vertical",
            plot.margin        = margin(8, 70, 8, 8)
        ) +
        coord_cartesian(clip = "off")
    
    fpath <- file.path(out_dir, "figures", file_name)
    ragg::agg_png(fpath,
                  width  = style$width,
                  height = style$base_h + style$row_px * nrow(df),
                  res = style$res)
    print(p); dev.off()
    invisible(NULL)
}

for (m in mets) {
    df <- build_plot_df(d_all, m)
    pooled <- get_pooled(m, res_all)
    plot_forest(df, pooled, paste0("Forest-style plot: ", m), paste0("forest_", m, "_all.png"))
}

if (run_method_subgroups) {
    d_thr <- d_all |> dplyr::filter(method_group == "traditional")
    d_oth <- d_all |> dplyr::filter(method_group == "non-traditional")
    for (m in mets) {
        df <- build_plot_df(d_thr, m); pooled <- get_pooled(m, res_thr)
        plot_forest(df, pooled, paste0("Forest-style plot: ", m, " (traditional)"), paste0("forest_", m, "_traditional.png"))
        df <- build_plot_df(d_oth, m); pooled <- get_pooled(m, res_oth)
        plot_forest(df, pooled, paste0("Forest-style plot: ", m, " (non-traditional)"), paste0("forest_", m, "_other.png"))
    }
}

if (!is.null(res_dom) && nrow(res_dom)) {
    doms <- sort(unique(as.character(d_all$domain)))
    for (d in doms) {
        for (m in mets) {
            df <- build_plot_df(d_all |> dplyr::filter(as.character(domain) == d), m)
            pooled <- get_pooled_domain(m, d, res_dom)
            safe_dom <- stringr::str_replace_all(d, "[^A-Za-z0-9]+", "_")
            plot_forest(df, pooled, paste0("Forest-style plot: ", m, " (", d, ")"),
                        paste0("forest_", m, "_bydomain_", safe_dom, ".png"))
        }
    }
}

cat("Done. Outputs saved to: ", normalizePath(out_dir), "\n")
