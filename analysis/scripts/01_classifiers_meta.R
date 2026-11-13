# =====================================================================
# Classifiers meta-analysis (F1, Sensitivity, Specificity)
# Prepares classifier data and model functions for the manuscript analyses.
# Author: Millen James Theophilus (GitHub: miltheo)
# =====================================================================

rm(list = ls()); gc()

# -----------------------------
# Paths
# -----------------------------

in_dir  <- file.path("analysis", "inputs")


csv_path <- file.path(in_dir, "Extraction_Classifiers.csv")
stopifnot(file.exists(csv_path))


# -----------------------------
# Settings (set as per study results, options available for sensitivity analysis)
# -----------------------------
weight_mode <- "class"                # equal | fold | class | fold_or_external (class set for classifier results)
treat_external_as_fold <- FALSE
weight_by_participants <- TRUE
use_two_level_random <- TRUE
include_env_prot_domain <- TRUE
standardize_over <- "within_group"    # participant-weighted reference distribution within each method group
eps_prob <- 1e-3
domain_levels <- c("Sleep","Activity type","Activity intensity","Energy expenditure")

# Packages
# -----------------------------
req <- c("readr","dplyr","stringr","tidyr","purrr","forcats",
         "janitor","metafor","clubSandwich","glue","tibble","tidyselect")
to_install <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))
source(file.path("analysis", "scripts", "analysis_helpers.R"))

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
    is_thresh <- !stringr::str_detect(m, "non[ -]?linear") & stringr::str_detect(m, "threshold|linear|glm|cut.?point|rule|heuristic|if[- ]?else")
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
if (!is.na(domain_col)) {

    domain_chr <- stringr::str_squish(as.character(dat0[[domain_col]]))
    domain_chr[is.na(domain_chr) | domain_chr == ""] <- "Unknown"

    extra_domain_levels <- sort(setdiff(unique(domain_chr), domain_levels))

    dat0$domain <- factor(
        domain_chr,
        levels = c(domain_levels, extra_domain_levels)
    )

} else {

    dat0$domain <- factor("Unknown")
}
if (!"study" %in% names(dat0)) stop("Column 'study' is required (FirstAuthor Year).")

dat0 <- dat0 |>
    mutate(
        study_id = factor(id),
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

validate_study_keys(dat0)

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

variance_groups <- interaction(d_all$metric, d_all$domain, drop = TRUE)
const_vi <- ave(vi_raw, variance_groups, FUN = function(x) {
    if (!any(is.finite(x))) stop("No usable variance in a metric/domain group")
    max(stats::median(x[is.finite(x)]), 0.05)
})
variance_imputation <- dplyr::tibble(metric = d_all$metric, domain = d_all$domain,
    usable_reported = is.finite(vi_raw), replacement = as.numeric(const_vi)) |>
    dplyr::group_by(metric, domain) |>
    dplyr::summarise(rows = dplyr::n(), observed = sum(usable_reported),
      imputed = sum(!usable_reported), replacement = dplyr::first(replacement), .groups = "drop")

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
    p_i <- clamp_prob(plogis(as.numeric(X %*% as.numeric(fit$model$beta))), eps = eps_prob)
    w <- if (weight_by_n) dfm[[n_col]] else rep(1, nrow(dfm))
    w[!is.finite(w)] <- 0
    if (sum(w) <= 0) stop("No participant weights")
    w <- w / sum(w)
    robust_delta(fit$model, dfm, sum(w * p_i), colSums((w * p_i * (1-p_i))*X), c(0,1))
}

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
    Xn <- X_with_method(as.matrix(fit$X), if (method_level == "traditional") 1 else 0)
    p_i <- clamp_prob(plogis(as.numeric(Xn %*% as.numeric(fit$beta))), eps = eps_prob)
    w <- ifelse(df_metric$method_group == method_level, df_metric$n_participants, 0)
    w[!is.finite(w)] <- 0; w <- w / sum(w)
    robust_delta(fit, df_metric, sum(w*p_i), colSums((w*p_i*(1-p_i))*Xn), c(0,1))
}

contrast_methods <- function(fit, df_metric) {
    a <- standardized_for_method(fit, df_metric, "non-traditional")
    b <- standardized_for_method(fit, df_metric, "traditional")
    grad <- function(md) {
      Xn <- X_with_method(as.matrix(fit$X), md)
      p <- clamp_prob(plogis(as.numeric(Xn %*% as.numeric(fit$beta))), eps = eps_prob)
      method_level <- if (md == 0) "non-traditional" else "traditional"
      w <- ifelse(df_metric$method_group == method_level, df_metric$n_participants, 0)
      w[!is.finite(w)] <- 0; w <- w/sum(w)
      colSums((w*p*(1-p))*Xn)
    }
    d <- robust_delta(fit, df_metric, as.numeric(a["est"]-b["est"]), grad(0)-grad(1), c(-1,1))
    tibble::tibble(other_std=a["est"],other_std_lb=a["lb"],other_std_ub=a["ub"],
      thresh_std=b["est"],thresh_std_lb=b["lb"],thresh_std_ub=b["ub"],
      diff_std=d["est"],diff_std_lb=d["lb"],diff_std_ub=d["ub"],p_std=d["p"],
      df_satt=d["df"],se_delta=d["se"])
}

# Validation-level estimates for the app.
build_plot_df <- function(d, metric_name) {
    d |>
        dplyr::filter(metric == metric_name) |>
        dplyr::mutate(
            se_use = sqrt(vi_use),
            est    = mean,
            ci_l   = pmax(0, pmin(est, inv_logit(yi - 1.96 * se_use))),
            ci_u   = pmin(1, pmax(est, inv_logit(yi + 1.96 * se_use))),
            auth_year = first_author_year(dplyr::cur_data_all()),
            method_lab = ifelse(is.na(method) | method == "", "Method n.r.", method),
            row_lab = glue::glue("{auth_year} - {method_lab} - {val_id}")
        ) |>
        dplyr::arrange(est) |>
        dplyr::mutate(row_lab = forcats::fct_inorder(row_lab))
}

# Validation-level estimates and variances for the app.
app_rows <- build_plot_df(d_all, "F1") |>
  dplyr::bind_rows(build_plot_df(d_all, "Sensitivity"), build_plot_df(d_all, "Specificity")) |>
  dplyr::mutate(study_label = study, study_id = as.character(id))
meta <- dat0 |> dplyr::select(val_id, is_the_model_published_previously, method_sub,
  validation_type, age_group, test_device)
app_rows <- dplyr::left_join(app_rows, meta, by = "val_id")
readr::write_csv(app_rows, file.path("app", "data", "REML_data_long2.csv"))
analysis_result <- list(dat0=dat0, d_all=d_all, variance_imputation=variance_imputation)
