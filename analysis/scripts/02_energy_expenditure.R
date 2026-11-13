# =====================================================================
# Energy expenditure meta-analysis (association models)
# Outcome: MAPE (proportion; lower is better)
# Prepares MAPE data and model functions for the manuscript analyses.
# Author: Millen James Theophilus (GitHub: miltheo)
# =====================================================================

rm(list = ls()); gc()

# -----------------------------
# Paths
# -----------------------------
in_dir  <- file.path("analysis", "inputs")


csv_path <- file.path(in_dir, "Extraction_EnergyExpenditure.csv")
stopifnot(file.exists(csv_path))


# -----------------------------
# Settings (set as per study results, options available for sensitivity analysis)
# -----------------------------
weight_mode <- "fold"                 # equal | fold | fold_or_external
weight_by_participants <- TRUE
use_two_level_random <- TRUE
include_env_prot_domain <- TRUE
standardize_over <- "within_group"    # participant-weighted reference distribution within each method group


# -----------------------------
# Packages
# -----------------------------
req <- c("readr","dplyr","stringr","tidyr","purrr",
         "janitor","metafor","clubSandwich","glue","tibble","tidyselect")
to_install <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))
source(file.path("analysis", "scripts", "analysis_helpers.R"))

# -----------------------------
# Helpers
# -----------------------------
na_tokens <- c("NA","NaN","","n.r.","n.r","n.a.","n.a","n/a","N/A","NR","nr")

classify_method_group <- function(method_chr) {
    s <- stringr::str_to_lower(dplyr::coalesce(method_chr, ""))
    is_linear <- !stringr::str_detect(s, "non[ -]?linear") & stringr::str_detect(s, "linear|glm|heuristic|threshold|cut.?point|rule")
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
        study_id = factor(id),
        row_id = dplyr::row_number(),
        val_id = dplyr::coalesce(as.character(coalesce_col(dat0, c("val_id","validation_id"), NA)), as.character(row_id)),
        k_folds = suppressWarnings(as.numeric(k_folds)),
        n_participants = suppressWarnings(as.numeric(n_participants)),
        sd_origin = stringr::str_to_lower(dplyr::coalesce(sd_origin, NA_character_)),
        test_environment = factor(dplyr::coalesce(test_environment, NA_character_)),
        test_protocol = factor(dplyr::coalesce(test_protocol, NA_character_)),
        method_group = classify_method_group(method)
    )

validate_study_keys(dat0)

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
    eta <- as.numeric(X %*% align_beta_to_X(fit$model$beta, X))
    w <- dfm$n_participants; w[!is.finite(w)] <- 0; w <- w/sum(w)
    robust_delta(fit$model, dfm, sum(w*eta), wcolsum(X,w))
}

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
    Xn <- X_with_method(X, if (method_level == "traditional") 1 else 0)
    eta <- as.numeric(Xn %*% align_beta_to_X(fit$beta,X))
    w <- ifelse(dfm$method_group == method_level, dfm$n_participants, 0)
    w[!is.finite(w)] <- 0; w <- w/sum(w)
    robust_delta(fit,dfm,sum(w*eta),wcolsum(Xn,w))
}

contrast_methods <- function(fit, dfm) {
    a <- standardized_for_method(fit,dfm,"non-traditional")
    b <- standardized_for_method(fit,dfm,"traditional")
    w0 <- ifelse(dfm$method_group == "non-traditional", dfm$n_participants, 0)
    w1 <- ifelse(dfm$method_group == "traditional", dfm$n_participants, 0)
    w0[!is.finite(w0)] <- 0; w1[!is.finite(w1)] <- 0
    w0 <- w0/sum(w0); w1 <- w1/sum(w1)
    X <- as.matrix(fit$X)
    g <- wcolsum(X_with_method(X,0),w0)-wcolsum(X_with_method(X,1),w1)
    d <- robust_delta(fit,dfm,as.numeric(a["est"]-b["est"]),g)
    tibble::tibble(other_std=a["est"],other_std_lb=a["lb"],other_std_ub=a["ub"],
      linear_std=b["est"],linear_std_lb=b["lb"],linear_std_ub=b["ub"],
      diff_std=d["est"],diff_std_lb=d["lb"],diff_std_ub=d["ub"],p_std=d["p"],
      df_satt=d["df"],se_delta=d["se"])
}

analysis_result <- list(dat0=dat0, d_ee=d_ee)
