# Study identifiers and cluster-robust delta-method inference.
validate_study_keys <- function(dat) {
  stopifnot(all(c("id", "study", "val_id") %in% names(dat)))
  if (any(is.na(dat$id) | trimws(as.character(dat$id)) == "")) stop("Missing unique study id")
  if (any(is.na(dat$val_id) | trimws(as.character(dat$val_id)) == "")) stop("Missing validation id")
  if (anyDuplicated(dat$val_id)) stop("Duplicate validation id")
  labels <- split(as.character(dat$study), as.character(dat$id))
  if (any(vapply(labels, function(x) length(unique(x)) != 1L, logical(1)))) stop("One study id has conflicting labels")
  invisible(TRUE)
}

robust_delta <- function(model, data, estimate, gradient, bounds = c(-Inf, Inf)) {
  gradient <- as.numeric(gradient)
  stopifnot(length(gradient) == ncol(model$X))
  clusters <- factor(data$study_id)
  type <- if (nlevels(clusters) >= 6L) "CR2" else "CR1S"
  V <- clubSandwich::vcovCR(model, cluster = clusters, type = type)
  se <- sqrt(max(as.numeric(crossprod(gradient, V %*% gradient)), 0))
  # The contrast is the first-order linearisation gradient. Its covariance and
  # Satterthwaite degrees of freedom apply to the original nonlinear estimand;
  # the linear-contrast estimate itself is deliberately not used.
  lc <- clubSandwich::linear_contrast(model, vcov = V,
    contrasts = matrix(gradient, nrow = 1), test = "Satterthwaite")
  df <- as.numeric(lc$df[1])
  if (!is.finite(df) || df <= 0 || !is.finite(se) || se <= 0)
    return(c(est = estimate, lb = NA_real_, ub = NA_real_, se = se, df = df, p = NA_real_))
  crit <- stats::qt(0.975, df)
  c(est = estimate,
    lb = max(bounds[1], estimate - crit * se),
    ub = min(bounds[2], estimate + crit * se),
    se = se, df = df, p = 2 * stats::pt(-abs(estimate / se), df))
}
