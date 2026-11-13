# Final model tables and the classifier views supported by manuscript mode.
suppressPackageStartupMessages({library(dplyr);library(readr);library(jsonlite)})
out <- file.path("analysis", "outputs", "manuscript")
pooled <- read_csv(file.path(out,"Tables/Pooled_Results.csv"),show_col_types=FALSE)
classifier <- read_csv(file.path(out,"Tables/Classifier_Analysis.csv"),show_col_types=FALSE,
                       col_types=cols(val_id=col_character()))
records <- lapply(which(pooled$metric %in% c("F1", "Sensitivity", "Specificity")), function(i) {
  r <- pooled[i, ]
  selected <- filter(classifier,metric==r$metric)
  if(r$domain!="all") selected <- filter(selected,domain==r$domain)
  if(r$method_group!="all") selected <- filter(selected,method_group==r$method_group)
  ids <- selected$val_id
  stopifnot(length(ids) == r$k_rows, !anyDuplicated(ids))
  list(model_id=r$model_id, metric=r$metric, domain=r$domain, method_group=r$method_group,
       validation_ids=sort(ids), est=r$pooled_std_est, low=r$pooled_std_lb, high=r$pooled_std_ub,
       df=r$df_satt, validation_rows=r$k_rows, studies=r$clusters,
       participants=r$summed_validation_N, kept_mods=r$kept_mods,
       I2=r$I2_total, I2_study=r$I2_study, I2_validation=r$I2_validation,
       QE=r$QE, QE_df=r$QE_df, QE_p=r$QE_p, imputed_rows=r$imputed_rows,
       reported_rows=r$reported_derived_rows, covariance_type=r$vcov_type)
})
stopifnot(length(records)==18)
write_json(records, "app/data/manuscript_pooled.json", pretty=TRUE, auto_unbox=TRUE, digits=NA)
message("Exported 18 exact R-derived classifier views with heterogeneity and inference context.")
