suppressPackageStartupMessages({library(readr);library(dplyr);library(metafor);library(clubSandwich);library(stringr);library(tibble)})
tables <- file.path("analysis","outputs","manuscript","Tables")
dir.create(tables,recursive=TRUE,showWarnings=FALSE)
repo <- "."
write_results <- function(data, name) {
 path <- file.path(tables,paste0(name,".csv"))
 if(file.exists(path)) {
  old <- read_csv(path,show_col_types=FALSE)
  for(key in intersect(names(old),names(data))) if(is.character(data[[key]])) old[[key]] <- as.character(old[[key]])
  old <- if("model_id" %in% names(old)) filter(old,!grepl("^EE_RMSE_",model_id)) else filter(old,metric!="RMSE")
  data <- bind_rows(old,data)
 }
 write_csv(data,path)
}
# Load the energy-expenditure model and inference functions.
load_defs<-function(path,env){for(e in parse(path))if(is.call(e)&&identical(e[[1]],as.name("<-"))&&length(e)>=3&&is.call(e[[3]])&&identical(e[[3]][[1]],as.name("function")))eval(e,envir=env)}
env<-new.env(parent=globalenv());sys.source(file.path(repo,"analysis/scripts/analysis_helpers.R"),env)
load_defs(file.path(repo,"analysis/scripts/02_energy_expenditure.R"),env)
env$use_two_level_random<-TRUE;env$include_env_prot_domain<-TRUE
env$weight_by_participants<-TRUE;env$standardize_over<-"within_group"
aux<-new.env(parent=globalenv());load_defs("analysis/scripts/05_model_context.R",aux)
raw<-read_csv("analysis/inputs/Extraction_EnergyExpenditure.csv",col_types=cols(.default=col_character()),show_col_types=FALSE)|>filter(!is.na(val_id)&val_id!="")
na_tokens<-c("","NA","NaN","n.r.","n.r","n.a.","n.a","n/a","N/A","NR","nr")
num<-function(x)suppressWarnings(as.numeric(x))
clean<-function(x){x[x%in%na_tokens]<-NA_character_;x}
raw<-raw|>mutate(across(everything(),clean));env$validate_study_keys(raw)
audit<-raw|>transmute(val_id,id,study,rmse_mean_raw=rmse_met_mean,rmse_sd_raw=rmse_met_sd,
 mean=num(rmse_met_mean),sd=num(rmse_met_sd),mean_parse_failure=!is.na(rmse_met_mean)&!is.finite(mean),sd_parse_failure=!is.na(rmse_met_sd)&!is.finite(sd),
 selected=is.finite(mean),selection_reason=if_else(is.finite(mean),"Numeric RMSE recorded in METs","No numeric MET RMSE extracted"))
if(any(audit$mean_parse_failure)) stop("Non-numeric RMSE means in the extraction input")
ds<-raw|>mutate(row_id=row_number())|>transmute(val_id,id,study,study_id=factor(id),row_id,method,
 method_group=factor(env$classify_method_group(method),levels=c("non-traditional","traditional")),
 sd_origin=str_to_lower(sd_origin),k_folds=num(k_folds),n_participants=num(n_participants),
 test_environment=factor(test_environment),test_protocol=factor(test_protocol),metric="RMSE",
 mean=num(rmse_met_mean),sd=num(rmse_met_sd))|>filter(is.finite(mean))
stopifnot(all(ds$mean>=0),all(is.na(ds$sd)|ds$sd>=0),all(is.na(ds$n_participants)|ds$n_participants>0))
# Fold-derived working variances on the RMSE MET scale.
ds<-ds|>mutate(vi_fold=if_else(sd_origin=="cv_folds"&is.finite(sd)&is.finite(k_folds)&k_folds>0,sd^2/k_folds,NA_real_),
 vi_ext=if_else(sd_origin=="external_test"&is.finite(sd),sd^2,NA_real_))
replacement<-max(if(any(is.finite(ds$vi_fold)))median(ds$vi_fold,na.rm=TRUE)else .0025,1e-5)
mean_k<-mean(ds$k_folds,na.rm=TRUE)
ds<-ds|>mutate(vi_use=pmax(if_else(is.finite(vi_fold),vi_fold,replacement),1e-6),vi_source=if_else(is.finite(vi_fold),"cv_folds","constant"),
 miss_k_folds=as.integer(!is.finite(k_folds)),k_folds_imp=coalesce(k_folds,mean_k),c_k_folds=k_folds_imp-mean_k,
 env_re=factor(test_environment),prot_re=factor(test_protocol),domain="Energy expenditure",ci_lower=pmax(0,mean-1.96*sqrt(vi_use)),ci_upper=mean+1.96*sqrt(vi_use),
 reference_weight=if_else(is.finite(n_participants),n_participants,0)/sum(n_participants,na.rm=TRUE),
 units="MET",sd_parse_failure=val_id%in%audit$val_id[audit$sd_parse_failure])
stopifnot(nrow(ds)>0,all(is.finite(ds$vi_use)),abs(sum(ds$reference_weight)-1)<1e-12)
hets<-list();sens<-list()
capture_fit<-function(fun){warns<-character();fit<-withCallingHandlers(fun(),warning=function(w){warns<<-c(warns,conditionMessage(w));invokeRestart("muffleWarning")});if(is.null(fit))stop("RMSE model failed; no fallback substituted");list(fit=fit,warnings=unique(warns))}
store<-function(m,d,id,kind,warnings){
 h<-aux$heterogeneity(m,d,id,"RMSE","all",if(kind=="pooled")"all"else"joint",kind,warnings)|>mutate(variance_scale="MET squared")
 hets[[id]]<<-h;h
}

cap<-capture_fit(function()env$fit_metric(ds));fit<-cap$fit;est<-env$pooled_standardized(fit,ds)
stopifnot(all(is.finite(est)))
id<-"EE_RMSE_all_all";h<-store(fit$model,ds,id,"pooled",cap$warnings)
pooled<-tibble(model_id=id,metric="RMSE",domain="all",method_group="all",k_rows=nrow(ds),clusters=n_distinct(ds$id),vcov_type="CR2",kept_mods=paste(fit$kept_mods,collapse=", "),
 pooled_std_est=unname(est['est']),pooled_std_lb=unname(est['lb']),pooled_std_ub=unname(est['ub']),df_satt=unname(est['df']),robust_SE=unname(est['se']),summed_validation_N=sum(ds$n_participants,na.rm=TRUE))|>left_join(select(h,-metric,-domain,-method_group,-k_rows,-clusters),by="model_id")
capj<-capture_fit(function()env$fit_joint(ds));cmp<-env$contrast_methods(capj$fit,ds)
stopifnot(all(is.finite(unlist(cmp))))
store(capj$fit,ds,"EE_RMSE_all_joint","method_contrast",capj$warnings)
cmp<-cmp|>mutate(model_id="EE_RMSE_all_joint",metric="RMSE",domain="all",.before=1)
for(mult in c(.5,1,2)){
 d<-ds;d$vi_use[d$vi_source=="constant"]<-d$vi_use[d$vi_source=="constant"]*mult
 cf<-if(mult==1)list(fit=fit,warnings=character())else capture_fit(function()env$fit_metric(d))
 e<-env$pooled_standardized(cf$fit,d);hh<-aux$heterogeneity(cf$fit$model,d,id,"RMSE","all","all","variance_sensitivity",cf$warnings)|>mutate(variance_scale="MET squared")
 stopifnot(identical(as.matrix(cf$fit$model$X),as.matrix(fit$model$X)))
 sens[[as.character(mult)]]<-hh|>mutate(multiplier=mult,pooled_std_est=unname(e['est']),pooled_std_lb=unname(e['lb']),pooled_std_ub=unname(e['ub']),df_satt=unname(e['df']),changed_columns=if(mult==1)"none"else"vi_use on imputed rows only",floor_reapplied=FALSE,.after=model_id)
}
write_csv(ds,file.path(tables,"EE_RMSE_Analysis.csv"))
write_results(pooled,"Pooled_Results")
write_results(cmp,"Method_Contrasts")
write_results(bind_rows(hets),"Model_Heterogeneity")
write_results(bind_rows(sens),"Variance_Sensitivity")
write_results(tibble(metric="RMSE",domain="Energy expenditure",rows=nrow(ds),observed=sum(ds$vi_source!="constant"),imputed=sum(ds$vi_source=="constant"),replacement=replacement,variance_scale="MET squared"),"Variance_Imputation")
print(pooled);print(cmp);message("RMSE analysis complete.")
