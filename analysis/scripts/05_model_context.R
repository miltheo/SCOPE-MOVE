suppressPackageStartupMessages({library(dplyr);library(readr);library(metafor);library(clubSandwich);library(stringr);library(tibble)})
out<-file.path("analysis","outputs","manuscript");repo<-"."
for(folder in c("Tables","Figures/Main","Figures/Supplementary"))dir.create(file.path(out,folder),recursive=TRUE,showWarnings=FALSE)
tables<-file.path(out,"Tables")
cx <- classifier_results; ee <- energy_results
load_functions<-function(path){
 env<-new.env(parent=globalenv());sys.source("analysis/scripts/analysis_helpers.R",envir=env)
 for(e in parse(path))if(is.call(e)&&identical(e[[1]],as.name("<-"))&&is.call(e[[3]])&&identical(e[[3]][[1]],as.name("function")))eval(e,envir=env)
 env$use_two_level_random<-TRUE;env$include_env_prot_domain<-TRUE;env$eps_prob<-1e-3
 env$weight_by_participants<-TRUE;env$standardize_over<-"within_group"
 env
}
cl_env<-load_functions("analysis/scripts/01_classifiers_meta.R");ee_env<-load_functions("analysis/scripts/02_energy_expenditure.R")
stopifnot(all(as.character(cx$d_all$study_id)==as.character(cx$d_all$id)),all(as.character(ee$d_ee$study_id)==as.character(ee$d_ee$id)))
cl_rows<-cx$d_all|>mutate(ci_lower=pmax(0,pmin(mean,plogis(yi-1.96*sqrt(vi_use)))),ci_upper=pmin(1,pmax(mean,plogis(yi+1.96*sqrt(vi_use)))))
ee_rows<-ee$d_ee|>mutate(domain="Energy expenditure",ci_lower=pmax(0,mean-1.96*sqrt(vi_use)),ci_upper=mean+1.96*sqrt(vi_use))
write_csv(cl_rows,file.path(tables,"Classifier_Analysis.csv"));write_csv(ee_rows,file.path(tables,"EE_Analysis.csv"))

baseline_specs<-list();pooled_rows<-list();diagnostics<-list();comparisons<-list()
model_id<-function(metric,domain="all",group="all",joint=FALSE,energy=FALSE)gsub("[ -]","_",paste(if(energy)"EE"else"CL",metric,domain,if(joint)"joint"else group,sep="_"))
fit_capture<-function(fun){
 warnings<-character();value<-withCallingHandlers(fun(),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
 if(is.null(value))stop("Model did not fit; no fallback model is substituted.")
 list(value=value,warnings=unique(warnings))
}
heterogeneity<-function(m,ds,id,metric,domain,group,kind,warnings){
 w<-1/ds$vi_use;X<-as.matrix(m$X);q<-qr(sqrt(w)*X)
 stopifnot(q$rank==ncol(X),length(m$sigma2)==2,m$k>m$p)
 traceP<-sum(w*(1-rowSums(qr.Q(q)^2)));typical_vi<-(m$k-m$p)/traceP
 W<-diag(w);P<-W-W%*%X%*%solve(crossprod(X,W%*%X))%*%t(X)%*%W
 stopifnot(typical_vi>0,abs(sum(diag(P))-traceP)<1e-6*max(1,traceP))
 q_check<-sum(w*lm.wfit(X,m$yi,w=w)$residuals^2)
 stopifnot(abs(q_check-m$QE)<1e-7*max(1,m$QE),abs(pchisq(m$QE,m$k-m$p,lower.tail=FALSE)-m$QEp)<1e-10)
 denominator<-sum(m$sigma2)+typical_vi
 tibble(model_id=id,metric=metric,domain=domain,method_group=group,model_kind=kind,k_rows=nrow(ds),clusters=n_distinct(ds$study_id),fixed_effect_columns=m$p,imputed_rows=sum(ds$vi_source=="constant"),reported_derived_rows=sum(ds$vi_source!="constant"),study_variance=m$sigma2[1],validation_variance=m$sigma2[2],total_residual_variance=sum(m$sigma2),variance_scale=if(metric=="MAPE")"proportion squared"else"logit squared",typical_working_variance=typical_vi,I2_total=100*sum(m$sigma2)/denominator,I2_study=100*m$sigma2[1]/denominator,I2_validation=100*m$sigma2[2]/denominator,QE=m$QE,QE_df=m$k-m$p,QE_p=m$QEp,QE_log_p=pchisq(m$QE,m$k-m$p,lower.tail=FALSE,log.p=TRUE),component_near_boundary=any(m$sigma2<1e-6),warnings=paste(warnings,collapse=" | "))
}
store_model<-function(m,ds,id,metric,domain,group,kind,warnings){
 diag<-heterogeneity(m,ds,id,metric,domain,group,kind,warnings)
 diagnostics[[id]]<<-diag
 diag
}

fit_pooled<-function(ds,metric,domain="all",group="all",energy=FALSE){
 env<-if(energy)ee_env else cl_env;id<-model_id(metric,domain,group,energy=energy)
 cap<-fit_capture(function()env$fit_metric(ds));fit<-cap$value;m<-fit$model
 est<-if(energy)env$pooled_standardized(fit,ds)else env$pooled_standardized(fit,ds,weight_by_n=TRUE)
 stopifnot(all(is.finite(est[c("est","lb","ub","se","df")])))
 h<-store_model(m,ds,id,metric,domain,group,"pooled",cap$warnings)
 row<-tibble(model_id=id,metric=metric,domain=domain,method_group=group,k_rows=nrow(ds),clusters=n_distinct(ds$study_id),vcov_type=if(n_distinct(ds$study_id)>=6)"CR2"else"CR1S",kept_mods=paste(fit$kept_mods,collapse=", "),pooled_std_est=unname(est["est"]),pooled_std_lb=unname(est["lb"]),pooled_std_ub=unname(est["ub"]),df_satt=unname(est["df"]),robust_SE=unname(est["se"]),summed_validation_N=sum(ds$n_participants))
 pooled_rows[[id]]<<-left_join(row,select(h,-metric,-domain,-method_group,-k_rows,-clusters),by="model_id")
 baseline_specs[[id]]<<-list(data=ds,env=env,energy=energy,metric=metric,domain=domain,group=group,fit=fit)
 cat("Pooled: ",id,"; I2 = ",round(h$I2_total,2),"%\n",sep="");flush.console()
}
for(metric in c("F1","Sensitivity","Specificity")){
 ds<-filter(cx$d_all,.data$metric==.env$metric)
 fit_pooled(ds,metric)
 for(dom in c("Activity intensity","Activity type","Sleep"))fit_pooled(filter(ds,as.character(domain)==dom),metric,dom)
 for(group in c("traditional","non-traditional"))
  fit_pooled(filter(ds,as.character(method_group)==group),metric,group=group)
}
fit_pooled(ee$d_ee,"MAPE",domain="all",energy=TRUE)

for(metric in c("F1","Sensitivity","Specificity"))for(domain in c("all","Activity intensity","Activity type","Sleep")){
 ds<-filter(cx$d_all,.data$metric==.env$metric)
 if(domain!="all")ds<-filter(ds,as.character(.data$domain)==.env$domain)
 ds<-mutate(ds,method_group=factor(method_group,levels=c("non-traditional","traditional")))
 id<-model_id(metric,domain,joint=TRUE)
 cap<-fit_capture(function()cl_env$fit_joint_metric(ds,include_domain=domain=="all"));m<-cap$value
 result<-cl_env$contrast_methods(m,ds)
 store_model(m,ds,id,metric,domain,"joint","method_contrast",cap$warnings)
 comparisons[[id]]<-mutate(result,model_id=id,metric=metric,domain=domain,.before=1)
 cat("Contrast: ",id,"\n",sep="");flush.console()
}
ds<-mutate(ee$d_ee,method_group=factor(method_group,levels=c("non-traditional","traditional")))
id<-model_id("MAPE",joint=TRUE,energy=TRUE)
cap<-fit_capture(function()ee_env$fit_joint(ds));result<-ee_env$contrast_methods(cap$value,ds)
store_model(cap$value,ds,id,"MAPE","all","joint","method_contrast",cap$warnings)
comparisons[[id]]<-mutate(result,model_id=id,metric="MAPE",domain="all",.before=1)

# Sensitivity to the working variances assigned to imputed rows.
sensitivity<-list()
for(id in names(baseline_specs)){
 spec<-baseline_specs[[id]];ds0<-spec$data
 for(multiplier in c(.5,1,2)){
  ds<-ds0;imputed<-as.character(ds$vi_source)=="constant"
  ds$vi_use[imputed]<-ds$vi_use[imputed]*multiplier
  if(multiplier==1||!any(imputed)){fit<-spec$fit;warns<-character()}else{
   cap<-fit_capture(function()spec$env$fit_metric(ds));fit<-cap$value;warns<-cap$warnings
  }
  stopifnot(identical(as.matrix(fit$model$X),as.matrix(spec$fit$model$X)),identical(ds$vi_use[!imputed],ds0$vi_use[!imputed]))
  est<-if(spec$energy)spec$env$pooled_standardized(fit,ds)else spec$env$pooled_standardized(fit,ds,weight_by_n=TRUE)
  h<-heterogeneity(fit$model,ds,id,spec$metric,spec$domain,spec$group,"variance_sensitivity",warns)
  sensitivity[[paste(id,multiplier)]]<-mutate(h,multiplier=multiplier,pooled_std_est=unname(est["est"]),pooled_std_lb=unname(est["lb"]),pooled_std_ub=unname(est["ub"]),df_satt=unname(est["df"]),changed_columns=if(multiplier==1||!any(imputed))"none"else"vi_use on imputed rows only",floor_reapplied=FALSE,.after=model_id)
 }
 cat("Variance sensitivity: ",id,"\n",sep="");flush.console()
}
pooled<-bind_rows(pooled_rows);hetero<-bind_rows(diagnostics);contrasts<-bind_rows(comparisons);sens<-bind_rows(sensitivity)
write_csv(pooled,file.path(tables,"Pooled_Results.csv"));write_csv(contrasts,file.path(tables,"Method_Contrasts.csv"))
write_csv(hetero,file.path(tables,"Model_Heterogeneity.csv"));write_csv(sens,file.path(tables,"Variance_Sensitivity.csv"))
imputation<-cx$variance_imputation|>mutate(variance_scale="logit squared")
ee_imp<-tibble(metric="MAPE",domain="Energy expenditure",rows=nrow(ee$d_ee),observed=sum(ee$d_ee$vi_source!="constant"),imputed=sum(ee$d_ee$vi_source=="constant"),replacement=unique(ee$d_ee$vi_use[ee$d_ee$vi_source=="constant"])[1],variance_scale="proportion squared")
write_csv(bind_rows(imputation,ee_imp),file.path(tables,"Variance_Imputation.csv"))

message("Pooled models, method contrasts, heterogeneity and variance sensitivity analyses complete.")
