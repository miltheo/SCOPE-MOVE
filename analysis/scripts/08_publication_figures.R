# Publication artwork from the regenerated full-precision analysis tables.
suppressPackageStartupMessages({library(readr);library(dplyr);library(ragg)})
out<-file.path("analysis","outputs","manuscript")
# Use 16 significant digits for presentation; model fitting and app exports
# retain full precision.
read_plot_table <- function(path) {
  d <- read_csv(path, show_col_types=FALSE)
  raw <- read_csv(path, col_types=cols(.default=col_character()), show_col_types=FALSE)
  if ("mean" %in% names(d)) d$sort_mean <- d$mean
  for (key in names(raw)) if (is.numeric(d[[key]])) {
    value <- as.numeric(raw[[key]])
    finite <- is.finite(value)
    value[finite] <- as.numeric(sprintf("%.16g", value[finite]))
    d[[key]] <- value
  }
  for (key in intersect(c("id","val_id","study_id","row_id"),names(d))) d[[key]] <- as.character(d[[key]])
  as_tibble(d)
}
cl<-read_plot_table(file.path(out,"Tables/Classifier_Analysis.csv"))
ee<-read_plot_table(file.path(out,"Tables/EE_Analysis.csv"))
rmse<-read_plot_table(file.path(out,"Tables/EE_RMSE_Analysis.csv"))
pooled_table<-read_plot_table(file.path(out,"Tables/Pooled_Results.csv"))
jobs <- tibble::tribble(
  ~figure_id, ~figure_number, ~location, ~metric, ~domain, ~method_group, ~model_id, ~plot_folder, ~filename_stem,
  "Main_3", 3L, "Main", "F1", "all", "all", "CL_F1_all_all", "Figures/Main", "Figure_3",
  "Main_4", 4L, "Main", "F1", "Activity intensity", "all", "CL_F1_Activity_intensity_all", "Figures/Main", "Figure_4",
  "Main_5", 5L, "Main", "F1", "Activity type", "all", "CL_F1_Activity_type_all", "Figures/Main", "Figure_5",
  "Main_6", 6L, "Main", "F1", "Sleep", "all", "CL_F1_Sleep_all", "Figures/Main", "Figure_6",
  "S4", 4L, "Supplementary", "Sensitivity", "all", "all", "CL_Sensitivity_all_all", "Figures/Supplementary", "Figure_S4",
  "S5", 5L, "Supplementary", "Specificity", "all", "all", "CL_Specificity_all_all", "Figures/Supplementary", "Figure_S5",
  "S6", 6L, "Supplementary", "Sensitivity", "Activity intensity", "all", "CL_Sensitivity_Activity_intensity_all", "Figures/Supplementary", "Figure_S6",
  "S7", 7L, "Supplementary", "Specificity", "Activity intensity", "all", "CL_Specificity_Activity_intensity_all", "Figures/Supplementary", "Figure_S7",
  "S8", 8L, "Supplementary", "Sensitivity", "Activity type", "all", "CL_Sensitivity_Activity_type_all", "Figures/Supplementary", "Figure_S8",
  "S9", 9L, "Supplementary", "Specificity", "Activity type", "all", "CL_Specificity_Activity_type_all", "Figures/Supplementary", "Figure_S9",
  "S10", 10L, "Supplementary", "Sensitivity", "Sleep", "all", "CL_Sensitivity_Sleep_all", "Figures/Supplementary", "Figure_S10",
  "S11", 11L, "Supplementary", "Specificity", "Sleep", "all", "CL_Specificity_Sleep_all", "Figures/Supplementary", "Figure_S11",
  "S12", 12L, "Supplementary", "MAPE", "Energy expenditure", "all", "EE_MAPE_all_all", "Figures/Supplementary", "Figure_S12",
  "S13", 13L, "Supplementary", "RMSE", "Energy expenditure", "all", "EE_RMSE_all_all", "Figures/Supplementary", "Figure_S13",
  "S14", 14L, "Supplementary", "F1", "all", "traditional", "CL_F1_all_traditional", "Figures/Supplementary", "Figure_S14",
  "S15", 15L, "Supplementary", "F1", "all", "non-traditional", "CL_F1_all_non_traditional", "Figures/Supplementary", "Figure_S15",
  "S16", 16L, "Supplementary", "Sensitivity", "all", "traditional", "CL_Sensitivity_all_traditional", "Figures/Supplementary", "Figure_S16",
  "S17", 17L, "Supplementary", "Sensitivity", "all", "non-traditional", "CL_Sensitivity_all_non_traditional", "Figures/Supplementary", "Figure_S17",
  "S18", 18L, "Supplementary", "Specificity", "all", "traditional", "CL_Specificity_all_traditional", "Figures/Supplementary", "Figure_S18",
  "S19", 19L, "Supplementary", "Specificity", "all", "non-traditional", "CL_Specificity_all_non_traditional", "Figures/Supplementary", "Figure_S19"
)
caption_text <- tibble::tribble(
  ~figure_id, ~caption,
  "Main_2", "Figure 2.  UpSet plot of device brands used across included studies (n=155). Horizontal bars on the left show the total number of studies using each device brand. Vertical bars at the top show the number of studies using each specific combination of brands. Filled dots indicate which brands are included in a given combination, and vertical lines link brands used within the same study.",
  "Main_3", "Figure 3. Forest-style plot of macro-averaged F1 overall for activity intensity, activity type, and sleep-wake classification (N=5847 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest F1 down panel A and then panel B. ML: machine learning; GLM: general linear model.",
  "Main_4", "Figure 4. Forest-style plot of macro-averaged F1 for activity intensity classification (N=1011 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest F1. ML: machine learning.",
  "Main_5", "Figure 5. Forest-style plot of macro-averaged F1 for activity type classification (N=1450 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest F1. ML: machine learning.",
  "Main_6", "Figure 6. Forest-style plot of macro-averaged F1 for sleep-wake classification (N=3386 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest F1. ML: machine learning; GLM: general linear model.",
  "Main_7", "Figure 7. Quality Assessment of Diagnostic Accuracy Studies-2 (QUADAS-2) risk of bias summary by domain. Horizontal stacked bars show the distribution of risk of bias ratings across included studies for four QUADAS-2 domains: participant selection/study design (patient selection in QUADAS-2), index measure, criterion measure, and flow/timing. Within each domain, segments represent the percentage of studies rated Low (green) or High (orange) risk of bias, with labels indicating the percentage and corresponding number of studies (N). Percentages are calculated within QUADAS-2 domain.",
  "S1", "Figure S1. Annual publication volume and scope of included wrist-accelerometry prediction model studies (2008 to 2026). Stacked bars show the number of included studies published each year. Colored segments indicate the prediction model subgroup, as defined in the key. Traditional methods comprise threshold, heuristic, and general linear model approaches; non-traditional methods comprise classical machine learning (ML), deep learning, probabilistic or temporal, and ensemble approaches.",
  "S2", "Figure S2. UpSet plot of 24-hour movement behavior domain assessed across included studies (n=155). Horizontal bars on the left show the total number of studies in each domain (studies per domain). Vertical bars at the top show the number of studies in each specific combination of domains (intersection size). Filled dots indicate which domains are included in each combination, and vertical lines link domains within the same study.",
  "S3", "Figure S3. UpSet plot of participant age groups represented across included studies (n=155). Horizontal bars on the left show the total number of studies in each age group (studies per age group), including studies with age group not reported (n.r.). Vertical bars at the top show the number of studies in each specific combination of age groups (intersection size). Filled dots indicate which age groups are included in each combination, and vertical lines link age groups within the same study.",
  "S4", "Figure S4. Forest-style plot of macro-averaged sensitivity overall for activity intensity, activity type, and sleep-wake classification (N=9042 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest sensitivity down panel A and then panel B. ML: machine learning; GLM: general linear model.",
  "S5", "Figure S5. Forest-style plot of macro-averaged specificity overall for activity intensity, activity type, and sleep-wake classification (N=8919 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest specificity down panel A and then panel B. ML: machine learning; GLM: general linear model.",
  "S6", "Figure S6. Forest-style plot of macro-averaged sensitivity for activity intensity classification (N=2144 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest sensitivity down panel A and then panel B. ML: machine learning.",
  "S7", "Figure S7. Forest-style plot of macro-averaged specificity for activity intensity classification (N=2144 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest specificity down panel A and then panel B. ML: machine learning.",
  "S8", "Figure S8. Forest-style plot of macro-averaged sensitivity for activity type classification (N=1301 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest sensitivity. ML: machine learning.",
  "S9", "Figure S9. Forest-style plot of macro-averaged specificity for activity type classification (N=1233 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest specificity. ML: machine learning.",
  "S10", "Figure S10. Forest-style plot of macro-averaged sensitivity for sleep-wake classification (N=5597 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest sensitivity down panel A and then panel B. ML: machine learning; GLM: general linear model.",
  "S11", "Figure S11. Forest-style plot of macro-averaged specificity for sleep-wake classification (N=5542 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the domain-specific model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest specificity down panel A and then panel B. ML: machine learning; GLM: general linear model.",
  "S12", "Figure S12. Forest-style plot of mean absolute percentage error for energy expenditure estimation (N=573 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered by increasing MAPE. ML: machine learning; GLM: general linear model.",
  "S13", "Figure S13. Forest-style plot of root mean square error for energy expenditure estimation (N=1456 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered by increasing RMSE. ML: machine learning; GLM: general linear model; METs: metabolic equivalents.",
  "S14", "Figure S14. Forest-style plot of macro-averaged F1 for traditional methods across all classifier validations (N=1253 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the traditional-methods-only model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest F1. GLM: general linear model.",
  "S15", "Figure S15. Forest-style plot of macro-averaged F1 for non-traditional methods across all classifier validations (N=4594 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the non-traditional-methods-only model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest F1. ML: machine learning.",
  "S16", "Figure S16. Forest-style plot of macro-averaged sensitivity for traditional methods across all classifier validations (N=4656 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the traditional-methods-only model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest sensitivity down panel A and then panel B. GLM: general linear model.",
  "S17", "Figure S17. Forest-style plot of macro-averaged sensitivity for non-traditional methods across all classifier validations (N=4386 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the non-traditional-methods-only model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest sensitivity down panel A and then panel B. ML: machine learning.",
  "S18", "Figure S18. Forest-style plot of macro-averaged specificity for traditional methods across all classifier validations (N=4706 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the traditional-methods-only model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest specificity down panel A and then panel B. GLM: general linear model.",
  "S19", "Figure S19. Forest-style plot of macro-averaged specificity for non-traditional methods across all classifier validations (N=4213 participants). Points and whiskers show validation estimates and approximate 95% confidence intervals. The vertical dashed line shows the participant-weighted standardized pooled estimate from the non-traditional-methods-only model, and the black diamond shows its 95% confidence interval. The horizontal dashed line separates validations above the pooled estimate from those at or below it. Validation rows are shown as first author and year, prediction model subgroup, and unique validation ID, ordered from highest to lowest specificity down panel A and then panel B. ML: machine learning."
)
palette<-c("Sleep"="#8E276E","Activity type"="#8A4F00","Activity intensity"="#0072B2","Energy expenditure"="#007A59")
codes<-c("Sleep"="SW","Activity type"="AT","Activity intensity"="AI","Energy expenditure"="EE")
fmt<-function(x,metric){if(metric=="RMSE")return(sprintf("%.2f",x));if(metric=="MAPE")return(sprintf("%.1f",100*x));ifelse(x<1&round(100*x,1)>=100,">99.9",sprintf("%.1f",100*x))}
alts<-list()
for(job_num in seq_len(nrow(jobs))){
 job<-jobs[job_num,];is_rmse<-job$metric=="RMSE";is_ee<-job$metric%in%c("MAPE","RMSE")
 ds<-if(is_rmse)rmse else if(is_ee)ee else cl[cl$metric==job$metric,,drop=FALSE]
 if(!is_ee&&job$domain!="all")ds<-ds[as.character(ds$domain)==job$domain,,drop=FALSE]
 if(job$method_group!="all")ds<-ds[as.character(ds$method_group)==job$method_group,,drop=FALSE]
 ds<-mutate(ds,method_label=as.character(method))
 stopifnot(all(!is.na(ds$method_label)))
 pooled<-pooled_table[pooled_table$model_id==job$model_id,,drop=FALSE]
 stopifnot(nrow(pooled)==1,nrow(ds)==pooled$k_rows,n_distinct(ds$study_id)==pooled$clusters)
 ds<-ds|>mutate(year=as.integer(sub(".*?((19|20)[0-9]{2}).*","\\1",study)))
 ds<-if(is_ee)arrange(ds,sort_mean,year,study,study_id,val_id)else arrange(ds,desc(sort_mean),year,study,study_id,val_id)
 ds<-ds|>mutate(row_label=paste(study,method_label,val_id,sep=" - "),domain_code=unname(codes[as.character(domain)]),
   marker=case_when(domain=="Sleep"~16L,domain=="Activity type"~15L,domain=="Activity intensity"~17L,domain=="Energy expenditure"~18L),
   numeric_label=paste0(fmt(mean,job$metric)," [",fmt(ci_lower,job$metric),", ",fmt(ci_upper,job$metric),"]"))
 panels<-if(nrow(ds)>45)2L else 1L;per_panel<-ceiling(nrow(ds)/panels)
 ds$panel<-LETTERS[pmin(panels,ceiling(seq_len(nrow(ds))/per_panel))]
 W<-(if(panels==2&&job$metric!="F1")210 else 200)/25.4*72;H<-max(90/25.4*72,235/25.4*72+(per_panel-41)*11.3-32)
 left<-12;right<-W-12;gap<-16;pw<-(right-left-gap*(panels-1))/panels
 first_y<-H-49;step<-11.3;summary_y<-first_y-(per_panel-1)*step-24;axis_y<-summary_y-25
 ink<-"#111111"
 if(is_rmse){
  range_values<-c(0,ds$ci_lower,ds$ci_upper,pooled$pooled_std_lb,pooled$pooled_std_ub)
  axis_limits<-c(min(0,floor(min(range_values)*2)/2),ceiling(max(range_values)*2)/2)
  ticks<-sort(unique(c(if(axis_limits[1]<0)axis_limits[1],pretty(c(0,axis_limits[2]),n=4))))
  ticks<-ticks[ticks>=axis_limits[1]&ticks<=axis_limits[2]]
 }else if(is_ee){
  range_values<-c(0,1,ds$ci_lower,ds$ci_upper,pooled$pooled_std_lb,pooled$pooled_std_ub)
  axis_limits<-c(min(0,floor(min(range_values)*10)/10),max(1,ceiling(max(range_values)*10)/10))
  ticks<-sort(unique(c(if(axis_limits[1]<0)axis_limits[1],pretty(c(0,axis_limits[2]),n=4))))
  ticks<-ticks[ticks>=axis_limits[1]&ticks<=axis_limits[2]]
 }else{axis_limits<-c(0,1);ticks<-seq(0,1,.25)}
 txt<-function(x,y,label,size=7.2,bold=FALSE,adj=0,limits=c(left,right)){
  font<-if(bold)2 else 1;cex<-size/9
  sw<-strwidth(label,cex=cex,font=font,units="user");sh<-strheight(label,cex=cex,font=font,units="user")
  if(!(x-adj*sw>=limits[1]-.2&&x+(1-adj)*sw<=limits[2]+.2&&y-sh/2>=8&&y+sh/2<=H-5))stop(paste("Text exceeds bounds:",job$figure_id,as.character(label)))
  text(x,y,label,adj=c(adj,.5),cex=cex,font=font,col=ink)
 }
 draw<-function(){
  pal<-palette
  par(mar=rep(0,4),oma=rep(0,4),family="Arial",ps=9,xaxs="i",yaxs="i")
  plot.new();plot.window(c(0,W),c(0,H))
  for(j in seq_len(panels)){
   origin<-left+(j-1)*(pw+gap);edge<-origin+pw
   if(panels==2){label_right<-origin+if(job$metric=="F1")142 else 153;plot_left<-origin+if(job$metric=="F1")145 else 156;plot_right<-origin+if(job$metric=="F1")195 else 209;num_left<-origin+if(job$metric=="F1")199 else 213}
   else{label_right<-origin+225;plot_left<-origin+245;plot_right<-origin+385;num_left<-origin+403}
   xp<-function(v)plot_left+(v-axis_limits[1])/diff(axis_limits)*(plot_right-plot_left)
   if(panels==2)txt(origin,H-13,LETTERS[j],size=9,bold=TRUE)
   txt(origin,H-31,"Author Year - Model subgroup - ID",size=7.2,bold=TRUE,limits=c(origin,label_right))

   txt((plot_left+plot_right)/2,H-31,job$metric,size=7.5,bold=TRUE,adj=.5)
   txt(edge,H-31,"Estimate [95% CI]",size=7.2,bold=TRUE,adj=1,limits=c(num_left,edge))
   segments(origin,H-40,edge,H-40,lwd=.7,col=ink)
   segments(xp(pooled$pooled_std_est),axis_y,xp(pooled$pooled_std_est),first_y+4,lwd=.5,lty="44",col=ink)
   inds<-seq.int((j-1)*per_panel+1,min(j*per_panel,nrow(ds)))
   for(i in seq_along(inds)){
    r<-ds[inds[i],];y<-first_y-(i-1)*step;colour<-pal[as.character(r$domain)]
    prefix<-paste0(r$study," - ",r$method_label," -")
    txt(origin,y,prefix,size=7.2,limits=c(origin,label_right))
    id_x<-origin+strwidth(prefix,cex=7.2/9,units="user")+1
    txt(id_x,y,paste0(" ",r$val_id),size=5.2,limits=c(origin,label_right))
    segments(xp(r$ci_lower),y,xp(r$ci_upper),y,col=colour,lwd=.9)
    segments(xp(r$ci_lower),y-1.8,xp(r$ci_lower),y+1.8,col=colour,lwd=.8)
    segments(xp(r$ci_upper),y-1.8,xp(r$ci_upper),y+1.8,col=colour,lwd=.8)
    points(xp(r$mean),y,pch=r$marker,cex=.62,col=colour,lwd=.85)
    txt(edge,y,r$numeric_label,size=7.2,adj=1,limits=c(num_left,edge))
   }
   if(j==panels){
    segments(origin,summary_y+11,edge,summary_y+11,lwd=.7,col=ink)
    txt(origin,summary_y,paste0("Pooled ",job$metric),size=7.5,bold=TRUE,limits=c(origin,label_right))
    segments(xp(pooled$pooled_std_lb),summary_y,xp(pooled$pooled_std_ub),summary_y,lwd=1,col=ink)
    segments(xp(pooled$pooled_std_lb),summary_y-2.8,xp(pooled$pooled_std_lb),summary_y+2.8,lwd=.8,col=ink)
    segments(xp(pooled$pooled_std_ub),summary_y-2.8,xp(pooled$pooled_std_ub),summary_y+2.8,lwd=.8,col=ink)
    polygon(xp(c(pooled$pooled_std_lb,pooled$pooled_std_est,pooled$pooled_std_ub,pooled$pooled_std_est)),c(summary_y,summary_y+3.5,summary_y,summary_y-3.5),col=ink,border=ink)
    txt(edge,summary_y,if(is_rmse)sprintf("%.2f [%.2f, %.2f]",pooled$pooled_std_est,pooled$pooled_std_lb,pooled$pooled_std_ub)else sprintf("%.1f [%.1f, %.1f]",100*pooled$pooled_std_est,100*pooled$pooled_std_lb,100*pooled$pooled_std_ub),size=7.2,bold=TRUE,adj=1,limits=c(num_left,edge))
   }
   segments(plot_left,axis_y,plot_right,axis_y,lwd=.65,col=ink)
   for(t in ticks){segments(xp(t),axis_y,xp(t),axis_y-3,lwd=.65,col=ink);txt(xp(t),axis_y-11,if(is_rmse)sprintf("%.1f",t)else sprintf("%.0f",100*t),size=7,adj=.5)}
   txt((plot_left+plot_right)/2,axis_y-24,paste0(job$metric,if(is_rmse)" (METs)"else" (%)"),size=7.5,adj=.5)
  }
  # One boundary in the sorted validation sequence; no divider if all rows
  # are on the same side. A boundary between panels is drawn below its last row.
  above <- ds$sort_mean > pooled$pooled_std_est
  boundary <- which(head(above, -1) != tail(above, -1))
  stopifnot(length(boundary) <= 1L)
  if (length(boundary)) {
    panel <- ceiling(boundary / per_panel)
    local_row <- boundary - (panel - 1L) * per_panel
    separator_y <- first_y - (local_row - 0.5) * step
    separator_left <- left + (panel - 1L) * (pw + gap)
    segments(separator_left, separator_y, separator_left + pw, separator_y,
             lty="44", lwd=0.5, col=ink)
  }
  stats_y<-axis_y-43
  p_text<-if(pooled$QE_p<.001)" < .001."else paste0(" = ",sub("^0","",sprintf("%.3f",pooled$QE_p)),".")
  txt(left,stats_y,bquote(.(paste0(nrow(ds)," validations; ",pooled$clusters," studies. Residual "))*I^2*" = "*.(sprintf("%.1f%%",pooled$I2_total))*"; "*Q[E]*"("*.(pooled$QE_df)*") = "*.(sprintf("%.1f",pooled$QE))*", "*italic(p)*.(p_text)),size=7.5)
  legend_y<-stats_y-19
  present_domains<-names(palette)[names(palette)%in%as.character(ds$domain)]
  for(i in seq_along(present_domains)){
   k<-present_domains[i];x0<-left+3+(i-1)*165
   points(x0,legend_y,pch=c(Sleep=16,"Activity type"=15,"Activity intensity"=17,"Energy expenditure"=18)[k],cex=.7,col=pal[k])
   label<-if(k=="Sleep")"Sleep-wake"else k
   txt(x0+9,legend_y,label,size=7.5)
  }

 }
 folder<-file.path(out,job$plot_folder);stem<-job$filename_stem
 agg_png(file.path(folder,paste0(stem,"_600dpi.png")),width=W/72,height=H/72,units="in",res=600,background="white");draw();dev.off()
 alt<-if(is_rmse)sprintf("Forest plot of %d energy expenditure RMSE validations from %d studies, ordered by increasing error. Pooled RMSE %.2f METs (95%% CI %.2f to %.2f); residual I2 %.1f%%.",nrow(ds),pooled$clusters,pooled$pooled_std_est,pooled$pooled_std_lb,pooled$pooled_std_ub,pooled$I2_total)else sprintf("Forest plot of %d %s validations from %d studies. Pooled %s is %.1f%% (95%% CI %.1f to %.1f%%); residual I2 is %.1f%%. Domain marker shapes support greyscale reading.",nrow(ds),job$metric,pooled$clusters,job$metric,100*pooled$pooled_std_est,100*pooled$pooled_std_lb,100*pooled$pooled_std_ub,pooled$I2_total)
 stopifnot(nchar(alt)<=250);alts[[job$figure_id]]<-tibble(figure_id=job$figure_id,alt_text=alt)
 cat("Rendered ",job$figure_id,": ",nrow(ds)," rows, ",panels," panel(s), ",round(H/72*25.4,1)," mm high\n",sep="");flush.console()
}
write_csv(left_join(caption_text,bind_rows(alts),by="figure_id"),file.path(out,"Tables","Figure_Captions.csv"))
message("Publication artwork complete.")
