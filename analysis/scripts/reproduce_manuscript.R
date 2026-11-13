# Run from the repository root. Analysis data pass between steps in memory.
results <- new.env(parent=globalenv())
for (script in c("01_classifiers_meta.R","02_energy_expenditure.R","03_study_characteristics_figures.R",
                 "04_quadas2_figure.R","05_model_context.R","06_rmse.R","07_export_app.R","08_publication_figures.R")) {
 message("Running ",script)
 step <- new.env(parent=results)
 sys.source(file.path("analysis","scripts",script),envir=step)
 if(script=="01_classifiers_meta.R") results$classifier_results <- step$analysis_result
 if(script=="02_energy_expenditure.R") results$energy_results <- step$analysis_result
}
message("Manuscript analyses and PNG figures exported. Run node app/build-data.js to rebuild the app bundle.")
