# ============================================================
# Human-Nature Relationship (HNR) cross-country analysis
# Master pipeline: runs every analysis script in order and writes a
# full log (pipeline_log.txt) and the R session info (sessionInfo.txt).
#
# Before the first run: source("00_setup.R") to install packages.
#
# RStudio: open this file and click Source (or Ctrl+Shift+S).
# Terminal: Rscript 00_run_pipeline.R
# (Always run the file itself; Rscript -e "..." crashes on this machine
#  when reading xlsx.)
#
# The working directory must be this "paperII/scripts" folder; the block
# below finds it automatically and stops with an explicit message if it
# cannot, instead of failing later for an unrelated-looking reason.
# ============================================================

# --- 1. locate this script's folder -------------------------------------
script_dir <- NA_character_
cmd <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(cmd)) script_dir <- dirname(sub("^--file=", "", cmd[1]))   # Rscript
if (is.na(script_dir) && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  p <- try(rstudioapi::getSourceEditorContext()$path, silent = TRUE)  # the file being sourced
  if (!inherits(p, "try-error") && length(p) == 1 && nzchar(p)) script_dir <- dirname(p)
}
if (!is.na(script_dir) && nzchar(script_dir) && script_dir != ".") setwd(script_dir)

# --- 2. refuse to continue from the wrong folder ------------------------
if (!file.exists("01_load_and_prepare.R")) {
  stop("Wrong working directory: ", getwd(), "\n",
       "  This script must run from the paperII/scripts folder.\n",
       "  Fix it with setwd(\"<path to>/paperII/scripts\") and run again.\n",
       "  (In RStudio: Session > Set Working Directory > To Source File Location.)",
       call. = FALSE)
}
data_dir <- file.path("..", "data")
n_xlsx <- length(list.files(data_dir, pattern = "[.]xlsx$"))
if (n_xlsx < 6) {
  stop("Expected 6 .xlsx survey files in ", normalizePath(data_dir, mustWork = FALSE),
       " but found ", n_xlsx, ".\n",
       "  The raw data are not in the public repository; copy them into paperII/data/.",
       call. = FALSE)
}
for (p in c("readxl", "lavaan", "nnet", "sirt")) {
  if (!requireNamespace(p, quietly = TRUE))
    stop("Package '", p, "' is not installed for this R (", R.version.string, ").\n",
         "  Run source(\"00_setup.R\") first -- and check RStudio is using the same\n",
         "  R version you installed the packages into (Tools > Global Options > General).",
         call. = FALSE)
}

# --- 3. open the log, and make sure output always returns to the console -
log_con <- tryCatch(file("pipeline_log.txt", open = "wt"), error = function(e)
  stop("Cannot write pipeline_log.txt in ", getwd(), ": ", conditionMessage(e), "\n",
       "  The file may be open in another program, or locked by a syncing folder\n",
       "  (OneDrive, Google Drive, Dropbox). Close it or pause syncing, then run again.", call. = FALSE))
sink(log_con, split = TRUE)
# without this, an error inside any step would leave the console silently
# redirected into the log file and RStudio would look broken
on.exit({
  try(sink(type = "message"), silent = TRUE)
  try(sink(), silent = TRUE)
  try(close(log_con), silent = TRUE)
}, add = TRUE)

# start from a clean slate so no result depends on an outdated file
unlink(list.files(pattern = "^hnr_.*[.]rds$"))


steps <- c(
  "01_load_and_prepare.R",       # load 6 country files, QC flags, response-style indices
  "02_cfa_invariance.R",         # belief scale: CFA + configural/metric/scalar/partial invariance
  "04_mimic_model.R",            # belief scale: MIMIC with country, age, gender
  "05_mimic_interactions.R",     # belief scale: country x age / country x gender
  "06_qc_robustness.R",          # belief scale: re-estimate excluding flagged respondents
  "07_alignment_check.R",        # belief scale: alignment method (approximate invariance)
  "08_mastery_paradox.R",        # CORE: perceived (now) vs ideal (should) human role
  "09_response_style_validity.R", # belief scale: construct validity + response-style controls
  "13_reasons_efa.R",          # reasons for staying: EFA in one half, CFA in the other
  "14_motives_and_roles.R",    # motives: who holds them, and do they predict the role
  "26_ordered_sem.R",          # ordered role and extremity: one joint SEM
  "27_ordered_mediation.R",    # do the motives act on the ordered wanted role through place agency?
  "28_story_model.R",          # the whole story as one model, for the path diagram (fig20)
  "25_motive_measurement.R",   # item-level effects, alternative scorings, split-half noise, serviced, invariance
  "24_outcome_structure.R",    # is the role one ordered scale? position versus extremity
  "23_robustness.R",           # the attacks a critical referee would make, and the tests that answer them
  "22_geography.R",            # can geography moderate, or account for, anything? (country, residence)
  "21_mediation_sem.R",        # do the motives act on the wanted role through place agency? (slow: about 4 minutes)
  "20_opposing_roles_sem.R",    # the same model for the roles that oppose Master
  "19_now_vs_should_sem.R",    # the role people see versus the role they want, one SEM, path by path
  "18_place_relationship.R",   # relationship to one own favourite place: scale, personal vs societal, effect on the model
  "17_scenarios_and_roles.R",  # two scenarios that state positions on the same continuum
  "16_motive_invariance.R",    # can the motives be compared across countries? (they cannot)
  "15_structural_model.R",     # one model in which every latent measure competes
  "12_agency_scale.R",         # agency of non-human beings: scale, invariance, latent change
  "11_square_table_models.R", # loglinear models for the 6x6 table (symmetry, quasi-symmetry)
  "10_figures.R"                 # figures for the manuscript (writes figures/)
)
# 03_typology_dif.R is superseded (see its header) and is not run.

for (s in steps) {
  cat("\n\n############################################################\n")
  cat("## ", s, "\n")
  cat("############################################################\n")
  tryCatch(source(s, echo = FALSE), error = function(e) {
    try(sink(type = "message"), silent = TRUE); try(sink(), silent = TRUE)
    stop("Step ", s, " failed: ", conditionMessage(e),
         "  (output up to this point is in pipeline_log.txt)", call. = FALSE)
  })
}

cat("\n\n================ PIPELINE COMPLETE ================\n")
cat("Outputs:", paste(list.files(pattern = "^hnr_.*\\.rds$"), collapse = ", "), "\n")

writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
# The sinks and the log connection are closed by the on.exit handler set
# above. Closing them again here would re-flush buffered output and write
# part of the log twice.
