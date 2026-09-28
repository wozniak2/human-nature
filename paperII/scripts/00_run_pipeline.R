# ============================================================
# Human-Nature Relationship (HNR) cross-country analysis
# Master pipeline: runs every analysis script in order and writes a
# full log (pipeline_log.txt) and the R session info (sessionInfo.txt).
#
# Before the first run: source("00_setup.R") to install packages.
#
# Working directory must be this "paperII/scripts" folder, e.g.
#   setwd("<path to>/paperII/scripts")
# (the block below does this automatically when run from RStudio).
# Raw xlsx files are read from ../data (see DATA_DIR in 01_load_and_prepare.R).
#
# From a terminal:  Rscript 00_run_pipeline.R
# (Always run the file itself; Rscript -e "..." crashes on this machine
#  when reading xlsx.)
# ============================================================

if (interactive()) {
  ok <- tryCatch({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) message("Could not auto-detect this script's folder -- set your working directory to paperII/scripts/ manually before running.")
}

# start from a clean slate so no result depends on an outdated file
unlink(list.files(pattern = "^hnr_.*\\.rds$"))

log_con <- file("pipeline_log.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

steps <- c(
  "01_load_and_prepare.R",       # load 6 country files, QC flags, response-style indices
  "02_cfa_invariance.R",         # belief scale: CFA + configural/metric/scalar/partial invariance
  "04_mimic_model.R",            # belief scale: MIMIC with country, age, gender
  "05_mimic_interactions.R",     # belief scale: country x age / country x gender
  "06_qc_robustness.R",          # belief scale: re-estimate excluding flagged respondents
  "07_alignment_check.R",        # belief scale: alignment method (approximate invariance)
  "08_mastery_paradox.R",        # CORE: perceived (now) vs ideal (should) human role
  "09_response_style_validity.R" # belief scale: construct validity + response-style controls
)
# 03_typology_dif.R is superseded (see its header) and is not run.

for (s in steps) {
  cat("\n\n############################################################\n")
  cat("## ", s, "\n")
  cat("############################################################\n")
  source(s, echo = FALSE)
}

cat("\n\n================ PIPELINE COMPLETE ================\n")
cat("Outputs:", paste(list.files(pattern = "^hnr_.*\\.rds$"), collapse = ", "), "\n")

sink(type = "message"); sink()
close(log_con)
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
