# ============================================================
# 00_setup.R
# Installs (if missing) and reports the R packages the pipeline uses.
# Run once before 00_run_pipeline.R.
#
# Versions used to produce the results in PaperII_Outline_v4.docx:
#   R 4.4.1, readxl 1.5.0, lavaan 0.7-2, nnet 7.3-20, sirt 4.2-133
# Other versions should give the same results up to rounding; if
# numbers differ noticeably, install these versions first
# (e.g. remotes::install_version("lavaan", "0.7-2")).
# ============================================================

pkgs <- c("readxl", "lavaan", "nnet", "sirt", "ggplot2", "ggalluvial", "psych", "GPArotation", "semPlot")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

cat(R.version.string, "\n")
for (p in pkgs) cat(sprintf("%-8s %s\n", p, as.character(packageVersion(p))))
