# ============================================================
# 02_cfa_invariance.R
# Multi-group CFA on the Q1 relational-entanglement scale:
# configural -> metric -> scalar -> partial-scalar invariance
# across the 6 countries. Also extracts per-respondent factor
# scores (from the partial-scalar model) for use in 03.
#
# Requires: 01_load_and_prepare.R already run (hnr_data.rds present)
# ============================================================

if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan", repos = "https://cloud.r-project.org")
suppressMessages(library(lavaan))

dat <- readRDS("hnr_data.rds")

q1_labels <- c(
  "presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
  "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person"
)
model <- paste0("relational_f =~ ", paste(q1_labels, collapse = " + "))
fitstats <- c("chisq","df","cfi","tli","rmsea","srmr")

cat("========== Configural invariance (no cross-group constraints) ==========\n")
fit_config <- cfa(model, data = dat, group = "country", estimator = "ML")
print(round(fitmeasures(fit_config, fitstats), 4))

cat("\n========== Metric invariance (+ equal loadings) ==========\n")
fit_metric <- cfa(model, data = dat, group = "country", estimator = "ML", group.equal = "loadings")
print(round(fitmeasures(fit_metric, fitstats), 4))

cat("\n========== Scalar invariance (+ equal intercepts) ==========\n")
fit_scalar <- cfa(model, data = dat, group = "country", estimator = "ML", group.equal = c("loadings","intercepts"))
print(round(fitmeasures(fit_scalar, fitstats), 4))

comp <- rbind(
  configural = fitmeasures(fit_config, fitstats),
  metric     = fitmeasures(fit_metric, fitstats),
  scalar     = fitmeasures(fit_scalar, fitstats)
)
cat("\n--- Model comparison ---\n")
print(round(comp, 4))
cat("Delta CFI (metric-configural):", round(comp["metric","cfi"]  - comp["configural","cfi"], 4), "\n")
cat("Delta CFI (scalar-metric):    ", round(comp["scalar","cfi"] - comp["metric","cfi"], 4), "\n")
cat("(Chen 2007 rule of thumb: Delta CFI <= -.010, backed by Delta RMSEA >= .015,\n")
cat(" flags meaningful non-invariance -- more trustworthy than the chi-square\n")
cat(" difference test alone once n is in the thousands.)\n")

cat("\n--- Chi-square difference tests ---\n")
print(lavTestLRT(fit_config, fit_metric, fit_scalar))

cat("\n--- Score test for non-invariant intercepts (top 10), if scalar looks strained ---\n")
mi <- lavTestScore(fit_scalar, epc = TRUE)
print(head(mi$uni[order(-mi$uni$X2), ], 10))
cat("\nThis pipeline was validated (2026-08) with 2 of 6 intercepts non-invariant:\n")
cat("  person_changes_place, place_influences_person\n")
cat("If your data changes (more responses, more countries), re-derive which items\n")
cat("to free from the score test above rather than assuming these same 2 still apply.\n")

cat("\n========== Partial scalar invariance (free the non-invariant intercepts) ==========\n")
free_items <- c("person_changes_place~1", "place_influences_person~1")
fit_partial <- cfa(model, data = dat, group = "country", estimator = "ML",
                    group.equal = c("loadings","intercepts"),
                    group.partial = free_items)
print(round(fitmeasures(fit_partial, fitstats), 4))
cat("Delta CFI (partial-metric):", round(fitmeasures(fit_partial,"cfi") - comp["metric","cfi"], 4),
    "-- should be > -.010 for partial invariance to be judged acceptable\n")
print(lavTestLRT(fit_metric, fit_partial))

cat("\n========== Latent means by country (Canada fixed at 0, reference) ==========\n")
pe <- parameterEstimates(fit_partial)
lmeans <- pe[pe$op == "~1" & pe$lhs == "relational_f", ]
lmeans$country <- levels(dat$country)[lmeans$group]
print(lmeans[, c("country","est","se","z","pvalue")])

# --- Extract per-respondent factor scores for the DIF analysis in 03 ---
fs <- lavPredict(fit_partial)  # list of matrices, one per group, in factor-level order
country_levels <- levels(dat$country)
dat$q1_factor <- NA_real_
for (i in seq_along(fs)) {
  idx <- which(dat$country == country_levels[i])
  dat$q1_factor[idx] <- fs[[i]][, "relational_f"]
}

saveRDS(dat, "hnr_data.rds")  # now includes q1_factor
saveRDS(list(configural = fit_config, metric = fit_metric, scalar = fit_scalar, partial = fit_partial),
        "hnr_cfa_fits.rds")
cat("\nSaved: hnr_data.rds (updated with q1_factor column), hnr_cfa_fits.rds\n")
