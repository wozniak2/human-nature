# ============================================================
# 06_qc_robustness.R
# Closes the "run the response-quality screen on all 6 countries"
# item from PaperII_Outline.docx's Next Steps: re-fits the partial-
# scalar multi-group CFA and the MIMIC model on the subsample that
# excludes flagged (speeding/straight-lining) respondents, and
# compares the key numbers against the full-sample versions from
# 02 and 04.
#
# Requires: 01-04 already run (hnr_data.rds has flag_lowqual,
# country_*, gender_bin, age_num)
# ============================================================

suppressMessages(library(lavaan))

dat <- readRDS("hnr_data.rds")
cat("Excluding", sum(dat$flag_lowqual), "of", nrow(dat), "flagged respondents\n")
dat_clean <- subset(dat, !flag_lowqual)
cat("Remaining n:", nrow(dat_clean), "\n\n")

q1_labels <- c(
  "presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
  "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person"
)
model <- paste0("relational_f =~ ", paste(q1_labels, collapse = " + "))

cat("========== Partial-scalar invariance, excluding flagged respondents ==========\n")
free_items <- c("person_changes_place~1", "place_influences_person~1")
fit_partial_clean <- cfa(model, data = dat_clean, group = "country", estimator = "ML",
                          group.equal = c("loadings","intercepts"), group.partial = free_items)
fit_metric_clean  <- cfa(model, data = dat_clean, group = "country", estimator = "ML", group.equal = "loadings")
print(round(fitmeasures(fit_partial_clean, c("chisq","df","cfi","tli","rmsea","srmr")), 4))
cat("Delta CFI (partial-metric), excl. flagged:",
    round(fitmeasures(fit_partial_clean,"cfi") - fitmeasures(fit_metric_clean,"cfi"), 4),
    "\n")
full <- readRDS("hnr_cfa_fits.rds")
cat("(full sample:", round(fitmeasures(full$partial, "cfi") - fitmeasures(full$metric, "cfi"), 4), ")\n\n")

cat("--- Latent means by country, excluding flagged (Canada = 0 reference) ---\n")
pe <- parameterEstimates(fit_partial_clean)
lmeans <- pe[pe$op == "~1" & pe$lhs == "relational_f", ]
lmeans$country <- levels(dat_clean$country)[lmeans$group]
pf <- parameterEstimates(full$partial)
lf <- pf[pf$op == "~1" & pf$lhs == "relational_f", ]
lmeans$full_sample_est <- round(lf$est[match(lmeans$group, lf$group)], 3)
print(lmeans[, c("country","est","se","pvalue","full_sample_est")])
cat("\n")

cat("========== MIMIC model, excluding flagged respondents ==========\n")
country_dummies <- paste0("country_", setdiff(levels(dat_clean$country), "Canada"))
model_mimic <- paste0('
  relational_f =~ ', paste(q1_labels, collapse = " + "), '
  relational_f ~ ', paste(country_dummies, collapse = " + "), ' + age_num + gender_bin
  person_changes_place    ~ ', paste(country_dummies, collapse = " + "), '
  place_influences_person ~ ', paste(country_dummies, collapse = " + "), '
')
fit_mimic_clean <- sem(model_mimic, data = dat_clean, estimator = "ML")
pe2 <- parameterEstimates(fit_mimic_clean, standardized = TRUE)
struct <- pe2[pe2$lhs == "relational_f" & pe2$op == "~", ]
pm <- parameterEstimates(readRDS("hnr_mimic_fit.rds"))
pm <- pm[pm$lhs == "relational_f" & pm$op == "~", ]
struct$full_sample_est <- round(pm$est[match(struct$rhs, pm$rhs)], 3)
print(struct[, c("rhs","est","se","pvalue","std.all","full_sample_est")])

saveRDS(list(partial = fit_partial_clean, mimic = fit_mimic_clean), "hnr_qc_robustness_fits.rds")
cat("\nSaved: hnr_qc_robustness_fits.rds\n")
