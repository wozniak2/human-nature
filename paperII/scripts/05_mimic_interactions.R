# ============================================================
# 05_mimic_interactions.R
# Extends the MIMIC model in 04: does the age effect or the
# gender effect on relational-entanglement itself differ by
# country, rather than just being present everywhere at the same
# strength? Adds country x age and country x gender interaction
# terms and compares against the main-effects-only model from 04
# via a likelihood-ratio test.
#
# Requires: 04_mimic_model.R already run (hnr_data.rds has
# country_*, age_num, gender_bin; hnr_mimic_fit.rds exists)
# ============================================================

suppressMessages(library(lavaan))

dat <- readRDS("hnr_data.rds")

q1_labels <- c(
  "presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
  "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person"
)
country_dummies <- paste0("country_", setdiff(levels(dat$country), "Canada"))

# --- build interaction terms (lavaan has no inline a:b syntax -- precompute products) ---
age_int <- character(0)
gender_int <- character(0)
for (cd in country_dummies) {
  age_name    <- paste0(cd, "_x_age")
  gender_name <- paste0(cd, "_x_gender")
  dat[[age_name]]    <- dat[[cd]] * dat$age_num
  dat[[gender_name]] <- dat[[cd]] * dat$gender_bin
  age_int    <- c(age_int, age_name)
  gender_int <- c(gender_int, gender_name)
}

build_model <- function(prefix) paste0('
  relational_f =~ ', paste(q1_labels, collapse = " + "), '

  relational_f ~ ', paste(country_dummies, collapse = " + "), ' + age_num + gender_bin +
                ', paste(paste0(prefix, c(age_int, gender_int)), collapse = " + "), '

  person_changes_place    ~ ', paste(country_dummies, collapse = " + "), '
  place_influences_person ~ ', paste(country_dummies, collapse = " + "), '
')
model_interact <- build_model("")

fit_interact <- sem(model_interact, data = dat, estimator = "ML")
# Nested comparison model: same observed variables, the 10 interaction paths
# fixed to 0. (Comparing against 04's fit directly would be invalid, because
# that model does not contain the interaction variables.)
fit_main <- sem(build_model("0*"), data = dat, estimator = "ML")

cat("========== Interaction model fit ==========\n")
print(round(fitmeasures(fit_interact, c("chisq","df","cfi","tli","rmsea","srmr")), 4))

cat("\n========== Omnibus test: do the 10 interaction terms improve fit at all? ==========\n")
print(lavTestLRT(fit_main, fit_interact))
cat("(fit_main = same model with the 10 interaction paths fixed to 0; fit_interact frees all 10\n")
cat(" country x age / country x gender terms at once -- a non-significant result\n")
cat(" here means none of the individual interactions below should be over-interpreted,\n")
cat(" even if one or two happen to clear p<.05 on their own.)\n")

cat("\n========== Country x Age interaction coefficients ==========\n")
pe <- parameterEstimates(fit_interact, standardized = TRUE)
age_rows <- pe[pe$lhs == "relational_f" & pe$rhs %in% age_int, ]
age_rows$country <- sub("^country_(.*)_x_age$", "\\1", age_rows$rhs)
print(age_rows[, c("country","est","se","z","pvalue","std.all")])
cat("(positive = the age effect is STRONGER in that country than in Canada; negative = weaker)\n")

cat("\n========== Country x Gender interaction coefficients ==========\n")
gender_rows <- pe[pe$lhs == "relational_f" & pe$rhs %in% gender_int, ]
gender_rows$country <- sub("^country_(.*)_x_gender$", "\\1", gender_rows$rhs)
print(gender_rows[, c("country","est","se","z","pvalue","std.all")])
cat("(positive = the male-vs-female gap is LARGER in that country than in Canada;\n")
cat(" negative = smaller, i.e. closer to no gap or reversed)\n")

cat("\n========== Main effects, re-estimated alongside the interactions ==========\n")
main_rows <- pe[pe$lhs == "relational_f" & pe$rhs %in% c("age_num","gender_bin", country_dummies), ]
print(main_rows[, c("rhs","est","se","z","pvalue","std.all")])
cat("(these are now the country/age/gender effects specifically for/at Canada's\n")
cat(" reference level once interactions are in the model -- compare with caution\n")
cat(" to 04's main-effects-only numbers, which averaged over all countries.)\n")

saveRDS(fit_interact, "hnr_mimic_interact_fit.rds")
cat("\nSaved: hnr_mimic_interact_fit.rds\n")
