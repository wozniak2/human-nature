# ============================================================
# 03_typology_dif.R
# SUPERSEDED (2026-09-28): this treats Q4 ("how is it NOW") as personal
# endorsement of a role, but Q4 asks for the PERCEIVED current human
# role; the personal stance is Q5 ("how should it be"). The anchor
# (q1_factor) is also unrelated to choosing "Master" (p = .19), so it
# cannot separate item bias from real differences. Kept for the
# record only -- see 08_mastery_paradox.R for the current analysis.
#
# DIF-style check for the Q4 "now" role typology (Master/Manager/
# User/Guardian/Partner/Object) across countries, using the bias-
# corrected Q1 latent factor score (from 02) as the matching /
# anchor variable -- a logistic-regression DIF approach (Zumbo,
# 1999; Swaminathan & Rogers, 1990), extended here to a nominal
# typology outcome since there is no continuous multi-item scale
# to run a CFA-style intercept test on.
#
# Logic: if the typology differs across countries only because the
# underlying relational attitude (q1_factor) differs across
# countries, then once we control for q1_factor, country should
# stop predicting typology choice ("uniform DIF" absent). If
# country still predicts typology after controlling for q1_factor,
# and/or the country x q1_factor interaction is significant, that
# is evidence the typology item itself is not measured equivalently
# across countries (e.g. translation/connotation differences in the
# narrative labels), independent of the underlying attitude level.
#
# Requires: 02_cfa_invariance.R already run (hnr_data.rds has q1_factor)
# ============================================================

if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet", repos = "https://cloud.r-project.org")
suppressMessages(library(nnet))

dat <- readRDS("hnr_data.rds")
stopifnot("q1_factor" %in% names(dat))  # run 02_cfa_invariance.R first

# ---- Focused binary test: "Master" endorsement (the category showing
#      the clearest raw country gradient: 23% Canada vs 46% Spain) ----
dat$master <- factor(ifelse(dat$typ_now == 1, "Master", "NotMaster"), levels = c("NotMaster","Master"))

m_null <- glm(master ~ 1,                     data = dat, family = binomial)
m0     <- glm(master ~ q1_factor,             data = dat, family = binomial)
m1     <- glm(master ~ q1_factor + country,   data = dat, family = binomial)
m2     <- glm(master ~ q1_factor * country,   data = dat, family = binomial)

cat("========== Step 0: does the anchor (q1_factor) predict 'Master' at all? ==========\n")
print(anova(m_null, m0, test = "Chisq"))

cat("\n========== Step 1 (uniform DIF): country effect after controlling for q1_factor ==========\n")
print(anova(m0, m1, test = "Chisq"))
cat("Significant here means country predicts 'Master' endorsement beyond what the\n")
cat("underlying relational attitude explains -- i.e. uniform DIF is present.\n")

cat("\n========== Step 2 (non-uniform DIF): does country change the attitude -> Master relationship? ==========\n")
print(anova(m1, m2, test = "Chisq"))
cat("Significant here means the SAME level of underlying attitude translates into\n")
cat("different odds of choosing 'Master' depending on country -- e.g. the narrative\n")
cat("label may carry different connotations across languages/cultures, not just a\n")
cat("uniform shift.\n")

cat("\n--- Coefficients, model m1 (uniform-DIF model, Canada = reference) ---\n")
print(round(summary(m1)$coefficients, 4))

# ---- Supplementary: full 6-category multinomial version ----
cat("\n\n========== Supplementary: full multinomial typology ~ q1_factor + country ==========\n")
dat$typ_now_f <- relevel(dat$typ_now_f, ref = "Master")
mm0 <- multinom(typ_now_f ~ q1_factor,           data = dat, trace = FALSE)
mm1 <- multinom(typ_now_f ~ q1_factor + country, data = dat, trace = FALSE)
cat("Likelihood-ratio test, country main effect across all 6 typology categories:\n")
print(anova(mm0, mm1))

results <- list(binary_m0 = m0, binary_m1 = m1, binary_m2 = m2, multinom_m0 = mm0, multinom_m1 = mm1)
saveRDS(results, "hnr_typology_dif_results.rds")
cat("\nSaved: hnr_typology_dif_results.rds\n")
