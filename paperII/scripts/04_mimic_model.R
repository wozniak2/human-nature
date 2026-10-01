# ============================================================
# 04_mimic_model.R
# Single-step MIMIC model: the relational-entanglement latent
# factor regressed directly on country + age + gender, in one
# lavaan model, instead of the two-step "extract factor scores
# then regress separately" approach used in 02/03.
#
# Country's partial-scalar correction (from 02) is preserved here
# as direct effects: the two items found non-invariant across
# countries (person_changes_place, place_influences_person) are
# regressed directly on the country dummies, on top of the usual
# path through the latent factor -- this is the MIMIC-model
# equivalent of freeing those two intercepts in the multi-group
# approach, and is the standard way DIF is represented in a MIMIC
# framework.
#
# Requires: 01_load_and_prepare.R already run (hnr_data.rds present)
# ============================================================

suppressMessages(library(lavaan))

dat <- readRDS("hnr_data.rds")

q1_labels <- c(
  "presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
  "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person"
)

# --- country dummies (Canada = reference, matches the multi-group model in 02) ---
for (lv in setdiff(levels(dat$country), "Canada")) {
  dat[[paste0("country_", lv)]] <- as.numeric(dat$country == lv)
}
country_dummies <- paste0("country_", setdiff(levels(dat$country), "Canada"))

# --- gender: collapse to binary Woman(0)/Man(1); trans/non-binary/prefer-not-say -> NA
#     (Netherlands only offered 2 categories to begin with, so this is the common
#     denominator across all 6 countries -- see caveats in 01_load_and_prepare.R) ---
dat$gender_bin <- ifelse(dat$gender_raw == 1, 0, ifelse(dat$gender_raw == 2, 1, NA))

# --- age: linear code 1-6 (18-24 ... 65+); a few respondents in Poland/Spain/Sweden
#     picked "prefer not to answer" (code 7) -- treated as NA, not a 7th age band ---
dat$age_num <- ifelse(dat$age_raw %in% 1:6, dat$age_raw, NA)

# --- education: three levels harmonised across the six questionnaires (Q19) ---
# The answer options differ by version (checked against the questionnaire text):
#   Canada, Netherlands, Sweden (English, 9 codes): 1 none, 2 primary, 3 lower secondary,
#     4 upper secondary, 5 post-secondary non-tertiary (vocational), 6 bachelor's,
#     7 master's, 8 doctoral, 9 prefer not to answer
#   Poland (8 codes): 1 none, 2 primary, 3 vocational, 4 secondary (liceum/technikum),
#     5 bachelor's, 6 master's, 7 doctoral, 8 prefer not to answer
#   Spain, Panama (Spanish, 7 codes): 1 none, 2 primary, 3 secondary/pre-media,
#     4 media (bachillerato), 5-6 higher, 7 prefer not to answer
# Harmonised to three ISCED 2011 levels: 1 = primary or less (ISCED 0-1), 2 = secondary
# (lower/upper secondary, vocational, post-secondary non-tertiary; ISCED 2-4),
# 3 = tertiary (ISCED 5-8). "Prefer not to answer" (28
# respondents) gets its own flag, edu_na, so that nobody drops out of the models.
# Code-to-label order is inferred from the questionnaire's option order and confirmed
# by the counts (the largest group is upper secondary everywhere).
edu_level <- function(country, raw) {
  cc <- as.character(country); out <- rep(NA_integer_, length(raw))
  wide <- cc %in% c("Canada", "Netherlands", "Sweden"); pl <- cc == "Poland"; es <- cc %in% c("Spain", "Panama")
  out[wide & raw %in% 1:2] <- 1L; out[wide & raw %in% 3:5] <- 2L; out[wide & raw %in% 6:8] <- 3L
  out[pl & raw %in% 1:2] <- 1L;   out[pl & raw %in% 3:4] <- 2L;   out[pl & raw %in% 5:7] <- 3L
  out[es & raw %in% 1:2] <- 1L;   out[es & raw %in% 3:4] <- 2L;   out[es & raw %in% 5:6] <- 3L
  out
}
dat$edu_level   <- edu_level(dat$country, dat$education_raw)
dat$edu_primary <- as.integer(!is.na(dat$edu_level) & dat$edu_level == 1)
dat$edu_higher  <- as.integer(!is.na(dat$edu_level) & dat$edu_level == 3)
dat$edu_na      <- as.integer(is.na(dat$edu_level))          # reference category: secondary
cat("Education (1 primary or less, 2 secondary, 3 tertiary; NA = prefer not to answer), by country:\n")
print(table(dat$country, dat$edu_level, useNA = "ifany"))

n_before <- nrow(dat)
n_complete <- sum(complete.cases(dat[, c(q1_labels, country_dummies, "gender_bin", "age_num")]))
cat("Respondents:", n_before, "| complete cases for MIMIC model:", n_complete,
    "(", n_before - n_complete, "dropped, mostly non-binary/trans/prefer-not-say gender",
    "and prefer-not-say age)\n\n")

model_mimic <- paste0('
  relational_f =~ ', paste(q1_labels, collapse = " + "), '

  # structural part -- country, age, gender predicting the latent factor directly
  relational_f ~ ', paste(country_dummies, collapse = " + "), ' + age_num + gender_bin

  # DIF / partial-invariance direct effects (see 02_cfa_invariance.R score test):
  # these 2 items get a country-specific boost/penalty beyond what the latent
  # factor explains, mirroring the group.partial freed intercepts in 02
  person_changes_place    ~ ', paste(country_dummies, collapse = " + "), '
  place_influences_person ~ ', paste(country_dummies, collapse = " + "), '
')

fit_mimic <- sem(model_mimic, data = dat, estimator = "ML")

cat("========== MIMIC model fit ==========\n")
print(round(fitmeasures(fit_mimic, c("chisq","df","cfi","tli","rmsea","srmr")), 4))

cat("\n========== Structural paths: what predicts the relational latent factor ==========\n")
pe <- parameterEstimates(fit_mimic, standardized = TRUE)
struct <- pe[pe$lhs == "relational_f" & pe$op == "~", ]
print(struct[, c("rhs","est","se","z","pvalue","std.all")])

cat("\n========== DIF direct effects (person_changes_place, place_influences_person) ==========\n")
dif <- pe[pe$lhs %in% c("person_changes_place","place_influences_person") & pe$op == "~", ]
print(dif[, c("lhs","rhs","est","se","z","pvalue")])

cat("\n========== Compare: MIMIC country coefficients vs. the earlier two-step latent means ==========\n")
cat("(two-step means are from the multi-group partial-scalar model in 02_cfa_invariance.R,\n")
cat(" Canada fixed at 0; MIMIC coefficients below are the direct effect of each country\n")
cat(" dummy on the latent factor, net of age and gender -- not identical quantities, but\n")
cat(" a similar magnitude/pattern here means the country effect is NOT just age/gender\n")
cat(" composition in disguise.)\n\n")
# latent means from the partial-scalar multi-group model saved by 02 (Canada = 0)
pe_mg <- parameterEstimates(readRDS("hnr_cfa_fits.rds")$partial)
lm_mg <- pe_mg[pe_mg$op == "~1" & pe_mg$lhs == "relational_f", ]
twostep <- data.frame(country = levels(dat$country)[lm_mg$group], twostep_est = round(lm_mg$est, 3))
twostep <- twostep[twostep$country != "Canada", ]
mimic_country <- struct[grepl("^country_", struct$rhs), c("rhs","est","pvalue")]
mimic_country$country <- sub("^country_", "", mimic_country$rhs)
cmp <- merge(twostep, mimic_country[, c("country","est","pvalue")], by = "country")
names(cmp) <- c("country","twostep_latent_mean","mimic_est_net_of_age_gender","mimic_pvalue")
print(cmp[order(cmp$twostep_latent_mean), ])

cat("\n========== Age and gender effects on their own ==========\n")
demo_rows <- struct[struct$rhs %in% c("age_num","gender_bin"), ]
print(demo_rows[, c("rhs","est","se","z","pvalue","std.all")])
cat("gender_bin: 0 = Woman, 1 = Man (positive estimate = higher relational score for men)\n")
cat("age_num: linear per age-band (1 = 18-24 ... 6 = 65+)\n")

saveRDS(fit_mimic, "hnr_mimic_fit.rds")
saveRDS(dat, "hnr_data.rds")  # now also carries country_*, gender_bin, age_num for 05
cat("\nSaved: hnr_mimic_fit.rds, hnr_data.rds (updated with MIMIC-ready columns)\n")
