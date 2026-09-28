# ============================================================
# 09_response_style_validity.R
# Secondary measure checks for the six-item belief scale (Q1,
# "reciprocal person-place influence"):
#   (a) construct validity against other batteries in the survey,
#       raw and after partialling out response style;
#   (b) country effects with response-style controls, in the MIMIC
#       model from 04 (ARS/MRS/ERS from 10 opposing-content items);
#   (c) one factor vs 'common-sense causal' {1,2,5} / 'communicative' {3,4,6}.
#
# Requires: 01 and 04 already run
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds")
q1_labels <- c("presence_influences_place","place_influences_person","person_shares_stories_w_place",
               "place_tells_story_to_person","person_changes_place","place_leaves_traces_in_person")

cat("========== (a) Construct validity (Spearman rho with scale mean) ==========\n")
vars <- c(dial_nature      = "Dialogue with natural entities possible (Q2, 12 items)",
          q12_communicates = "Favourite place communicates with me (Q12)",
          q12_teaches      = "Favourite place teaches me (Q12)",
          q11_dialogue     = "Changes in place = dialogue (Q11)",
          q10_message      = "Flood = message from autonomous nature (Q10)",
          control_mean     = "Coexist/subject vs exploit (Q3, 12 items)",
          q12_relax        = "I relax there (Q12)",
          dial_human       = "Dialogue with another human possible (Q2)",
          q12_comfort      = "Wants comfort/facilities (Q12)",
          q12_cheap        = "Wants it cheap (Q12)")
res_rel <- resid(lm(relational ~ ARS + MRS + ERS, dat, na.action = na.exclude))
tab <- data.frame(item = vars, rho_raw = NA_real_, rho_style_partialled = NA_real_)
for (v in names(vars)) {
  tab[v, "rho_raw"] <- cor(dat$relational, dat[[v]], use = "pairwise.complete.obs", method = "spearman")
  rv <- resid(lm(dat[[v]] ~ ARS + MRS + ERS, dat, na.action = na.exclude))
  tab[v, "rho_style_partialled"] <- cor(res_rel, rv, use = "pairwise.complete.obs", method = "spearman")
}
tab[, 2:3] <- round(tab[, 2:3], 3); print(tab, row.names = FALSE)
cat("\nResponse style by country:\n")
print(round(aggregate(cbind(ARS, MRS, ERS) ~ country, dat, mean)[, -1], 3), row.names = FALSE)
print(levels(dat$country))
cat("Correlation of scale with ARS / MRS / ERS:", round(cor(dat$relational, dat[, c("ARS","MRS","ERS")]), 3), "\n")
cat("R2 relational ~ country:", round(summary(lm(relational ~ country, dat))$r.squared, 3),
    "| ~ country + style:", round(summary(lm(relational ~ country + ARS + MRS + ERS, dat))$r.squared, 3), "\n")

cat("\n========== (b) MIMIC with response-style covariates ==========\n")
country_dummies <- paste0("country_", setdiff(levels(dat$country), "Canada"))
mimic <- function(extra) paste0('
  relational_f =~ ', paste(q1_labels, collapse = " + "), '
  relational_f ~ ', paste(country_dummies, collapse = " + "), ' + age_num + gender_bin', extra, '
  person_changes_place    ~ ', paste(country_dummies, collapse = " + "), '
  place_influences_person ~ ', paste(country_dummies, collapse = " + "), '
')
fit_base  <- sem(mimic(""), data = dat, estimator = "ML")
fit_style <- sem(mimic(" + ARS + MRS + ERS"), data = dat, estimator = "ML")
get <- function(f) { pe <- parameterEstimates(f, standardized = TRUE); pe[pe$lhs == "relational_f" & pe$op == "~", c("rhs","est","se","pvalue","std.all")] }
b <- get(fit_base); s <- get(fit_style)
cmp <- merge(b[, c("rhs","est","pvalue")], s[, c("rhs","est","pvalue","std.all")], by = "rhs", all = TRUE,
             suffixes = c("_no_style", "_with_style"))
cmp[, -1] <- round(cmp[, -1], 3); print(cmp, row.names = FALSE)
print(round(fitmeasures(fit_style, c("chisq","df","cfi","tli","rmsea","srmr")), 3))

cat("\n========== (c) One factor vs causal {1,2,5} / communicative {3,4,6} ==========\n")
f1 <- cfa(paste0("rel =~ ", paste(q1_labels, collapse = " + ")), dat)
f2 <- cfa('causal =~ presence_influences_place + place_influences_person + person_changes_place
           commun =~ person_shares_stories_w_place + place_tells_story_to_person + place_leaves_traces_in_person', dat)
print(rbind(one_factor = round(fitmeasures(f1, c("chisq","df","cfi","rmsea","srmr","bic")), 3),
            two_factor = round(fitmeasures(f2, c("chisq","df","cfi","rmsea","srmr","bic")), 3)))
cat("factor correlation:", round(lavInspect(f2, "cor.lv")[1, 2], 3), "\n")
cat("% agree (4-5) per item:", round(100 * colMeans(dat[, q1_labels] >= 4), 1), "\n")

saveRDS(list(validity = tab, mimic_style = fit_style, cfa_2f = f2), "hnr_style_validity.rds")
cat("\nSaved: hnr_style_validity.rds\n")
