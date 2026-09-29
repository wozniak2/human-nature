# ============================================================
# 28_story_model.R
# The whole story as one structural model, fitted so that it can be drawn:
#
#   restorative and dialogic experience (the motives)
#        -> place agency (the construal)          [mediation]
#        -> position of the wanted role, and its extremity   [two outcomes]
#   plus the direct paths from the motives to both outcomes.
#
# Differences from scripts 26 and 27, which carry the analysis:
#   - both motives are in the model together. In the ordered specification this
#     is stable (script 27 fitted them one at a time only because the binary
#     specification was not); the largest standardised path is printed below
#     and the model is not to be trusted if it exceeds 1.
#   - societal control enters as its MEAN SCORE, an observed variable, so the
#     diagram does not need twelve more indicator boxes. Its effect was at most
#     +/-.02 as a mediator in script 27.
#   - only the WANTED role is modelled; the seen role is in script 26.
#
# Position runs 1 Master ... 6 Object (positive = a role further from Master);
# extremity = 1 for either pole. Age, gender and five country dummies are in
# the model on every equation and are left out of the drawing.
#
# Requires: 01, 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
dat$pos_should <- dat$typ_should
dat$ext_should <- as.integer(dat$typ_should %in% c(1, 6))
dat$ctl_mean   <- dat$control_mean
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
covs <- paste(c("age_num", "gender_bin", "edu_primary", "edu_higher", "edu_na", paste0("c_", others)), collapse = " + ")

mod <- paste0(
  "restorative =~ ", paste(g$restorative, collapse = " + "), "\n",
  "dialogic =~ ", paste(g$dialogic, collapse = " + "), "\n",
  "place =~ ", paste(mp, collapse = " + "), "\n",
  "place ~ a1*restorative + a2*dialogic + ", covs, "\n",
  "ctl_mean ~ restorative + dialogic + ", covs, "\n",
  "pos_should ~ d1*restorative + d2*dialogic + b1*place + ctl_mean + ", covs, "\n",
  "ext_should ~ restorative + dialogic + place + ctl_mean + ", covs, "\n",
  "pos_should ~~ ext_should\n",
  "ind_rest := a1 * b1\n",
  "ind_dia  := a2 * b1\n",
  "tot_rest := d1 + a1 * b1\n",
  "tot_dia  := d2 + a2 * b1\n")

t0 <- Sys.time()
fit <- sem(mod, dat, estimator = "WLSMV",
           ordered = c("pos_should", "ext_should", mp, g$restorative, g$dialogic))
cat("fitted in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "seconds\n")

cat("========== 1. Fit ==========\n")
print(round(fitmeasures(fit, c("chisq", "df", "cfi", "tli", "rmsea", "srmr")), 3))
ps <- standardizedSolution(fit)
reg <- ps[ps$op == "~" & !grepl("^c_|^age|^gender|^edu", ps$rhs), ]
cat("largest standardised structural path:", round(max(abs(reg$est.std)), 2),
    ifelse(max(abs(reg$est.std)) > 1, "  <- UNSTABLE, do not interpret", "  (inside +/-1)"), "\n")

cat("\n========== 2. Structural paths (standardised) ==========\n")
show <- reg[, c("lhs", "rhs", "est.std", "pvalue")]
show$est.std <- round(show$est.std, 3); show$pvalue <- signif(show$pvalue, 2)
names(show) <- c("outcome", "predictor", "beta", "p"); print(show, row.names = FALSE)

cat("\n========== 3. What the motives do to the wanted position ==========\n")
dp <- ps[ps$label %in% c("ind_rest", "ind_dia", "tot_rest", "tot_dia"), c("label", "est.std", "ci.lower", "ci.upper", "pvalue")]
dp[, 2:4] <- round(dp[, 2:4], 3); dp$pvalue <- signif(dp$pvalue, 2); print(dp, row.names = FALSE)
cat("(ind_ = the indirect effect through place agency; tot_ = direct plus indirect. Compare\n")
cat(" with script 27, which fitted the motives one at a time.)\n")

r2 <- inspect(fit, "r2")
cat(sprintf("\nR2: place agency %.3f | position wanted %.3f | extremity wanted %.3f\n",
            r2[["place"]], r2[["pos_should"]], r2[["ext_should"]]))

saveRDS(list(fit = fit), "hnr_story_model.rds")
cat("\nSaved: hnr_story_model.rds\n")
