# ============================================================
# 15_structural_model.R
# One model in which every latent measure competes for the same
# outcome. Until now each scale has been related to the role item on
# its own, which cannot say whether they carry the same information.
#
# Measurement: the three motive factors from 13, the agency scale from
# 12, and the belief scale used throughout the paper.
# Structural:  mastery as the ideal role, regressed on all five latents
#              plus age, gender and the five country dummies.
#
# The outcome is binary, so estimation is WLSMV with a probit link.
# Coefficients are reported standardised, which puts the latents on a
# common footing; the country dummies are kept in the model but are
# summarised rather than drawn, since none of them reaches significance.
#
# Requires: 01, 12 and 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds")
re  <- readRDS("hnr_reasons_efa.rds")

nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) {
  L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]])
}, character(1))

q1 <- c("presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
        "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person")
ag <- grep("^ag_now_", names(dat), value = TRUE)
dat$master_should <- as.integer(dat$typ_should == 1)
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd <- paste0("c_", others)

mod <- paste0(
 'restorative  =~ ', paste(g$restorative, collapse = " + "), '
  dialogic     =~ ', paste(g$dialogic,    collapse = " + "), '
  serviced     =~ ', paste(g$serviced,    collapse = " + "), '
  agency       =~ ', paste(ag, collapse = " + "), '
  relational_f =~ ', paste(q1, collapse = " + "), '
  master_should ~ restorative + dialogic + serviced + agency + relational_f +
                  age_num + gender_bin + ', paste(cd, collapse = " + "))

fit <- sem(mod, dat, estimator = "WLSMV", ordered = "master_should")

cat("========== 1. Fit ==========\n")
print(round(fitmeasures(fit, c("chisq","df","cfi","tli","rmsea","srmr")), 3))
cat("(CFI below .95: the agency block fits poorly on its own and carries that\n")
cat(" here too. The structural estimates are the point, not the global fit.)\n")

cat("\n========== 2. Standardised paths to mastery as the ideal ==========\n")
ps <- standardizedSolution(fit)
ps <- ps[ps$op == "~" & ps$lhs == "master_should", ]
ps$sig <- ifelse(ps$pvalue < .05, "*", "")
print(data.frame(predictor = ps$rhs, beta = round(ps$est.std, 3),
                 se = round(ps$se, 3), p = signif(ps$pvalue, 3), s = ps$sig),
      row.names = FALSE)
cat("\nCountry: none of the five dummies reaches p < .05 once the motives and\n")
cat("demographics are in the model (smallest p =",
    signif(min(ps$pvalue[grepl("^c_", ps$rhs)]), 3), ").\n")

cat("\n========== 3. What the two attitude scales add ==========\n")
mod_nolat <- paste0(
 'restorative =~ ', paste(g$restorative, collapse = " + "), '
  dialogic    =~ ', paste(g$dialogic,    collapse = " + "), '
  serviced    =~ ', paste(g$serviced,    collapse = " + "), '
  master_should ~ restorative + dialogic + serviced + age_num + gender_bin + ',
  paste(cd, collapse = " + "))
fit_nolat <- sem(mod_nolat, dat, estimator = "WLSMV", ordered = "master_should")
cat("R2 for the outcome, motives + demographics only:",
    round(inspect(fit_nolat, "r2")[["master_should"]], 4), "\n")
cat("R2 with the agency and belief scales added:    ",
    round(inspect(fit, "r2")[["master_should"]], 4), "\n")

cat("\n========== 4. Correlations among the latents ==========\n")
print(round(lavInspect(fit, "cor.lv")[1:5, 1:5], 2))

paths <- data.frame(predictor = ps$rhs, beta = ps$est.std, se = ps$se,
                    p = ps$pvalue, stringsAsFactors = FALSE)
saveRDS(list(fit = fit, fit_nolat = fit_nolat, paths = paths,
             n_items = c(restorative = length(g$restorative), dialogic = length(g$dialogic),
                         serviced = length(g$serviced), agency = length(ag),
                         relational_f = length(q1))),
        "hnr_structural_model.rds")
cat("\nSaved: hnr_structural_model.rds\n")
