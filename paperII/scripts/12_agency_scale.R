# ============================================================
# 12_agency_scale.R
# The agency-of-non-human-beings scale (Q2 in the instrument): can
# dialogue be held with wild animals, pets, plants, forces of nature,
# a river, dunes, a forest, a lake, the sea, soil, a mountain, a place
# in nature? Twelve targets, 1 (definitely not) to 5 (definitely yes).
#
# Why this block matters. It is the strongest scale in the survey
# (alpha .95 pooled, against .82-.88 for the six-item relational scale
# the paper currently carries as its secondary measure), and Q3 repeats
# the same twelve items about the FUTURE. Two occasions with twelve
# indicators each identify a latent change model, which the single
# forced-choice role item does not.
#
# The thirteenth item of each block, dialogue with another human, is not
# an indicator of the construct: it is held out as a discriminant anchor.
#
# Requires: 01 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds")
lab <- c("wild_animals", "pets", "plants", "forces", "river", "dunes",
         "forest", "lake", "sea", "soil", "mountain", "place")
now <- paste0("ag_now_", lab)
fut <- paste0("ag_fut_", lab)

alpha <- function(m) { m <- m[complete.cases(m), , drop = FALSE]; k <- ncol(m)
  (k / (k - 1)) * (1 - sum(apply(m, 2, var)) / var(rowSums(m))) }

cat("========== 1. Items and descriptives (now) ==========\n")
desc <- data.frame(item = lab,
                   mean = round(colMeans(dat[, now], na.rm = TRUE), 2),
                   sd   = round(apply(dat[, now], 2, sd, na.rm = TRUE), 2),
                   agree_pct = round(100 * colMeans(dat[, now] >= 4, na.rm = TRUE), 1))
print(desc[order(-desc$mean), ], row.names = FALSE)
cat("\nalpha now:", round(alpha(dat[, now]), 3),
    " future:", round(alpha(dat[, fut]), 3), "\n")
cat("Discriminant anchor -- dialogue with another human: mean",
    round(mean(dat$dial_human, na.rm = TRUE), 2),
    "| correlation with the scale:",
    round(cor(rowMeans(dat[, now]), dat$dial_human, use = "complete.obs"), 3), "\n")
cat("(The anchor should sit well above the non-human items and correlate only\n")
cat(" moderately with them; a high correlation would suggest the scale measures\n")
cat(" willingness to say yes rather than beliefs about non-human agency.)\n")

cat("\n========== 2. Dimensionality: are living and non-living targets distinct? ==========\n")
animate  <- paste0("ag_now_", c("wild_animals", "pets", "plants"))
inanimate <- paste0("ag_now_", c("forces", "river", "dunes", "forest", "lake",
                                 "sea", "soil", "mountain", "place"))
m1 <- 'agency =~ ag_now_wild_animals + ag_now_pets + ag_now_plants + ag_now_forces +
        ag_now_river + ag_now_dunes + ag_now_forest + ag_now_lake + ag_now_sea +
        ag_now_soil + ag_now_mountain + ag_now_place'
m2 <- paste0('living =~ ', paste(animate, collapse = " + "), '
             landscape =~ ', paste(inanimate, collapse = " + "))
f1 <- sem(m1, dat, estimator = "MLR"); f2 <- sem(m2, dat, estimator = "MLR")
print(round(rbind(one_factor = fitmeasures(f1, c("chisq","df","cfi","tli","rmsea","srmr")),
                  two_factor = fitmeasures(f2, c("chisq","df","cfi","tli","rmsea","srmr"))), 3))
r12 <- lavInspect(f2, "cor.lv")[1, 2]
cat("correlation between the two factors:", round(r12, 3), "\n")
cat("(A correlation near 1 means the split is not worth keeping; report the\n")
cat(" two-factor model as a sensitivity check and treat the scale as one.)\n")

cat("\n========== 3. Measurement invariance across the six countries ==========\n")
inv <- list(
  configural = cfa(m1, dat, group = "country", estimator = "MLR"),
  metric     = cfa(m1, dat, group = "country", estimator = "MLR", group.equal = "loadings"),
  scalar     = cfa(m1, dat, group = "country", estimator = "MLR",
                   group.equal = c("loadings", "intercepts")))
tab <- t(sapply(inv, function(f) fitmeasures(f, c("chisq","df","cfi","rmsea","srmr"))))
tab <- cbind(round(tab, 3), dCFI = round(c(NA, diff(tab[, "cfi"])), 4))
print(tab)
cat("(Chen 2007: a drop in CFI larger than .010 counts against invariance.)\n")
cat("\nLatent means by country, scalar model (Canada = 0):\n")
pe <- parameterEstimates(inv$scalar)
lm_ <- pe[pe$op == "~1" & pe$lhs == "agency", ]
print(data.frame(country = levels(dat$country),
                 est = round(lm_$est[match(seq_along(levels(dat$country)), lm_$group)], 3),
                 p   = signif(lm_$pvalue[match(seq_along(levels(dat$country)), lm_$group)], 3)),
      row.names = FALSE)

cat("\n========== 4. Now vs future: a latent change model ==========\n")
# Two occasions, twelve indicators each. Loadings and intercepts are held
# equal across occasions (the condition for comparing latent means at all),
# and the residuals of each repeated item are allowed to correlate.
res_cov <- paste(paste0(now, " ~~ ", fut), collapse = "\n  ")
# Comparing latent means across occasions requires BOTH loadings and item
# intercepts to be equal across occasions; constraining loadings alone leaves
# the mean difference unidentified.
int_now <- paste(paste0(now, " ~ i", seq_along(now), "*1"), collapse = "\n  ")
int_fut <- paste(paste0(fut, " ~ i", seq_along(fut), "*1"), collapse = "\n  ")
m_long <- paste0(
  'AgNow =~ ', paste(paste0("L", seq_along(now), "*", now), collapse = " + "), '
   AgFut =~ ', paste(paste0("L", seq_along(fut), "*", fut), collapse = " + "), '
  ', res_cov, '
  ', int_now, '
  ', int_fut, '
   AgNow ~ 0*1
   AgFut ~ 1')
f_long <- sem(m_long, dat, estimator = "MLR")
print(round(fitmeasures(f_long, c("chisq","df","cfi","tli","rmsea","srmr")), 3))
ch <- parameterEstimates(f_long)
ch <- ch[ch$op == "~1" & ch$lhs == "AgFut", ]
cat("\nLatent change, future minus now (Now fixed at 0):\n")
cat(sprintf("  estimate %+.3f  SE %.3f  z %.2f  p %.3g\n", ch$est, ch$se, ch$z, ch$pvalue))
cat("(Positive would mean respondents expect dialogue to become more possible.\n")
cat(" This is the comparison the role item cannot support, because one nominal\n")
cat(" indicator per occasion does not identify a latent change model.)\n")
cat("\nObserved means for comparison: now", round(mean(rowMeans(dat[, now])), 3),
    " future", round(mean(rowMeans(dat[, fut])), 3), "\n")

cat("\n========== 5. Validity against what is already in the paper ==========\n")
dat$agency_mean <- rowMeans(dat[, now], na.rm = TRUE)
cat("correlation with the six-item relational scale:",
    round(cor(dat$agency_mean, dat$relational, use = "complete.obs"), 3), "\n")
cat("correlation with the 12-item control battery (1 exploit .. 4 subject):",
    round(cor(dat$agency_mean, dat$control_mean, use = "complete.obs"), 3), "\n")
cat("correlation with acquiescence (ARS):",
    round(cor(dat$agency_mean, dat$ARS, use = "complete.obs"), 3),
    "| midpoint (MRS):", round(cor(dat$agency_mean, dat$MRS, use = "complete.obs"), 3), "\n")
typ <- c("Master","Manager","User","Guardian","Partner","Object")
cat("\nAgency by IDEAL role:\n")
print(round(tapply(dat$agency_mean, factor(dat$typ_should, 1:6, typ), mean), 3))
a <- anova(lm(agency_mean ~ factor(typ_should), dat))
cat("eta2 =", round(a[1, "Sum Sq"] / sum(a[, "Sum Sq"]), 4),
    " F =", round(a[1, "F value"], 2), " p =", signif(a[1, "Pr(>F)"], 3), "\n")
cat("(For reference, the relational scale reaches eta2 = .011 on the same outcome.)\n")

saveRDS(list(fits = list(one = f1, two = f2, invariance = inv, longitudinal = f_long),
             desc = desc, alpha_now = alpha(dat[, now]), alpha_fut = alpha(dat[, fut])),
        "hnr_agency_scale.rds")
cat("\nSaved: hnr_agency_scale.rds\n")
