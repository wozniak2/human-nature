# ============================================================
# 16_motive_invariance.R
# Can the three motives be compared across the six countries?
#
# Comparing latent means requires the measurement model to hold in the
# same way everywhere. It does not. Metric invariance is already
# borderline (dCFI = -.010) and scalar invariance clearly fails
# (-.022), so the country means from the scalar model are printed as a
# diagnostic and should NOT be read as substantive differences.
#
# This does not affect the predictive results in 14 and 15. Those are
# estimated within the pooled sample with country as a covariate, and
# ask whether motives predict the role a respondent wants, not whether
# one country holds more of a motive than another.
#
# Requires: 01 and 13 already run. Packages: lavaan.
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

mod <- paste0('restorative =~ ', paste(g$restorative, collapse = " + "), '
  dialogic    =~ ', paste(g$dialogic, collapse = " + "), '
  serviced    =~ ', paste(g$serviced, collapse = " + "))

fits <- list(
  configural = cfa(mod, dat, group = "country", estimator = "MLR"),
  metric     = cfa(mod, dat, group = "country", estimator = "MLR", group.equal = "loadings"),
  scalar     = cfa(mod, dat, group = "country", estimator = "MLR",
                   group.equal = c("loadings", "intercepts")))

cat("========== 1. Invariance across the six countries ==========\n")
tab <- t(sapply(fits, function(f) fitmeasures(f, c("chisq","df","cfi","rmsea","srmr"))))
print(cbind(round(tab, 3), dCFI = round(c(NA, diff(tab[, "cfi"])), 4)))
cat("(Chen 2007: a CFI drop beyond .010 counts against invariance. Metric is\n")
cat(" borderline and scalar fails, so the means below are diagnostic only.)\n")

cat("\n========== 2. Latent means by country, scalar model (Canada = 0) ==========\n")
pe  <- parameterEstimates(fits$scalar)
lat <- pe[pe$op == "~1" & pe$lhs %in% names(g), ]
ct  <- levels(dat$country)
est <- do.call(cbind, lapply(names(g), function(f) {
  r <- lat[lat$lhs == f, ]; round(r$est[match(seq_along(ct), r$group)], 3) }))
pv  <- do.call(cbind, lapply(names(g), function(f) {
  r <- lat[lat$lhs == f, ]; signif(r$pvalue[match(seq_along(ct), r$group)], 2) }))
dimnames(est) <- list(ct, names(g)); dimnames(pv) <- list(ct, names(g))
print(est); cat("\np-values:\n"); print(pv)
cat("\nDO NOT report these as country differences in motive: the measurement\n")
cat("model that would license the comparison does not hold. What they show is\n")
cat("where non-invariance is concentrated -- the Netherlands and Sweden sit\n")
cat("lowest on all three, which is the same pattern those two samples show on\n")
cat("every scale in the survey and is bound up with the English version.\n")

cat("\n========== 3. Which items carry the non-invariance? ==========\n")
mi <- lavTestScore(fits$metric)$uni
mi <- mi[order(-mi$X2), ][1:8, ]
pt <- parTable(fits$metric)
lbl <- function(p) paste0(pt$lhs[pt$plabel == p], " =~ ", pt$rhs[pt$plabel == p],
                          " (group ", pt$group[pt$plabel == p], ")")
cat("largest loading constraints by score test:\n")
for (i in seq_len(nrow(mi)))
  cat(sprintf("  X2 = %6.1f  %s\n", mi$X2[i], lbl(mi$lhs[i])))

saveRDS(list(fits = fits, means = est, p = pv), "hnr_motive_invariance.rds")
cat("\nSaved: hnr_motive_invariance.rds\n")
