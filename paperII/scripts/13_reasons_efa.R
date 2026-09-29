# ============================================================
# 13_reasons_efa.R
# Why do people go to their natural place? Q13 asks fourteen reasons,
# rated 1-5. Pooled alpha is .81, but the block plainly mixes several
# things: relaxation, aesthetics, being taught by the place, physical
# activity, toilets and wifi. A single score would average them into
# nothing, so the structure is estimated rather than assumed.
#
# The sample is split once, stratified by country: the structure is
# found by exploratory factor analysis in one half and then tested by
# confirmatory factor analysis in the other. Reporting an EFA and a CFA
# on the same respondents would only confirm that the model was fitted
# to them.
#
# Requires: 01 already run. Packages: psych, GPArotation, lavaan.
# ============================================================

suppressMessages({ library(psych); library(GPArotation); library(lavaan) })
dat <- readRDS("hnr_data.rds")
lab <- c("relax", "beauty", "meet_people", "watch_plants", "meet_animals",
         "own_thoughts", "quiet", "communicates", "teaches", "struggles",
         "active", "comfort", "photos", "cheap")
it <- paste0("rs_", lab)
X <- dat[, it]

set.seed(20260929)
half <- unlist(lapply(split(seq_len(nrow(dat)), dat$country), function(i)
  sample(i, floor(length(i) / 2))))
explore <- X[half, ]; confirm <- X[-half, ]
cat("EFA sample:", nrow(explore), " CFA sample:", nrow(confirm), "\n")

cat("\n========== 1. Is the block factorable? ==========\n")
km <- KMO(explore)
cat("KMO overall:", round(km$MSA, 3), "\n")
print(round(sort(km$MSAi), 3))
bt <- cortest.bartlett(cor(explore, use = "complete.obs"), n = nrow(explore))
cat("Bartlett chi2 =", round(bt$chisq, 1), " df =", bt$df, " p =", signif(bt$p.value, 3), "\n")
cat("(KMO above .8 is good; items below .5 individually are poor candidates.)\n")

cat("\n========== 2. How many factors? ==========\n")
pa <- fa.parallel(explore, fa = "fa", plot = FALSE, n.iter = 50)
cat("parallel analysis suggests:", pa$nfact, "factors\n")
cat("observed eigenvalues:", paste(round(pa$fa.values[1:6], 2), collapse = " "), "\n")
cat("simulated  eigenvalues:", paste(round(pa$fa.sim[1:6], 2), collapse = " "), "\n")

cat("\n========== 3. Exploratory factor analysis (oblimin, ML) ==========\n")
k <- max(2, pa$nfact)
ef <- fa(explore, nfactors = k, rotate = "oblimin", fm = "ml")
L <- unclass(ef$loadings); L[abs(L) < 0.30] <- NA
out <- data.frame(item = lab, round(L, 2), h2 = round(ef$communality, 2))
print(out, row.names = FALSE, na.print = "")
cat("\nFactor correlations:\n"); print(round(ef$Phi, 2))
cat("\nVariance explained (cumulative):",
    round(colSums(ef$loadings^2) / nrow(L), 3), "\n")
cat("(Loadings below .30 are blanked. Items loading on nothing, or on two\n")
cat(" factors at once, are the ones to reconsider.)\n")

cat("\n========== 4. Confirming the structure in the held-out half ==========\n")
# Assign each item to the factor it loads on most strongly, keeping only
# items with a loading of at least .40 and no cross-loading within .15.
Lr <- unclass(ef$loadings)
best <- apply(abs(Lr), 1, which.max)
top  <- apply(abs(Lr), 1, max)
second <- apply(abs(Lr), 1, function(v) sort(v, decreasing = TRUE)[2])
keep <- top >= 0.40 & (top - second) >= 0.15
cat("items retained:", sum(keep), "of", length(keep), "\n")
if (any(!keep)) cat("dropped:", paste(lab[!keep], collapse = ", "), "\n")
groups <- split(it[keep], best[keep])
names(groups) <- paste0("F", names(groups))
mod <- paste(sapply(names(groups), function(g)
  paste0(g, " =~ ", paste(groups[[g]], collapse = " + "))), collapse = "\n")
cat("\nmodel tested on the held-out half:\n"); cat(mod, "\n")
fit <- cfa(mod, data = confirm, estimator = "MLR")
print(round(fitmeasures(fit, c("chisq","df","cfi","tli","rmsea","srmr")), 3))

cat("\n========== 5. Reliability and naming ==========\n")
for (g in names(groups)) {
  m <- confirm[, groups[[g]], drop = FALSE]
  m <- m[complete.cases(m), , drop = FALSE]
  a <- if (ncol(m) > 1) (ncol(m)/(ncol(m)-1)) * (1 - sum(apply(m,2,var))/var(rowSums(m))) else NA
  cat(sprintf("%-4s k=%d  alpha %s  items: %s\n", g, length(groups[[g]]),
      ifelse(is.na(a), "-", sprintf("%.2f", a)),
      paste(sub("^rs_", "", groups[[g]]), collapse = ", ")))
}

cat("\n========== 6. Do these motives relate to the role people want? ==========\n")
for (g in names(groups)) dat[[g]] <- rowMeans(dat[, groups[[g]], drop = FALSE], na.rm = TRUE)
typ <- c("Master","Manager","User","Guardian","Partner","Object")
for (g in names(groups)) {
  a <- anova(lm(as.formula(paste(g, "~ factor(typ_should)")), dat))
  cat(sprintf("%-4s  eta2 %.4f  p %-9.3g  means: %s\n", g,
      a[1,"Sum Sq"]/sum(a[,"Sum Sq"]), a[1,"Pr(>F)"],
      paste(sprintf("%s %.2f", typ, tapply(dat[[g]], factor(dat$typ_should,1:6,typ), mean)),
            collapse = "  ")))
}
cat("(For reference: the relational scale reaches eta2 = .011 and the agency\n")
cat(" scale .009 on the same outcome.)\n")

saveRDS(list(efa = ef, cfa = fit, groups = groups, parallel = pa, kmo = km),
        "hnr_reasons_efa.rds")
cat("\nSaved: hnr_reasons_efa.rds\n")
