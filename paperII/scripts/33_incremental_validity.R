# ============================================================
# 33_incremental_validity.R
# Does place agency predict the wanted role beyond its nearest rivals?
#
# Place agency (five questions about one's own favourite place) could be a
# re-description of constructs the survey also measures in general terms:
#   relational belief   Q1, six items on people and places influencing each other
#   general agency      Q2, twelve items: is dialogue possible with non-human beings
#   societal control    Q4, twelve items: how far humans control natural entities
#   the three motives   Q13: restful, dialogic, serviced
#
#   1. how place agency correlates with each rival (is it a distinct construct?)
#   2. hierarchical models of the wanted role: what place agency adds AFTER all
#      rivals, and what the rivals add after place agency (ordered logit,
#      McFadden pseudo-R2, likelihood-ratio tests, bootstrap interval)
#   3. dominance analysis: the fit each predictor contributes on average over
#      all subsets of the others (Azen & Traxel 2009), for the role wanted and
#      the role seen
#   4. the same question for rejecting the mastery one sees (script 32)
#   5. a latent model (WLSMV), where unreliability cannot favour any scale
#
# Age, gender, education and country are in every model. Odds ratios per SD.
#
# Requires: 01, 04, 13 already run. Packages: MASS, lavaan.
# ============================================================

suppressMessages({ library(MASS); library(lavaan) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
q1 <- c("presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
        "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person")
ag  <- grep("^ag_now_", names(dat), value = TRUE)
ctl <- grep("^ctl_", names(dat), value = TRUE)
zs <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
dat$place      <- zs(rowMeans(dat[, mp]))
dat$relational <- zs(rowMeans(dat[, q1]))
dat$agency     <- zs(rowMeans(dat[, ag]))
dat$control    <- zs(dat$control_mean)
dat$restful    <- zs(rowMeans(dat[, g$restorative]))
dat$dialogic   <- zs(rowMeans(dat[, g$dialogic]))
dat$serviced   <- zs(rowMeans(dat[, g$serviced]))
rivals <- c("relational", "agency", "control", "restful", "dialogic", "serviced")
preds  <- c("place", rivals)
lab <- c(place = "place agency", relational = "relational belief (Q1)", agency = "general agency (Q2)",
         control = "societal control (Q4)", restful = "restful motive", dialogic = "dialogic motive", serviced = "serviced motive")
cov_txt <- "age_num + gender_bin + edu_primary + edu_higher + edu_na + country"
d <- dat[complete.cases(dat[, c("typ_now", "typ_should", "age_num", "gender_bin", preds)]), ]
d$w <- factor(d$typ_should, levels = 1:6, ordered = TRUE); d$s <- factor(d$typ_now, levels = 1:6, ordered = TRUE)
cat("respondents:", nrow(d), "\n")

cat("\n========== 1. Is place agency a distinct construct? ==========\n")
cat("correlation of place agency with each rival (Pearson), and the rivals among themselves:\n")
R <- cor(d[, preds]); dimnames(R) <- list(lab[preds], c("place", "relat", "agency", "control", "restful", "dialog", "servic"))
print(round(R, 2))
cat(sprintf("largest correlation of place agency with a rival: %.2f; share of its variance the six rivals explain together: %.1f%%\n",
            max(abs(R[1, -1])), 100 * summary(lm(as.formula(paste("place ~", paste(rivals, collapse = " + "))), d))$r.squared))

mcf <- function(y, rhs, data = d) {
  f  <- polr(as.formula(paste(y, "~", paste(c(rhs, cov_txt), collapse = " + "))), data, Hess = TRUE)
  f0 <- polr(as.formula(paste(y, "~ 1")), data)
  list(r2 = 1 - as.numeric(logLik(f)) / as.numeric(logLik(f0)), ll = as.numeric(logLik(f)), fit = f)
}

cat("\n========== 2. What place agency adds after the rivals, and the rivals after place agency ==========\n")
for (y in c("w", "s")) {
  base <- mcf(y, character(0)); riv <- mcf(y, rivals); plc <- mcf(y, "place"); all <- mcf(y, preds)
  cat(sprintf("\n-- role %s (ordered logit, McFadden pseudo-R2) --\n", ifelse(y == "w", "WANTED", "SEEN")))
  cat(sprintf("covariates only %.4f | + six rivals %.4f | + place agency on top %.4f  (place agency adds %.4f; LR chi2(1) = %.1f, p = %.2g)\n",
              base$r2, riv$r2, all$r2, all$r2 - riv$r2, 2 * (all$ll - riv$ll), pchisq(2 * (all$ll - riv$ll), 1, lower.tail = FALSE)))
  cat(sprintf("covariates only %.4f | + place agency %.4f | + six rivals on top %.4f  (rivals add %.4f; LR chi2(6) = %.1f, p = %.2g)\n",
              base$r2, plc$r2, all$r2, all$r2 - plc$r2, 2 * (all$ll - plc$ll), pchisq(2 * (all$ll - plc$ll), 6, lower.tail = FALSE)))
  s <- coef(summary(all$fit))[preds, ]
  print(data.frame(predictor = lab[preds], OR_all_together = sprintf("%.2f [%.2f, %.2f]", exp(s[, 1]), exp(s[, 1] - 1.96 * s[, 2]), exp(s[, 1] + 1.96 * s[, 2])),
                   t = round(s[, 3], 1), row.names = NULL), row.names = FALSE)
}
set.seed(20260930)
bs <- replicate(300, { b <- d[sample(nrow(d), replace = TRUE), ]; mcf("w", preds, b)$r2 - mcf("w", rivals, b)$r2 })
cat(sprintf("\nplace agency's added pseudo-R2 for the wanted role, 95%% bootstrap interval: [%.4f, %.4f]\n", quantile(bs, .025), quantile(bs, .975)))
p_or <- function(y, rhs) { s <- coef(summary(mcf(y, rhs)$fit))["place", ]; exp(s[1]) }
cat(sprintf("odds ratio of place agency for the wanted role: alone %.2f | with all six rivals %.2f\n", p_or("w", "place"), p_or("w", preds)))

cat("\n========== 3. Dominance analysis: the fit each predictor adds on average over all subsets of the others ==========\n")
dominance <- function(y) {
  k <- length(preds); sub <- expand.grid(rep(list(c(FALSE, TRUE)), k)); names(sub) <- preds
  r2 <- apply(sub, 1, function(z) mcf(y, preds[z])$r2); key <- apply(sub, 1, function(z) paste(as.integer(z), collapse = ""))
  base <- r2[key == paste(rep(0, k), collapse = "")]
  gd <- sapply(preds, function(p) {
    j <- match(p, preds); without <- sub[!sub[[p]], , drop = FALSE]
    inc <- apply(without, 1, function(z) { z2 <- z; z2[j] <- TRUE
      r2[key == paste(as.integer(z2), collapse = "")] - r2[key == paste(as.integer(z), collapse = "")] })
    size <- rowSums(without); mean(tapply(inc, size, mean))       # average within subset size, then across sizes
  })
  list(gd = gd, total = r2[key == paste(rep(1, k), collapse = "")] - base)
}
dw <- dominance("w"); ds <- dominance("s")
tab3 <- data.frame(predictor = lab[preds],
  wanted = round(dw$gd, 4), wanted_share = sprintf("%.0f%%", 100 * dw$gd / dw$total),
  seen = round(ds$gd, 4), seen_share = sprintf("%.0f%%", 100 * ds$gd / ds$total), row.names = NULL)
print(tab3[order(-dw$gd), ], row.names = FALSE)
cat(sprintf("(general dominance weights; they sum to the pseudo-R2 the seven predictors add over the covariates: wanted %.4f, seen %.4f)\n", dw$total, ds$total))

cat("\n========== 4. The same question for rejecting the mastery one sees ==========\n")
m <- d[d$typ_now == 1, ]; m$reject <- as.integer(m$typ_should != 1)
lg <- function(rhs) glm(as.formula(paste("reject ~", paste(c(rhs, cov_txt), collapse = " + "))), binomial, m)
f0 <- glm(reject ~ 1, binomial, m); r2g <- function(f) 1 - as.numeric(logLik(f)) / as.numeric(logLik(f0))
fr <- lg(rivals); fa <- lg(preds); fp <- lg("place")
cat(sprintf("n = %d | six rivals %.4f | + place agency %.4f (adds %.4f; LR chi2(1) = %.1f, p = %.2g) | place agency alone %.4f\n",
            nrow(m), r2g(fr), r2g(fa), r2g(fa) - r2g(fr), 2 * (as.numeric(logLik(fa)) - as.numeric(logLik(fr))),
            pchisq(2 * (as.numeric(logLik(fa)) - as.numeric(logLik(fr))), 1, lower.tail = FALSE), r2g(fp)))
s4 <- coef(summary(fa))[preds, ]
print(data.frame(predictor = lab[preds], OR = round(exp(s4[, 1]), 2), lo = round(exp(s4[, 1] - 1.96 * s4[, 2]), 2),
                 hi = round(exp(s4[, 1] + 1.96 * s4[, 2]), 2), p = signif(s4[, 4], 2), row.names = NULL), row.names = FALSE)

cat("\n========== 5. Latent model: all constructs as latent variables (WLSMV) ==========\n")
dat$pos_should <- dat$typ_should
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
covs <- paste(c("age_num", "gender_bin", "edu_primary", "edu_higher", "edu_na", paste0("c_", others)), collapse = " + ")
lat <- c("place_f", "relational_f", "agency_f", "control_f", "restful_f", "dialogic_f")
mod <- paste0(
  "place_f =~ ", paste(mp, collapse = " + "), "\n",
  "relational_f =~ ", paste(q1, collapse = " + "), "\n",
  "agency_f =~ ", paste(ag, collapse = " + "), "\n",
  "control_f =~ ", paste(ctl, collapse = " + "), "\n",
  "restful_f =~ ", paste(g$restorative, collapse = " + "), "\n",
  "dialogic_f =~ ", paste(g$dialogic, collapse = " + "), "\n",
  "pos_should ~ ", paste(lat, collapse = " + "), " + ", covs, "\n")
t0 <- Sys.time()
fit <- sem(mod, dat, estimator = "WLSMV", ordered = c("pos_should", mp, q1, ag, ctl, g$restorative, g$dialogic))
cat("fitted in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "seconds\n")
print(round(fitmeasures(fit, c("cfi", "tli", "rmsea", "srmr")), 3))
ps <- standardizedSolution(fit); r <- ps[ps$op == "~" & ps$lhs == "pos_should" & ps$rhs %in% lat, ]
print(data.frame(latent = sub("_f$", "", r$rhs), beta = round(r$est.std, 3), lo = round(r$ci.lower, 3), hi = round(r$ci.upper, 3),
                 p = signif(r$pvalue, 2)), row.names = FALSE)
cat(sprintf("largest standardised path: %.2f%s\n", max(abs(r$est.std)), ifelse(max(abs(r$est.std)) > 1, "  <- UNSTABLE", "  (inside +/-1)")))
lc <- ps[ps$op == "~~" & ps$lhs == "place_f" & ps$rhs %in% lat[-1], ]
cat("latent correlations of place agency with the rivals:", paste(sprintf("%s %.2f", sub("_f$", "", lc$rhs), lc$est.std), collapse = " | "), "\n")
cat("(the serviced motive is left out of the latent model, as in scripts 26-28)\n")

saveRDS(list(correlations = R, dominance_wanted = dw, dominance_seen = ds, table = tab3,
             latent = ps[ps$op == "~" & ps$lhs == "pos_should", ]), "hnr_incremental_validity.rds")
cat("\nSaved: hnr_incremental_validity.rds\n")
