# ============================================================
# 11_square_table_models.R
# Loglinear models for the square (6 x 6) table of perceived vs ideal
# role. Script 08 tests symmetry (Bowker) and marginal homogeneity
# (Stuart-Maxwell) separately; this fits the standard nested family for
# paired categorical data (Bishop, Fienberg & Holland 1975; Agresti
# 2013, ch. 11) so the structure is modelled rather than tested
# piecemeal:
#
#   independence  - the two answers are unrelated (a floor, not credible)
#   quasi-indep.  - unrelated apart from the diagonal (staying put)
#   symmetry      - n_ij = n_ji, no net drift in any direction
#   quasi-symmetry- symmetric association, margins free to differ
#
# Marginal homogeneity is the difference between symmetry and
# quasi-symmetry, so the conditional test QS -> S is the likelihood-ratio
# counterpart of Stuart-Maxwell. If QS fits and S does not, the movement
# is not random exchange: it has a direction, and the fitted marginal
# parameters say how far each role shifts.
#
# Requires: 01 and 08 already run.
# ============================================================

dat <- readRDS("hnr_data.rds")
typ <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")
k   <- length(typ)

tab <- table(now = factor(dat$typ_now, 1:k, typ), should = factor(dat$typ_should, 1:k, typ))
d <- as.data.frame(tab, responseName = "n")
d$now    <- factor(d$now, levels = typ)
d$should <- factor(d$should, levels = typ)
i <- as.integer(d$now); j <- as.integer(d$should)

# symmetry: one parameter per unordered pair {i,j}
d$pair <- factor(ifelse(i <= j, paste(i, j, sep = "_"), paste(j, i, sep = "_")))
# quasi-independence: a free parameter for each diagonal cell, measured
# against the off-diagonal cells so every role gets an interpretable estimate
d$diag <- relevel(factor(ifelse(i == j, as.character(i), "off")), ref = "off")

fit <- list(
  independence   = glm(n ~ now + should,               family = poisson, data = d),
  quasi_indep    = glm(n ~ now + should + diag,        family = poisson, data = d),
  symmetry       = glm(n ~ pair,                       family = poisson, data = d),
  quasi_symmetry = glm(n ~ now + should + pair,        family = poisson, data = d)
)

cat("========== 1. Fit of the nested family ==========\n")
gof <- t(sapply(fit, function(m) c(G2 = round(deviance(m), 1), df = df.residual(m),
                                   p = signif(pchisq(deviance(m), df.residual(m), lower.tail = FALSE), 3),
                                   BIC = round(BIC(m), 1))))
print(gof)
cat("(G2 is the likelihood-ratio statistic against the saturated table;\n")
cat(" a LARGE p means the model is consistent with the data.)\n")

cat("\n========== 2. Marginal homogeneity: quasi-symmetry vs symmetry ==========\n")
# Under quasi-symmetry the difference between these two models tests exactly
# the hypothesis Stuart-Maxwell tests, with k-1 df.
print(anova(fit$symmetry, fit$quasi_symmetry, test = "LRT"))
cat("A significant difference means the two margins differ: the roles people\n")
cat("see and the roles they want are not the same distribution.\n")

cat("\n========== 3. How far does each role shift? (quasi-symmetry margins) ==========\n")
# In the quasi-symmetry model the now/should contrasts carry the marginal
# shift once the symmetric association is accounted for. Positive = the role
# is chosen more often as the ideal than as the perceived role.
co <- coef(summary(fit$quasi_symmetry))
sh <- co[grepl("^should", rownames(co)), , drop = FALSE]
nw <- co[grepl("^now", rownames(co)),   , drop = FALSE]
role <- sub("^should", "", rownames(sh))
shift <- data.frame(role = role,
                    log_shift = round(sh[, 1] - nw[, 1], 3),
                    se = round(sqrt(sh[, 2]^2 + nw[, 2]^2), 3))
shift$z <- round(shift$log_shift / shift$se, 2)
shift <- shift[order(-shift$log_shift), ]
print(shift, row.names = FALSE)
cat("(Master is the reference category, so its shift is 0 by construction;\n")
cat(" values are relative to it. Compare with the net percentage-point\n")
cat(" changes in section 8 of 08_mastery_paradox.R.)\n")

cat("\n========== 4. Staying put: the diagonal (quasi-independence) ==========\n")
dg <- coef(summary(fit$quasi_indep))
dg <- dg[grepl("^diag", rownames(dg)), , drop = FALSE]
stay <- data.frame(role = typ[as.integer(sub("^diag", "", rownames(dg)))],
                   log_odds_of_staying = round(dg[, 1], 3), se = round(dg[, 2], 3),
                   p = signif(dg[, 4], 3))
print(stay[order(-stay$log_odds_of_staying), ], row.names = FALSE)
cat("(Higher = respondents in that role are more likely to keep it than\n")
cat(" chance association would predict. The small, distinctive categories\n")
cat(" (Object, Partner) hold their own most strongly; Manager and User, in\n")
cat(" the middle of the range, are the ones people most readily leave.)\n")

cat("\n========== 5. Same family fitted per country ==========\n")
per_country <- do.call(rbind, lapply(levels(dat$country), function(cn) {
  x <- subset(dat, country == cn)
  tt <- table(factor(x$typ_now, 1:k), factor(x$typ_should, 1:k))
  dd <- as.data.frame(tt, responseName = "n")
  ii <- as.integer(dd$Var1); jj <- as.integer(dd$Var2)
  dd$pair <- factor(ifelse(ii <= jj, paste(ii, jj, sep = "_"), paste(jj, ii, sep = "_")))
  s  <- glm(n ~ pair, family = poisson, data = dd)
  qs <- glm(n ~ Var1 + Var2 + pair, family = poisson, data = dd)
  a  <- anova(s, qs, test = "LRT")
  data.frame(country = cn,
             QS_G2 = round(deviance(qs), 1), QS_df = df.residual(qs),
             QS_p = signif(pchisq(deviance(qs), df.residual(qs), lower.tail = FALSE), 3),
             MH_G2 = round(a[2, "Deviance"], 1), MH_df = a[2, "Df"],
             MH_p = signif(a[2, "Pr(>Chi)"], 3))
}))
print(per_country, row.names = FALSE)
cat("(QS_p large = quasi-symmetry is an adequate description in that country;\n")
cat(" MH_p small = its two margins differ, i.e. a real shift. Canada is the\n")
cat(" country where no shift was detected in 08.)\n")

saveRDS(list(models = fit, gof = gof, shift = shift, per_country = per_country),
        "hnr_square_table_models.rds")
cat("\nSaved: hnr_square_table_models.rds\n")
