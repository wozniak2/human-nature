# ============================================================
# 14_motives_and_roles.R
# Two questions about the three motives found in 13_reasons_efa.R
# (restorative, dialogic, serviced):
#
#   Who holds them?   motive ~ country + age + gender
#   What do they do?  ideal role ~ motives + country + age + gender
#
# The second model is the point of the script. A motive that only
# reproduces the country pattern adds nothing to the paper, so the
# motives are entered alongside country, age and gender and judged on
# what they add to a model that already contains them.
#
# Requires: 01 and 13 already run. Packages: nnet.
# ============================================================

suppressMessages(library(nnet))
dat <- readRDS("hnr_data.rds")
re  <- readRDS("hnr_reasons_efa.rds")

# name each retained factor by the item that defines it, as in 10_figures.R
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
groups <- re$groups
names(groups) <- vapply(groups, function(v) {
  L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]])
}, character(1))
mot <- names(groups)
for (g in mot) dat[[g]] <- rowMeans(dat[, groups[[g]], drop = FALSE], na.rm = TRUE)
cat("motive scores built from:\n")
for (g in mot) cat(sprintf("  %-12s %s\n", g, paste(sub("^rs_", "", groups[[g]]), collapse = ", ")))

typ <- c("Master","Manager","User","Guardian","Partner","Object")
dat$role_ideal <- factor(dat$typ_should, 1:6, typ)
dat$role_now   <- factor(dat$typ_now, 1:6, typ)
dat$woman <- ifelse(dat$gender_bin == 0, "woman", "man")

cat("\n========== 1. Who holds which motive? ==========\n")
for (g in mot) {
  cat("--", g, "--\n")
  print(round(tapply(dat[[g]], dat$country, mean, na.rm = TRUE), 2))
  m <- lm(as.formula(paste(g, "~ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), dat)
  a <- anova(m)
  cat(sprintf("   country p = %-9.3g partial eta2 = %.3f | age b = %+.3f p = %-8.3g | man vs woman b = %+.3f p = %.3g\n",
      a["country","Pr(>F)"], a["country","Sum Sq"]/sum(a[,"Sum Sq"]),
      coef(m)["age_num"], summary(m)$coefficients["age_num",4],
      coef(m)["gender_bin"], summary(m)$coefficients["gender_bin",4]))
  print(round(tapply(dat[[g]], dat$woman, mean, na.rm = TRUE), 2))
}
cat("(gender_bin: 0 = woman, 1 = man, so a positive coefficient means men score higher.)\n")

cat("\n========== 2. Do the motives predict the ideal role? (multinomial) ==========\n")
# Manager is the modal ideal, so it is the reference; coefficients are
# relative risk ratios for choosing role X over Manager.
dat$role_ideal <- relevel(dat$role_ideal, ref = "Manager")
keep <- complete.cases(dat[, c("role_ideal", mot, "country", "age_num", "gender_bin")])
d <- dat[keep, ]
cat("n =", nrow(d), "\n")
base <- multinom(role_ideal ~ country + age_num + gender_bin + edu_primary + edu_higher + edu_na, d, trace = FALSE)
full <- multinom(as.formula(paste("role_ideal ~", paste(mot, collapse = " + "),
                                  "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), d, trace = FALSE)
cat("\nDoes adding the three motives improve on country + age + gender?\n")
print(anova(base, full))
cat(sprintf("McFadden pseudo-R2: %.4f -> %.4f\n",
            1 - base$deviance / multinom(role_ideal ~ 1, d, trace = FALSE)$deviance,
            1 - full$deviance / multinom(role_ideal ~ 1, d, trace = FALSE)$deviance))

cat("\nRelative risk ratios for the motives (reference role: Manager)\n")
co <- summary(full)$coefficients; se <- summary(full)$standard.errors
for (g in mot) {
  rr <- exp(co[, g]); p <- 2 * pnorm(-abs(co[, g] / se[, g]))
  cat("--", g, "--\n")
  print(data.frame(vs_Manager = rownames(co), RRR = round(rr, 2),
                   lo = round(exp(co[, g] - 1.96 * se[, g]), 2),
                   hi = round(exp(co[, g] + 1.96 * se[, g]), 2),
                   p = signif(p, 3)), row.names = FALSE)
}
cat("(RRR above 1: a one-point rise in that motive raises the chance of choosing\n")
cat(" that role rather than Manager.)\n")

cat("\n========== 3. The same thing as a simple contrast: mastery as the ideal ==========\n")
d$master_should <- as.integer(d$typ_should == 1)
m0 <- glm(master_should ~ country + age_num + gender_bin + edu_primary + edu_higher + edu_na, binomial, d)
m1 <- glm(as.formula(paste("master_should ~", paste(mot, collapse = " + "),
                           "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), binomial, d)
print(anova(m0, m1, test = "Chisq"))
s <- coef(summary(m1))
print(data.frame(term = rownames(s), OR = round(exp(s[, 1]), 2),
                 lo = round(exp(s[, 1] - 1.96 * s[, 2]), 2),
                 hi = round(exp(s[, 1] + 1.96 * s[, 2]), 2),
                 p = signif(s[, 4], 3))[2:4, ], row.names = FALSE)

cat("\n========== 4. And among those who perceive mastery, who rejects it? ==========\n")
mn <- subset(d, typ_now == 1); mn$rej <- as.integer(mn$typ_should != 1)
cat("n =", nrow(mn), "\n")
r0 <- glm(rej ~ country + age_num + gender_bin + edu_primary + edu_higher + edu_na, binomial, mn)
r1 <- glm(as.formula(paste("rej ~", paste(mot, collapse = " + "),
                           "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), binomial, mn)
print(anova(r0, r1, test = "Chisq"))
s2 <- coef(summary(r1))
print(data.frame(term = rownames(s2), OR = round(exp(s2[, 1]), 2),
                 lo = round(exp(s2[, 1] - 1.96 * s2[, 2]), 2),
                 hi = round(exp(s2[, 1] + 1.96 * s2[, 2]), 2),
                 p = signif(s2[, 4], 3))[2:4, ], row.names = FALSE)

# tidy RRR table, saved so the figures script does not have to refit
co_ <- summary(full)$coefficients; se_ <- summary(full)$standard.errors
rrr_tab <- do.call(rbind, lapply(mot, function(g) data.frame(
  motive = g, role = rownames(co_), rrr = exp(co_[, g]),
  lo = exp(co_[, g] - 1.96 * se_[, g]), hi = exp(co_[, g] + 1.96 * se_[, g]),
  p = 2 * pnorm(-abs(co_[, g] / se_[, g])), row.names = NULL)))

saveRDS(list(base = base, full = full, master = m1, reject = r1, groups = groups, rrr = rrr_tab),
        "hnr_motives_roles.rds")
cat("\nSaved: hnr_motives_roles.rds\n")
