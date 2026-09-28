# ============================================================
# 08_mastery_paradox.R
# Core analysis of the reframed paper: the gap between the human
# role respondents PERCEIVE in society ("how is it now", Q4) and the
# role they consider RIGHT ("how should it be", Q5). Both are the
# same six-narrative, pick-one item, so the comparison is within
# person and within format.
#
# NOTE: Q4 measures perceived current human role, NOT personal
# endorsement. Personal stance = Q5. (03_typology_dif.R treated Q4
# as endorsement and is superseded by this script.)
#
# Requires: 01 and 04 already run (hnr_data.rds has age_num, gender_bin)
# ============================================================

dat <- readRDS("hnr_data.rds")
typ <- c("Master","Manager","User","Guardian","Partner","Object")
pct <- function(x) round(100 * x, 1)

cat("========== 1. Perceived (now) vs ideal (should) role, % by country ==========\n")
cat("-- now --\n");    print(pct(prop.table(table(dat$country, dat$typ_now_f), 1)))
cat("-- should --\n"); print(pct(prop.table(table(dat$country, dat$typ_should_f), 1)))

cat("\n========== 2. Master now vs should, per country (McNemar) ==========\n")
by_c <- do.call(rbind, lapply(split(dat, dat$country), function(x) {
  mt <- mcnemar.test(table(factor(x$typ_now == 1, c(FALSE, TRUE)), factor(x$typ_should == 1, c(FALSE, TRUE))))
  data.frame(n = nrow(x),
             master_now = pct(mean(x$typ_now == 1)), master_should = pct(mean(x$typ_should == 1)),
             guard_partner_now = pct(mean(x$typ_now %in% 4:5)), guard_partner_should = pct(mean(x$typ_should %in% 4:5)),
             same_answer = pct(mean(x$typ_now == x$typ_should)),
             paradox_among_master_now = pct(mean(x$typ_should[x$typ_now == 1] != 1)),
             mcnemar_p = signif(mt$p.value, 3))
}))
print(by_c)
pooled <- mcnemar.test(table(dat$typ_now == 1, dat$typ_should == 1))
cat("Pooled McNemar chi2 =", round(pooled$statistic, 1), " p =", signif(pooled$p.value, 3), "\n")
cat("Pooled: Master now", pct(mean(dat$typ_now == 1)), "% | should", pct(mean(dat$typ_should == 1)),
    "% | same answer", pct(mean(dat$typ_now == dat$typ_should)), "% | paradox among Master-now",
    pct(mean(dat$typ_should[dat$typ_now == 1] != 1)), "%\n")

cat("\n========== 3. Where do Master-now respondents move? (row % of 'now') ==========\n")
print(pct(prop.table(table(now = dat$typ_now_f, should = dat$typ_should_f), 1)))

# odds ratios with Wald CIs
or_tab <- function(m) {
  est <- coef(summary(m))
  data.frame(OR = round(exp(est[, 1]), 2),
             lo = round(exp(est[, 1] - 1.96 * est[, 2]), 2),
             hi = round(exp(est[, 1] + 1.96 * est[, 2]), 2),
             p = signif(est[, 4], 3))[-1, ]
}

cat("\n========== 4. Personal endorsement of mastery (Q5 = Master) ~ country + age + gender ==========\n")
dat$master_should <- as.integer(dat$typ_should == 1)
dat$master_now    <- as.integer(dat$typ_now == 1)
m_should <- glm(master_should ~ country + age_num + gender_bin, family = binomial, data = dat)
print(or_tab(m_should))
cat("\n-- for contrast: perceived mastery (Q4 = Master) ~ country + age + gender --\n")
m_now <- glm(master_now ~ country + age_num + gender_bin, family = binomial, data = dat)
print(or_tab(m_now))

cat("\n========== 5. Who holds the paradox? (among Master-now respondents: should != Master) ==========\n")
mn <- subset(dat, typ_now == 1)
mn$paradox <- as.integer(mn$typ_should != 1)
cat("n Master-now =", nrow(mn), "\n")
m_par <- glm(paradox ~ country + age_num + gender_bin, family = binomial, data = mn)
print(or_tab(m_par))
cat("\nRelational-belief score by paradox status (Master-now respondents):\n")
print(round(tapply(mn$relational, mn$paradox, mean), 3))
print(t.test(relational ~ paradox, data = mn))

cat("\n========== 6. Language of administration ==========\n")
for (v in c("master_now", "master_should")) {
  cat("--", v, "--\n")
  print(pct(tapply(dat[[v]], dat$country, mean)))
  eng <- droplevels(subset(dat, language == "English"))
  spa <- droplevels(subset(dat, language == "Spanish"))
  cat("within English (CA/NL/SE): p =", signif(chisq.test(table(eng$country, eng[[v]]))$p.value, 3),
      "| within Spanish (PA/ES): p =", signif(chisq.test(table(spa$country, spa[[v]]))$p.value, 3), "\n")
  ml <- glm(as.formula(paste(v, "~ language")), family = binomial, data = dat)
  mc <- glm(as.formula(paste(v, "~ country")),  family = binomial, data = dat)
  m0 <- glm(as.formula(paste(v, "~ 1")),        family = binomial, data = dat)
  cat("share of country-model deviance reduction captured by language alone:",
      pct((deviance(m0) - deviance(ml)) / (deviance(m0) - deviance(mc))), "% | country beyond language: p =",
      signif(anova(ml, mc, test = "Chisq")[2, "Pr(>Chi)"], 3), "\n")
}

cat("\n========== 7. Robustness: excluding low-quality respondents ==========\n")
cl <- subset(dat, !flag_lowqual)
print(do.call(rbind, lapply(split(cl, cl$country), function(x) data.frame(
  n = nrow(x), master_now = pct(mean(x$typ_now == 1)), master_should = pct(mean(x$typ_should == 1)),
  paradox_among_master_now = pct(mean(x$typ_should[x$typ_now == 1] != 1))))))
cat("Pooled McNemar (clean):", round(mcnemar.test(table(cl$typ_now == 1, cl$typ_should == 1))$statistic, 1), "\n")
print(or_tab(glm(master_should ~ country + age_num + gender_bin, family = binomial, data = cl)))

cat("\n========== 8. Full six-by-six test: do perceived and ideal role distributions differ? ==========\n")
# Stuart-Maxwell test of marginal homogeneity for a paired k x k table
stuart_maxwell <- function(now, should) {
  t <- table(factor(now, 1:6), factor(should, 1:6))
  k <- nrow(t); d <- (rowSums(t) - colSums(t))[-k]
  S <- -(t + t(t)); diag(S) <- rowSums(t) + colSums(t) - 2 * diag(t)
  S <- S[-k, -k]
  stat <- as.numeric(t(d) %*% solve(S) %*% d)
  c(chi2 = round(stat, 1), df = k - 1, p = signif(pchisq(stat, k - 1, lower.tail = FALSE), 3))
}
sm <- rbind(t(sapply(split(dat, dat$country), function(x) stuart_maxwell(x$typ_now, x$typ_should))),
            Pooled = stuart_maxwell(dat$typ_now, dat$typ_should))
print(sm)
cat("\nNet shift, ideal minus perceived (percentage points), by country:\n")
shift <- pct(prop.table(table(dat$country, dat$typ_should_f), 1)) - pct(prop.table(table(dat$country, dat$typ_now_f), 1))
print(round(rbind(shift, Pooled = pct(prop.table(table(dat$typ_should_f))) - pct(prop.table(table(dat$typ_now_f)))), 1))
cat("\nDoes the direction of change differ by country? (change category ~ country, among those who change)\n")
ch <- subset(dat, typ_now != typ_should)
ch$direction <- factor(ifelse(ch$typ_should %in% 4:5, "toward Guardian/Partner",
                        ifelse(ch$typ_should == 1, "toward Master", "other")))
print(pct(prop.table(table(ch$country, ch$direction), 1)))
print(chisq.test(table(ch$country, ch$direction)))

cat("\n========== 9. Belief scale vs perceived and ideal role (eta squared) ==========\n")
for (v in c("typ_now_f", "typ_should_f")) {
  a <- anova(lm(as.formula(paste("relational ~", v)), dat))
  cat(v, ": F =", round(a[1, "F value"], 2), " p =", signif(a[1, "Pr(>F)"], 3),
      " eta2 =", round(a[1, "Sum Sq"] / sum(a[, "Sum Sq"]), 4), "\n")
  print(round(tapply(dat$relational, dat[[v]], mean), 3))
}

cat("\n========== 10. Checks against over-interpretation ==========\n")
cat("-- (a) Overall country effect on mastery as ideal (LR test, 5 df) --\n")
m_should0 <- glm(master_should ~ age_num + gender_bin, family = binomial, data = dat)
print(anova(m_should0, m_should, test = "Chisq"))
cat("-- (b) Predicted P(mastery as ideal): age, gender vs country ranges --\n")
nd <- expand.grid(country = factor("Canada", levels(dat$country)), age_num = c(1, 6), gender_bin = c(0, 1))
print(cbind(nd, p = round(predict(m_should, nd, type = "response"), 3)))
nc <- data.frame(country = factor(levels(dat$country), levels(dat$country)), age_num = 3, gender_bin = 0)
print(cbind(nc, p = round(predict(m_should, nc, type = "response"), 3)))
cat("-- (c) Net change in mastery (now minus should), 95% CI, per country --\n")
print(do.call(rbind, lapply(split(dat, dat$country), function(x) {
  d <- x$master_now - x$master_should; se <- sd(d) / sqrt(length(d))
  round(100 * c(net = mean(d), lo = mean(d) - 1.96 * se, hi = mean(d) + 1.96 * se, changed_answer = mean(x$typ_now != x$typ_should)), 1)
})))
cat("-- (d) Are changes directional or random? (random differentiation, e.g. from question order, gives symmetric changes) --\n")
t6 <- table(dat$typ_now, dat$typ_should)
print(mcnemar.test(t6))   # Bowker test of symmetry
cat("changes away from Master:", sum(t6[1, -1]), "| changes to Master:", sum(t6[-1, 1]), "\n")
cat("-- (e) Does the size of the gap depend on questionnaire language? --\n")
dat$gap_i <- dat$master_now - dat$master_should
print(round(100 * tapply(dat$gap_i, dat$language, mean), 1))
print(anova(lm(gap_i ~ 1, dat), lm(gap_i ~ language, dat)))
cat("country beyond language:\n"); print(anova(lm(gap_i ~ language, dat), lm(gap_i ~ country, dat)))


cat("\n========== 11. Is Canada's gap actually DIFFERENT from the others, or just non-significant? ==========\n")
# Reading six separate within-country tests as "significant in 5, absent in Canada"
# confuses a difference in significance with a significant difference. These tests
# compare the gap BETWEEN countries directly, on the per-respondent difference
# gap_i = (now == Master) - (should == Master), which takes values -1, 0, 1.
cat("-- (a) Does the gap differ across countries at all? --\n")
print(anova(lm(gap_i ~ 1, dat), lm(gap_i ~ country, dat)))

cat("-- (b) Canada vs each other country (Welch t on gap_i, and the difference in pp) --\n")
ca_gap <- subset(dat, country == "Canada")$gap_i
others <- setdiff(levels(dat$country), "Canada")
cmp <- do.call(rbind, lapply(others, function(cn) {
  x <- subset(dat, country == cn)$gap_i
  tt <- t.test(x, ca_gap)
  data.frame(country = cn,
             gap_pp = round(100 * mean(x), 1), canada_pp = round(100 * mean(ca_gap), 1),
             diff_pp = round(100 * (mean(x) - mean(ca_gap)), 1),
             lo = round(100 * tt$conf.int[1], 1), hi = round(100 * tt$conf.int[2], 1),
             p = signif(tt$p.value, 3))
}))
cmp$p_holm <- signif(p.adjust(cmp$p, "holm"), 3)
print(cmp, row.names = FALSE)
cat("(diff_pp > 0 means a LARGER gap than Canada; the CI is for that difference.\n")
cat(" p_holm corrects for the five comparisons. Where the CI includes 0, Canada is\n")
cat(" not distinguishable from that country and 'absent in Canada' is not supported.)\n")
saveRDS(list(by_country = by_c, m_should = m_should, m_now = m_now, m_paradox = m_par, stuart_maxwell = sm),
        "hnr_mastery_paradox.rds")
cat("\nSaved: hnr_mastery_paradox.rds\n")
