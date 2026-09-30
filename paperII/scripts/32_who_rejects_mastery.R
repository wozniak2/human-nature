# ============================================================
# 32_who_rejects_mastery.R
# Who rejects the mastery they see? The gap within each person.
#
# The gap (script 08) is a count: 480 people move away from Master, 200 toward
# it. This script asks what separates the movers from the stayers, which ties the
# gap to place agency within each person rather than across the sample:
#
#   1. the people who SEE Master: how many keep it, how many reject it, and
#      where the rejecters go
#   2. among them, who rejects mastery (logistic regression): place agency,
#      the restful and dialogic motives, societal control, age, gender,
#      education, country; odds ratios and what they mean in probabilities
#   3. which facet of place agency carries it (independence vs communication)
#   4. the reverse move: among the people who do NOT see Master, who wants it
#   5. everyone together: the wanted role given the seen role (ordered logit
#      with the seen role as a factor), i.e. which predictors go with moving
#      away from Master whatever one starts from
#   6. the same main model without respondents flagged for speeding or
#      straight-lining
#
# Odds ratios are per standard deviation. Cross-sectional: "rejecting" means
# choosing a different role for how it should be than for how it is now.
#
# Requires: 01, 04, 13 already run. Packages: MASS.
# ============================================================

suppressMessages(library(MASS))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
roles <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
zs  <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
dat$plc_z   <- zs(rowMeans(dat[, mp]))
dat$indep_z <- zs(rowMeans(dat[, c("mp_emancipation", "mp_agency")]))
dat$comm_z  <- zs(rowMeans(dat[, c("mp_dialogue", "mp_learning", "mp_time")]))
dat$rest_z  <- zs(rowMeans(dat[, g$restorative])); dat$dia_z <- zs(rowMeans(dat[, g$dialogic]))
dat$ctl_z   <- zs(dat$control_mean)
cov_txt <- "age_num + gender_bin + edu_primary + edu_higher + edu_na + country"
d <- dat[complete.cases(dat[, c("typ_now", "typ_should", "age_num", "gender_bin", "plc_z", "rest_z", "dia_z", "ctl_z")]), ]
cat("respondents:", nrow(d), "\n")

or_tab <- function(f, keep) {
  s <- coef(summary(f))[keep, , drop = FALSE]
  data.frame(predictor = keep, OR = round(exp(s[, 1]), 2),
             lo = round(exp(s[, 1] - 1.96 * s[, 2]), 2), hi = round(exp(s[, 1] + 1.96 * s[, 2]), 2),
             p = signif(s[, ncol(s)], 2), row.names = NULL)
}
mcf <- function(f, f0) 1 - as.numeric(logLik(f)) / as.numeric(logLik(f0))
pp  <- function(f, data, var) {   # average predicted probability at the 10th and 90th percentile of var
  q <- quantile(data[[var]], c(.1, .9), na.rm = TRUE)
  sapply(q, function(v) { nd <- data; nd[[var]] <- v; mean(predict(f, nd, type = "response")) })
}
main <- c("plc_z", "rest_z", "dia_z", "ctl_z")
lab  <- c(plc_z = "place agency", rest_z = "restful motive", dia_z = "dialogic motive", ctl_z = "societal control",
          indep_z = "independence facet", comm_z = "communication facet")

cat("\n========== 1. The people who see Master ==========\n")
m <- d[d$typ_now == 1, ]
m$reject <- as.integer(m$typ_should != 1)
cat(sprintf("see Master: %d | keep Master: %d (%.1f%%) | want another role: %d (%.1f%%)\n",
            nrow(m), sum(m$reject == 0), 100 * mean(m$reject == 0), sum(m$reject), 100 * mean(m$reject)))
dest <- table(factor(m$typ_should, 1:6, roles))
cat("\nwhere they go (role wanted by people who see Master), with their mean place agency (z):\n")
print(data.frame(role_wanted = roles, n = as.vector(dest), share = round(100 * as.vector(dest) / nrow(m), 1),
                 place_agency_mean = round(as.vector(tapply(m$plc_z, factor(m$typ_should, 1:6, roles), mean)), 2)), row.names = FALSE)
cat("\nshare rejecting, by country:\n")
print(round(100 * tapply(m$reject, m$country, mean), 1))

cat("\n========== 2. Who rejects the mastery they see? (logistic regression, n = ", nrow(m), ") ==========\n", sep = "")
f0 <- glm(as.formula(paste("reject ~", cov_txt)), binomial, m)
f1 <- glm(as.formula(paste("reject ~ plc_z +", cov_txt)), binomial, m)
f2 <- glm(as.formula(paste("reject ~", paste(main, collapse = " + "), "+", cov_txt)), binomial, m)
fn <- glm(reject ~ 1, binomial, m)
t2 <- or_tab(f2, main); t2$predictor <- lab[t2$predictor]; print(t2, row.names = FALSE)
cat(sprintf("\nfit (McFadden pseudo-R2): covariates only %.3f | + place agency %.3f | + motives and control %.3f\n",
            mcf(f0, fn), mcf(f1, fn), mcf(f2, fn)))
cat(sprintf("likelihood-ratio test for place agency over the covariates: chi2(1) = %.1f, p = %.2g\n",
            anova(f0, f1, test = "Chisq")[2, "Deviance"], anova(f0, f1, test = "Chisq")[2, "Pr(>Chi)"]))
p2 <- pp(f2, m, "plc_z")
cat(sprintf("probability of rejecting mastery at low vs high place agency (10th vs 90th percentile): %.1f%% vs %.1f%%\n", 100 * p2[1], 100 * p2[2]))
pr <- pp(f2, m, "rest_z"); pd <- pp(f2, m, "dia_z")
cat(sprintf("same for the restful motive: %.1f%% vs %.1f%% | dialogic motive: %.1f%% vs %.1f%%\n", 100 * pr[1], 100 * pr[2], 100 * pd[1], 100 * pd[2]))

cat("\n========== 3. Which facet of place agency carries it? ==========\n")
f3 <- glm(as.formula(paste("reject ~ indep_z + comm_z + rest_z + dia_z + ctl_z +", cov_txt)), binomial, m)
t3 <- or_tab(f3, c("indep_z", "comm_z")); t3$predictor <- lab[t3$predictor]; print(t3, row.names = FALSE)

cat("\n========== 4. The reverse move: who wants the mastery they do NOT see? ==========\n")
o <- d[d$typ_now != 1, ]
o$adopt <- as.integer(o$typ_should == 1)
cat(sprintf("do not see Master: %d | want Master: %d (%.1f%%)\n", nrow(o), sum(o$adopt), 100 * mean(o$adopt)))
cat("where they start (role seen by those who move to Master):\n")
print(table(factor(o$typ_now[o$adopt == 1], 2:6, roles[2:6])))
f4 <- glm(as.formula(paste("adopt ~", paste(main, collapse = " + "), "+ factor(typ_now) +", cov_txt)), binomial, o)
t4 <- or_tab(f4, main); t4$predictor <- lab[t4$predictor]; print(t4, row.names = FALSE)
p4 <- pp(f4, o, "plc_z")
cat(sprintf("probability of moving to Master at low vs high place agency (10th vs 90th percentile): %.1f%% vs %.1f%%\n", 100 * p4[1], 100 * p4[2]))
cat("(the seen role is controlled, because the chance of moving to Master depends on where one starts)\n")

cat("\n========== 5. Everyone: the wanted role given the seen role (ordered logit) ==========\n")
d$w <- factor(d$typ_should, levels = 1:6, ordered = TRUE)
f5 <- polr(as.formula(paste("w ~ factor(typ_now) +", paste(main, collapse = " + "), "+", cov_txt)), d, Hess = TRUE)
f5b <- polr(as.formula(paste("w ~", paste(main, collapse = " + "), "+", cov_txt)), d, Hess = TRUE)
s5 <- coef(summary(f5))[main, ]; s5b <- coef(summary(f5b))[main, ]
t5 <- data.frame(predictor = lab[main],
                 OR_given_seen_role = sprintf("%.2f [%.2f, %.2f]", exp(s5[, 1]), exp(s5[, 1] - 1.96 * s5[, 2]), exp(s5[, 1] + 1.96 * s5[, 2])),
                 OR_without_seen_role = sprintf("%.2f", exp(s5b[, 1])), row.names = NULL)
print(t5, row.names = FALSE)
cat("(An odds ratio above 1 given the seen role means the predictor goes with wanting a role further from Master\n")
cat(" than the one people see, i.e. with the shift itself, not only with where people stand.)\n")

cat("\n========== 6. Without respondents flagged for speeding or straight-lining ==========\n")
mc <- m[!m$flag_lowqual, ]
f6 <- glm(as.formula(paste("reject ~", paste(main, collapse = " + "), "+", cov_txt)), binomial, mc)
t6 <- or_tab(f6, main); t6$predictor <- lab[t6$predictor]; cat("n =", nrow(mc), "\n"); print(t6, row.names = FALSE)

saveRDS(list(n_master_seers = nrow(m), destinations = dest, reject_model = t2, facets = t3, adopt_model = t4,
             conditional = t5, clean = t6, prob_reject_plc = p2, prob_adopt_plc = p4), "hnr_who_rejects.rds")
cat("\nSaved: hnr_who_rejects.rds\n")
