# ============================================================
# 29_education_check.R
# Education (three harmonised levels, built in 04) as a covariate: what it
# does and does not change.
#
#   1. its distribution by country (the questionnaires offered 9, 8 and 7
#      answer options, harmonised to primary or less / secondary / tertiary, ISCED 2011);
#   2. how it relates to place agency, the motives, societal control and the
#      role people want, adjusted for country, age and gender;
#   3. the central estimates with and without it, on the wanted role
#      (ordered logit), on extremity and on wanting Master;
#   4. whether it moderates the place-agency effect.
#
# From 04 onward every model of the role carries education as two dummies
# (primary or less, tertiary; secondary is the reference) plus a "not stated" flag
# for the 28 respondents who preferred not to answer, so nobody drops out.
# This script compares against the same models without those terms.
#
# Requires: 01, 04, 13 already run. Packages: MASS.
# ============================================================

suppressMessages(library(MASS))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
stopifnot("edu_level" %in% names(dat))

nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
dat$rest <- z(rowMeans(dat[, g$restorative])); dat$dia <- z(rowMeans(dat[, g$dialogic]))
dat$plc  <- z(rowMeans(dat[, mp]));            dat$ctl <- z(dat$control_mean)
dat$ext_should <- as.integer(dat$typ_should %in% c(1, 6))
dat$mw <- as.integer(dat$typ_should == 1)
d <- dat[complete.cases(dat[, c("typ_should", "age_num", "gender_bin", "plc", "rest", "dia", "ctl")]), ]
cat("respondents in the models:", nrow(d), "\n")

cat("\n========== 1. Education by country (harmonised) ==========\n")
lv <- factor(dat$edu_level, 1:3, c("primary or less", "secondary", "tertiary"))
tab <- table(dat$country, lv, useNA = "ifany"); print(tab)
print(round(100 * prop.table(table(dat$country, lv), 1), 0))
cat("not stated:", sum(dat$edu_na), "respondents\n")

cat("\n========== 2. What education goes with (adjusted for country, age, gender) ==========\n")
ed <- "edu_primary + edu_higher + edu_na"
res <- list()
for (v in c("plc", "rest", "dia", "ctl")) {
  m <- lm(as.formula(paste(v, "~ country + age_num + gender_bin +", ed)), d)
  s <- summary(m)$coefficients
  res[[v]] <- data.frame(outcome = v, primary = s["edu_primary", 1], p_primary = s["edu_primary", 4],
                         tertiary = s["edu_higher", 1], p_tertiary = s["edu_higher", 4])
}
r2 <- do.call(rbind, res); r2[, -1] <- round(r2[, -1], 3); rownames(r2) <- NULL
cat("(SD of the construct; reference = secondary)\n"); print(r2)

d$ranked <- factor(d$typ_should, ordered = TRUE)
f1 <- polr(as.formula(paste("ranked ~ plc + rest + dia + ctl + age_num + gender_bin +", ed, "+ country")), d, Hess = TRUE)
s1 <- summary(f1)$coefficients
m_ext <- glm(as.formula(paste("ext_should ~ plc + rest + dia + ctl + age_num + gender_bin +", ed, "+ country")), binomial, d)
cat("\n-- the wanted role and its extremity, education terms --\n")
cat(sprintf("position (ordered logit): primary %+.2f (p = %.3f), tertiary %+.2f (p = %.3f)\n",
    s1["edu_primary", 1], 2 * pnorm(-abs(s1["edu_primary", 3])), s1["edu_higher", 1], 2 * pnorm(-abs(s1["edu_higher", 3]))))
se <- summary(m_ext)$coefficients
cat(sprintf("extremity (logit): primary %+.2f (p = %.3f), tertiary %+.2f (p = %.3f)\n",
    se["edu_primary", 1], se["edu_primary", 4], se["edu_higher", 1], se["edu_higher", 4]))

cat("\n========== 3. The central estimates with and without education ==========\n")
f0 <- polr(ranked ~ plc + rest + dia + ctl + age_num + gender_bin + country, d, Hess = TRUE)
pick <- c("plc", "rest", "dia", "ctl")
cmp <- function(a, b) round(cbind(without = a[pick], with = b[pick]), 3)
cat("-- wanted role, ordered logit (log-odds per SD) --\n")
print(cmp(summary(f0)$coefficients[, 1], s1[, 1]))
m0 <- glm(ext_should ~ plc + rest + dia + ctl + age_num + gender_bin + country, binomial, d)
cat("-- extremity, logit (log-odds per SD) --\n"); print(cmp(coef(m0), coef(m_ext)))
b0 <- glm(mw ~ plc + rest + dia + ctl + age_num + gender_bin + country, binomial, d)
b1 <- glm(as.formula(paste("mw ~ plc + rest + dia + ctl + age_num + gender_bin +", ed, "+ country")), binomial, d)
cat("-- wanting Master, odds ratio per SD --\n"); print(round(exp(cmp(coef(b0), coef(b1))), 3))

cat("\n========== 4. Does education moderate the place-agency effect? ==========\n")
d$edu_num <- ifelse(is.na(d$edu_level), 2, d$edu_level)
fi <- polr(ranked ~ plc * edu_num + rest + dia + ctl + age_num + gender_bin + country, d, Hess = TRUE)
si <- summary(fi)$coefficients
cat(sprintf("place agency x education (linear, 1-3): %+.3f (t = %.2f, p = %.3f)\n", si["plc:edu_num", 1], si["plc:edu_num", 3],
            2 * pnorm(-abs(si["plc:edu_num", 3]))))

saveRDS(list(table = tab, associations = r2, position = s1, extremity = se), "hnr_education_check.rds")
cat("\nSaved: hnr_education_check.rds\n")
