# ============================================================
# 22_geography.R
# Can geography do anything in this model? Three separate questions, because
# geography cannot be a mediator: country and residence are fixed before any
# of the psychological variables, and a mediator has to be caused by the
# thing it mediates. It can be (1) a cause upstream of the chain, whose effect
# the constructs might carry, (2) a moderator of the relations within the
# chain, or (3) a predictor in its own right.
#
# Geography in this survey: country (six samples), and a self-reported
# residence item (urban or suburban / rural). The wording is identical across
# versions, but it records perceived residence, not an official classification
# (35% of Dutch respondents call themselves rural). Respondent IP addresses
# exist in the raw files but are not used: they were not collected for this
# purpose and geocoding them would expose respondents.
#
# Requires: 01, 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
d <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
for (x in names(g)) d[[x]] <- rowMeans(d[, g[[x]], drop = FALSE], na.rm = TRUE)
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
d$plcm  <- rowMeans(d[, mp]); d$ctl <- d$control_mean
d$mw <- as.integer(d$typ_should == 1); d$mn <- as.integer(d$typ_now == 1)
d$rural <- ifelse(d$residence_raw == 2, 1, ifelse(d$residence_raw == 1, 0, NA))
dd <- d[complete.cases(d[, c("mw", "mn", "age_num", "gender_bin")]), ]
lr <- function(m0, m1) { a <- anova(m0, m1, test = "Chisq")
  c(chi2 = round(a$Deviance[2], 1), df = a$Df[2], p = signif(a[2, "Pr(>Chi)"], 3)) }

cat("========== 1. Are the country differences in wanting mastery carried by the constructs? ==========\n")
m1 <- glm(mw ~ age_num + gender_bin, binomial, dd)
m2 <- update(m1, . ~ . + restorative + dialogic + serviced)
m3 <- update(m2, . ~ . + plcm)
m4 <- update(m3, . ~ . + ctl)
step <- list("age + gender" = m1, "+ motives" = m2, "+ place agency" = m3, "+ societal control" = m4)
print(t(sapply(names(step), function(s) lr(step[[s]], update(step[[s]], . ~ . + country)))))
cat("(Omnibus test of country, five degrees of freedom, once each block is controlled. If the\n")
cat(" constructs carried the country effect, chi2 would fall toward zero. It does not: it\n")
cat(" barely moves with the motives and RISES when place agency is added.)\n")
cat("\ncountry means: place agency (1-4) and % wanting mastery\n")
print(round(cbind(place_agency = tapply(d$plcm, d$country, mean),
                  wants_mastery_pct = 100 * tapply(d$mw, d$country, mean)), 2))
cat("(Spain has the highest place agency AND the highest wish for mastery -- the opposite of\n")
cat(" the individual-level relation, so place agency acts as a suppressor of the country\n")
cat(" effect, not a mediator of it.)\n")
dd$country <- relevel(dd$country, "Spain")
ms <- glm(mw ~ country + age_num + gender_bin + restorative + dialogic + serviced + plcm, binomial, dd)
cat("\nWith every construct controlled, log-odds of wanting mastery relative to Spain:\n")
print(round(summary(ms)$coefficients[grep("^country", rownames(summary(ms)$coefficients)), c(1, 4)], 3))
cat("(Spain differs from every other sample. Panama reads identical Spanish wording, so this\n")
cat(" is not simply a translation effect.)\n")

cat("\n========== 2. Does country change how the constructs relate to wanting mastery? ==========\n")
dd$country <- relevel(dd$country, "Canada")
base <- glm(mw ~ country + age_num + gender_bin + restorative + dialogic + serviced + plcm + ctl, binomial, dd)
for (v in c("restorative", "dialogic", "serviced", "plcm", "ctl")) {
  r <- lr(base, update(base, as.formula(paste(". ~ . +", v, ":country"))))
  cat(sprintf("  %-12s x country   chi2(%d) = %5.1f   p = %.3g\n", v, r["df"], r["chi2"], r["p"]))
}
cat("(None reaches p < .05. The mechanism looks the same in every sample.)\n")

cat("\nCan slopes be compared? Place-agency invariance across countries (MLR):\n")
mod <- paste("plc =~", paste(mp, collapse = " + "))
fs <- list(configural = cfa(mod, d, group = "country", estimator = "MLR"),
           metric = cfa(mod, d, group = "country", estimator = "MLR", group.equal = "loadings"),
           scalar = cfa(mod, d, group = "country", estimator = "MLR", group.equal = c("loadings", "intercepts")))
t <- t(sapply(fs, function(f) fitmeasures(f, c("cfi", "rmsea", "srmr"))))
print(cbind(round(t, 3), dCFI = round(c(NA, diff(t[, "cfi"])), 4)))
cat("(Loadings are equivalent, so slopes can be compared across countries; intercepts are not,\n")
cat(" so country MEANS on place agency cannot be.)\n")

cat("\n========== 3. Urban versus rural, as reported ==========\n")
print(round(100 * prop.table(table(d$country, d$rural), 1), 1))
res <- do.call(rbind, lapply(c("mw", "mn"), function(y) {
  m <- glm(as.formula(paste(y, "~ rural + country + age_num + gender_bin")), binomial, d)
  s <- summary(m)$coefficients["rural", ]
  data.frame(outcome = ifelse(y == "mw", "wants mastery", "sees mastery"),
             OR = round(exp(s[1]), 2), p = signif(s[4], 3)) }))
cat("\nRural versus urban, adjusted for country, age and gender:\n"); print(res, row.names = FALSE)
for (v in c("restorative", "dialogic", "serviced", "plcm", "ctl")) {
  m <- lm(as.formula(paste(v, "~ rural + country + age_num + gender_bin")), d)
  s <- summary(m)$coefficients["rural", ]; cat(sprintf("  %-12s rural b = %+.3f  p = %.3g\n", v, s[1], s[4]))
}
mr <- glm(mw ~ rural + country + age_num + gender_bin, binomial, d[!is.na(d$rural), ])
cat("\nrural x country interaction on wanting mastery:\n"); print(lr(mr, update(mr, . ~ . + rural:country)))
cat("(Residence as reported predicts nothing and does not vary by country.)\n")
