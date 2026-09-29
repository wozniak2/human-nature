# ============================================================
# 17_scenarios_and_roles.R
# Two blocks of the survey state positions on the same continuum the role
# item asks about, but through scenarios rather than narratives:
#
#   a destroyed natural place (Q11)  not a message / technology prevents it /
#     humans should heed its signals / nature is a partner sending a message /
#     humans always lose to natural forces
#   climate change (Q15)  it is not happening / real and technology can stop it /
#     real and cannot be stopped / real and will end humanity
#
# These are mutually exclusive stances, not indicators of one factor
# (alpha .59 and .30), so they are never summed. Used as items they test
# whether the forced-choice typology behaves as it should against the same
# idea asked a different way, and whether they add to the motives.
#
# Requires: 01, 13 already run. Packages: nnet.
# ============================================================

suppressMessages(library(nnet))
dat <- readRDS("hnr_data.rds")
re  <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mot <- names(g); for (x in mot) dat[[x]] <- rowMeans(dat[, g[[x]], drop = FALSE], na.rm = TRUE)

sc <- c("sc_not_message", "sc_tech_prevents", "sc_heed_signals",
        "sc_nature_partner", "sc_humans_lose")
cl <- c("cl_denial", "cl_tech_stops", "cl_cannot_stop", "cl_extinction")
items <- c(sc, cl)
typ <- c("Master","Manager","User","Guardian","Partner","Object")
dat$role_ideal <- factor(dat$typ_should, 1:6, typ)
dat$master_should <- as.integer(dat$typ_should == 1)

cat("========== 1. Mean agreement with each stance, by the role people want ==========\n")
tb <- t(sapply(items, function(v) round(tapply(dat[[v]], dat$role_ideal, mean, na.rm = TRUE), 2)))
print(tb)
cat("\neta2 of the ideal role on each stance:\n")
for (v in items) {
  a <- anova(lm(as.formula(paste(v, "~ role_ideal")), dat))
  cat(sprintf("  %-18s eta2 %.4f  p %.3g\n", v, a[1,"Sum Sq"]/sum(a[,"Sum Sq"]), a[1,"Pr(>F)"]))
}
cat("(If the typology means anything, the technological-control stances should\n")
cat(" peak among those wanting Master, and the humans-are-subject stances among\n")
cat(" those wanting Object.)\n")

cat("\n========== 2. Do the stances add beyond the motives? (mastery as ideal) ==========\n")
d <- dat[complete.cases(dat[, c("master_should", mot, items, "country","age_num","gender_bin")]), ]
f0 <- glm(as.formula(paste("master_should ~", paste(mot, collapse=" + "),
                           "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), binomial, d)
f1 <- glm(as.formula(paste("master_should ~", paste(c(mot, items), collapse=" + "),
                           "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), binomial, d)
print(anova(f0, f1, test = "Chisq"))
s <- coef(summary(f1))
s <- s[rownames(s) %in% items, , drop = FALSE]
print(data.frame(stance = rownames(s), OR = round(exp(s[,1]), 2),
                 lo = round(exp(s[,1] - 1.96*s[,2]), 2),
                 hi = round(exp(s[,1] + 1.96*s[,2]), 2),
                 p = signif(s[,4], 3)), row.names = FALSE)

cat("\n========== 3. And beyond the motives for the whole typology? ==========\n")
d$role_ideal <- relevel(d$role_ideal, ref = "Manager")
m0 <- multinom(as.formula(paste("role_ideal ~", paste(mot, collapse=" + "),
                                "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), d, trace = FALSE)
m1 <- multinom(as.formula(paste("role_ideal ~", paste(c(mot, items), collapse=" + "),
                                "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")), d, trace = FALSE)
print(anova(m0, m1))
cat(sprintf("McFadden pseudo-R2: %.4f -> %.4f\n",
            1 - m0$deviance / multinom(role_ideal ~ 1, d, trace = FALSE)$deviance,
            1 - m1$deviance / multinom(role_ideal ~ 1, d, trace = FALSE)$deviance))

saveRDS(list(by_role = tb, master = f1, multinom = m1), "hnr_scenarios.rds")
cat("\nSaved: hnr_scenarios.rds\n")
