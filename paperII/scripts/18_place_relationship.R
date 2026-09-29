# ============================================================
# 18_place_relationship.R
# Two survey blocks not yet used: how respondents relate to their OWN
# favourite natural place (Q7-Q10, four ordered stances each) and whether
# that place changes and whether the change is a form of communication
# (Q12). Questions:
#
#   1. Do these five items form a scale, or a facet the paper can use?
#   2. Are they the personal counterpart of the societal role item?
#   3. Do they enrich the latents already in the paper (motives, agency)?
#   4. Do they change what predicts the role people want?
#
# All items are ordinal 1-4 and are treated as such (polychoric
# correlations, WLSMV). Requires: 01, 13 already run.
# ============================================================

suppressMessages({ library(psych); library(lavaan); library(nnet) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mot <- names(g); for (x in mot) dat[[x]] <- rowMeans(dat[, g[[x]], drop = FALSE], na.rm = TRUE)
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
typ <- c("Master","Manager","User","Guardian","Partner","Object")
dat$role_ideal <- factor(dat$typ_should, 1:6, typ)
dat$role_now   <- factor(dat$typ_now, 1:6, typ)
dat$master_should <- as.integer(dat$typ_should == 1)
dat$agency_mean <- rowMeans(dat[, grep("^ag_now_", names(dat))], na.rm = TRUE)

cat("========== 1. Do the five items form a scale? ==========\n")
cat("Response distributions (%, options 1-4):\n")
print(round(sapply(mp, function(v) 100 * prop.table(table(factor(dat[[v]], 1:4)))), 1))
pc <- polychoric(dat[, mp])$rho
cat("\nPolychoric correlations:\n"); print(round(pc, 2))
cat("\nalpha (ordinal, from polychoric):",
    round(psych::alpha(pc, check.keys = FALSE)$total$raw_alpha, 3), "\n")
fa1 <- fa(pc, nfactors = 1, n.obs = nrow(dat), fm = "ml")
cat("one-factor loadings:", paste(mp, round(fa1$loadings[, 1], 2), collapse = "  "), "\n")
cat("parallel analysis (polychoric) suggests:",
    fa.parallel(pc, n.obs = nrow(dat), fa = "fa", plot = FALSE)$nfact, "factor(s)\n")
cat("(A useful scale wants alpha of about .70 and loadings above .40 on every\n")
cat(" item; the four stances are alternatives to one another, not indicators.)\n")

cat("\n========== 2. Is the personal stance the counterpart of the societal role? ==========\n")
cat("Q7 -- control over MY place (1 = depends entirely on my will .. 4 = it shapes me)\n")
cat("by the role people SEE now and the role they WANT:\n")
cat("  seen now : "); print(round(tapply(dat$mp_emancipation, dat$role_now,   mean, na.rm = TRUE), 2))
cat("  wanted   : "); print(round(tapply(dat$mp_emancipation, dat$role_ideal, mean, na.rm = TRUE), 2))
for (v in c("role_now", "role_ideal")) {
  a <- anova(lm(as.formula(paste("mp_emancipation ~", v)), dat))
  cat(sprintf("  eta2 (%s) = %.4f, p = %.3g\n", v, a[1,"Sum Sq"]/sum(a[,"Sum Sq"]), a[1,"Pr(>F)"]))
}
cat("Spearman of each me-place item with the 12-item control battery:\n")
print(round(sapply(mp, function(v) cor(dat[[v]], dat$control_mean, method = "spearman",
                                       use = "complete.obs")), 3))

cat("\n========== 3. Do they enrich the existing latents? ==========\n")
cc <- sapply(c(mot, "agency_mean"), function(x) sapply(mp, function(v)
  cor(dat[[v]], dat[[x]], method = "spearman", use = "complete.obs")))
print(round(cc, 3))
cat("(Which of the motive and agency scales each place stance tracks.)\n")

cat("\n========== 4. Do they change what predicts the role people want? ==========\n")
d <- dat[complete.cases(dat[, c("role_ideal", mot, mp, "country", "age_num", "gender_bin")]), ]
d$role_ideal <- relevel(d$role_ideal, ref = "Manager")
for (v in mp) d[[v]] <- factor(d[[v]])
base <- paste("role_ideal ~", paste(mot, collapse = " + "), "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na")
m0 <- multinom(as.formula(base), d, trace = FALSE)
r2 <- function(m) 1 - m$deviance / multinom(role_ideal ~ 1, d, trace = FALSE)$deviance
res <- data.frame(added = "(motives + demographics)", LR = NA, df = NA, p = NA, pseudoR2 = round(r2(m0), 4))
for (v in mp) {
  m1 <- multinom(as.formula(paste(base, "+", v)), d, trace = FALSE)
  a <- anova(m0, m1)
  res <- rbind(res, data.frame(added = v, LR = round(a[2, 6], 1), df = a[2, 5],
                               p = signif(a[2, 7], 3), pseudoR2 = round(r2(m1), 4)))
}
mall <- multinom(as.formula(paste(base, "+", paste(mp, collapse = " + "))), d, trace = FALSE)
a <- anova(m0, mall)
res <- rbind(res, data.frame(added = "all five together", LR = round(a[2, 6], 1),
                             df = a[2, 5], p = signif(a[2, 7], 3),
                             pseudoR2 = round(r2(mall), 4)))
print(res, row.names = FALSE)

cat("\n========== 5. Do the stances differ across countries? ==========\n")
for (v in mp) {
  tt <- table(dat$country, dat[[v]])
  cat(sprintf("  %-16s chi2 p = %-9.3g Cramer V = %.3f\n", v,
              suppressWarnings(chisq.test(tt))$p.value,
              sqrt(suppressWarnings(chisq.test(tt))$statistic / (sum(tt) * (min(dim(tt)) - 1)))))
}

cat("\n========== 6. Can the five items be used as ONE latent? ==========\n")
f1 <- cfa(paste("place =~", paste(mp, collapse = " + ")), dat, ordered = mp, estimator = "WLSMV")
print(round(fitmeasures(f1, c("chisq","df","cfi","tli","rmsea","srmr")), 3))
e2 <- fa(pc, nfactors = 2, n.obs = nrow(dat), rotate = "oblimin", fm = "ml")
L2 <- unclass(e2$loadings); L2[abs(L2) < .25] <- NA
cat("\ntwo-factor solution (blank = below .25), factor correlation",
    round(e2$Phi[1, 2], 2), ":\n"); print(round(L2, 2))
cat("(A single factor fits well. Parallel analysis prefers two, but they correlate\n")
cat(" .64 and split by content -- who acts on the place (emancipation, agency)\n")
cat(" versus what passes between them (dialogue, learning, time) -- so one general\n")
cat(" latent with two facets is the defensible reading.)\n")

cat("\n========== 7. Personal versus societal ==========\n")
tv <- function(x, y) { t <- table(x, y)
  sqrt(suppressWarnings(chisq.test(t))$statistic / (sum(t) * (min(dim(t)) - 1))) }
cat(sprintf("Q7 (control over MY place) x role SEEN now:   Cramer V = %.3f\n", tv(dat$mp_emancipation, dat$typ_now)))
cat(sprintf("Q7 (control over MY place) x role WANTED:     Cramer V = %.3f\n", tv(dat$mp_emancipation, dat$typ_should)))
cat(sprintf("Q4 societal control battery x role SEEN now:  Spearman   = %.3f\n",
            cor(dat$control_mean, dat$typ_now, method = "spearman", use = "complete.obs")))
cat(sprintf("Q4 societal control battery x role WANTED:    Spearman   = %.3f\n",
            cor(dat$control_mean, dat$typ_should, method = "spearman", use = "complete.obs")))
cat("(The societal battery tracks the role people SEE; the personal stance tracks\n")
cat(" the role people WANT. What people want appears to be grounded in how they\n")
cat(" relate to a particular place, not in beliefs about control in general.)\n")

cat("\n========== 8. Do the motives survive once the place stances are in? ==========\n")
dd <- dat[complete.cases(dat[, c("role_ideal", mot, mp, "country", "age_num", "gender_bin")]), ]
dd$role_ideal <- relevel(dd$role_ideal, ref = "Manager")
for (v in mp) dd[[v]] <- factor(dd[[v]])
dm <- "+ country + age_num + gender_bin + edu_primary + edu_higher + edu_na"
ms <- multinom(as.formula(paste("role_ideal ~", paste(mp, collapse = " + "), dm)), dd, trace = FALSE)
mb <- multinom(as.formula(paste("role_ideal ~", paste(c(mp, mot), collapse = " + "), dm)), dd, trace = FALSE)
an <- anova(ms, mb); n0 <- multinom(role_ideal ~ 1, dd, trace = FALSE)$deviance
cat(sprintf("stances + demographics -> + motives: LR chi2(%d) = %.1f, p = %.3g\n", an[2, 5], an[2, 6], an[2, 7]))
cat(sprintf("pseudo-R2: %.4f -> %.4f\n", 1 - ms$deviance / n0, 1 - mb$deviance / n0))
cat("(Motives keep an independent contribution, but a smaller one than the place\n")
cat(" stances make. The stances are the personal-level counterpart of the role item,\n")
cat(" so their strength is partly the same construct measured twice -- convergent\n")
cat(" validity, not an independent explanation. The motives are the less circular\n")
cat(" of the two predictors.)\n")

cat("\n========== 9. One structural model: motives and the place latent together ==========\n")
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd <- paste0("c_", others)
sm <- paste0(
 'restorative =~ ', paste(g$restorative, collapse = " + "), '
  dialogic    =~ ', paste(g$dialogic,    collapse = " + "), '
  serviced    =~ ', paste(g$serviced,    collapse = " + "), '
  place       =~ ', paste(mp, collapse = " + "), '
  master_should ~ restorative + dialogic + serviced + place + age_num + gender_bin + edu_primary + edu_higher + edu_na + ',
  paste(cd, collapse = " + "))
dat9 <- dat[, setdiff(names(dat), mot)]   # observed scores would collide with the latent names
fs <- sem(sm, dat9, estimator = "WLSMV", ordered = c("master_should", mp))
print(round(fitmeasures(fs, c("chisq","df","cfi","tli","rmsea","srmr")), 3))
ps <- standardizedSolution(fs); ps <- ps[ps$op == "~" & ps$lhs == "master_should" & !grepl("^c_|^edu", ps$rhs), ]
print(data.frame(predictor = ps$rhs, beta = round(ps$est.std, 3), se = round(ps$se, 3),
                 p = signif(ps$pvalue, 3)), row.names = FALSE)
cat("R2 for wanting mastery:", round(inspect(fs, "r2")[["master_should"]], 4),
    "(motives + demographics alone: .1006 in script 15)\n")
cat("latent correlations with the place latent:\n")
print(round(lavInspect(fs, "cor.lv")["place", c("restorative", "dialogic", "serviced")], 2))

saveRDS(list(polychoric = pc, cross = cc, prediction = res, cfa = f1, structural = fs), "hnr_place_relationship.rds")
cat("\nSaved: hnr_place_relationship.rds\n")
