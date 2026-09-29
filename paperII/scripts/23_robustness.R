# ============================================================
# 23_robustness.R
# The attacks a critical referee would make on the central result, and the
# tests that answer them. The central result: the role people WANT for humans
# is anchored in how they relate to a place (place agency) and in what they
# go there for (restorative and dialogic motives), more than the role they SEE.
#
# All models use observed composites so that they run quickly and can be
# resampled; predictors are standardised on the full sample so odds ratios are
# per standard deviation and comparable across subsets.
#
#   1. Question stems differ across countries    -> split by stem group; drop each country
#   2. One survey, one method                    -> add response-style indices
#   3. Place agency is circular with the role     -> rebuild it from communicative items only
#   4. Delta-method mediation intervals are weak  -> bootstrap the indirect effect
#   5. Standardised paths mean little to readers  -> effects in probabilities
#   6. Nothing replicates                         -> estimate in each random half
#   7. Low-quality respondents                    -> exclude the flagged 15%
#
# Requires: 01, 13 already run.
# ============================================================

dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
for (x in names(g)) dat[[x]] <- rowMeans(dat[, g[[x]], drop = FALSE], na.rm = TRUE)
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
dat$plc      <- rowMeans(dat[, mp])
dat$plc_comm <- rowMeans(dat[, c("mp_dialogue", "mp_learning", "mp_time")])   # no control-of-the-place items
dat$ctl      <- dat$control_mean
dat$mw <- as.integer(dat$typ_should == 1); dat$mn <- as.integer(dat$typ_now == 1)
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
for (v in c("restorative", "dialogic", "plc", "plc_comm", "ctl", "ARS", "MRS", "ERS")) dat[[paste0(v, "_z")]] <- zs(dat[[v]])
dat <- dat[complete.cases(dat[, c("mw", "mn", "age_num", "gender_bin")]), ]
stem_announced <- dat$country %in% c("Netherlands", "Sweden", "Poland")   # told a normative question follows
dat$stem <- ifelse(stem_announced, "announced", "not announced")

core <- c("restorative_z", "dialogic_z", "plc_z", "ctl_z")
fit_or <- function(d, y, terms = core, extra = character(0), country = TRUE) {
  rhs <- c(terms, extra, "age_num", "gender_bin", if (country && length(unique(d$country)) > 1) "country")
  m <- glm(as.formula(paste(y, "~", paste(rhs, collapse = " + "))), binomial, d)
  s <- summary(m)$coefficients[terms, , drop = FALSE]
  setNames(c(rbind(round(exp(s[, 1]), 2), signif(s[, 4], 2))),
           c(rbind(sub("_z", "", terms), paste0("p_", sub("_z", "", terms)))))
}
show <- function(label, d, y = "mw", ...) data.frame(variant = label, n = nrow(d), t(fit_or(d, y, ...)), check.names = FALSE)

cat("========== 0. Reference: the pooled model ==========\n")
ref <- rbind(show("wants mastery", dat, "mw"), show("sees mastery", dat, "mn"))
print(ref, row.names = FALSE)
cat("(Odds ratios per standard deviation. Place agency lowers wanting mastery more than seeing it;\n")
cat(" restorative matters for wanting and not for seeing.)\n")

cat("\n========== 1. Question stems: split by stem group, and drop each country in turn ==========\n")
sg <- rbind(show("stem announced (NL, SE, PL)", dat[stem_announced, ]),
            show("stem not announced (CA, ES, PA)", dat[!stem_announced, ]))
print(sg, row.names = FALSE)
base <- glm(mw ~ restorative_z + dialogic_z + plc_z + ctl_z + age_num + gender_bin + country, binomial, dat)
for (v in c("restorative_z", "dialogic_z", "plc_z", "ctl_z")) {
  a <- anova(base, update(base, as.formula(paste(". ~ . +", v, ":stem"))), test = "Chisq")
  cat(sprintf("  %-14s x stem group   chi2(1) = %5.2f   p = %.3g\n", sub("_z", "", v), a$Deviance[2], a[2, "Pr(>Chi)"]))
}
loo <- do.call(rbind, lapply(levels(dat$country), function(cn) show(paste("without", cn), dat[dat$country != cn, ])))
print(loo, row.names = FALSE)
cat("(If the stem wording produced the result, it would differ between the stem groups and\n")
cat(" collapse when Canada, the least contrastive stem, is dropped.)\n")

cat("\n========== 2. Response style added as covariates ==========\n")
print(rbind(show("wants, + ARS MRS ERS", dat, "mw", extra = c("ARS_z", "MRS_z", "ERS_z")),
            show("sees,  + ARS MRS ERS", dat, "mn", extra = c("ARS_z", "MRS_z", "ERS_z"))), row.names = FALSE)

cat("\n========== 3. Place agency without the control items (communicative items only) ==========\n")
cat("Uses dialogue, learning and time; drops emancipation and agency, the two closest in content\n")
cat("to the Master narrative (the place serves my will / rules defined by humans).\n")
alt <- c("restorative_z", "dialogic_z", "plc_comm_z", "ctl_z")
print(rbind(data.frame(variant = "wants, place agency = communicative items", n = nrow(dat),
                       t(fit_or(dat, "mw", alt)), check.names = FALSE),
            data.frame(variant = "sees,  place agency = communicative items", n = nrow(dat),
                       t(fit_or(dat, "mn", alt)), check.names = FALSE)), row.names = FALSE)
cat("correlation of the two place-agency versions:", round(cor(dat$plc, dat$plc_comm), 3), "\n")

cat("\n========== 4. Mediation, bootstrapped (motive -> place agency -> wanting mastery) ==========\n")
set.seed(20260929)
med_once <- function(d, x, m = "plc_z") {
  a <- coef(lm(as.formula(paste(m, "~", x, "+ age_num + gender_bin + country")), d))[[x]]
  f <- glm(as.formula(paste("mw ~", x, "+", m, "+ age_num + gender_bin + country")), binomial, d)
  b <- coef(f)[[m]]; dr <- coef(f)[[x]]
  c(a = a, b = b, indirect = a * b, direct = dr, prop = a * b / (a * b + dr))
}
for (x in c("restorative_z", "dialogic_z")) {
  est <- med_once(dat, x)
  bs <- replicate(1000, med_once(dat[sample(nrow(dat), replace = TRUE), ], x))
  ci <- apply(bs[c("indirect", "direct"), ], 1, quantile, c(.025, .975))
  cat(sprintf("%-12s a = %+.3f  b = %+.3f | indirect %+.3f [%+.3f, %+.3f] | direct %+.3f [%+.3f, %+.3f] (logit scale, per SD)\n",
      sub("_z", "", x), est["a"], est["b"], est["indirect"], ci[1, 1], ci[2, 1], est["direct"], ci[1, 2], ci[2, 2]))
}
cat("(Percentile intervals, 1000 resamples of respondents. Non-collapsibility of the logit makes the\n")
cat(" split between direct and indirect approximate; the sign pattern is the point.)\n")

cat("\n========== 5. What the effects mean in probabilities ==========\n")
mods <- lapply(c(mw = "mw", mn = "mn"), function(y)
  glm(as.formula(paste(y, "~ restorative_z + dialogic_z + plc_z + ctl_z + age_num + gender_bin + country")), binomial, dat))
pp <- function(m, v, lo, hi) {
  d1 <- dat; d1[[v]] <- lo; d2 <- dat; d2[[v]] <- hi
  c(low = mean(predict(m, d1, "response")), high = mean(predict(m, d2, "response")))
}
res5 <- do.call(rbind, lapply(names(mods), function(y) do.call(rbind, lapply(c("plc_z", "restorative_z", "dialogic_z"), function(v) {
  q <- quantile(dat[[v]], c(.1, .9)); p <- pp(mods[[y]], v, q[1], q[2])
  data.frame(outcome = ifelse(y == "mw", "wants mastery", "sees mastery"), predictor = sub("_z", "", v),
             p10 = round(100 * p[["low"]], 1), p90 = round(100 * p[["high"]], 1),
             change_pp = round(100 * (p[["high"]] - p[["low"]]), 1))
}))))
print(res5, row.names = FALSE)
cat("(Average predicted probability, in percent, with the predictor set to its 10th and 90th\n")
cat(" percentile for everyone and all else left as observed.)\n")

cat("\n========== 6. Split-half replication (same halves as script 13) ==========\n")
set.seed(20260929)
d13 <- readRDS("hnr_data.rds")
half <- unlist(lapply(split(seq_len(nrow(d13)), d13$country), function(i) sample(i, floor(length(i) / 2))))
d13$half <- ifelse(seq_len(nrow(d13)) %in% half, "A", "B")
dh <- dat; dh$half <- d13$half[match(rownames(dat), rownames(d13))]
print(rbind(show("wants, half A (structure found here)", dh[dh$half == "A", ]),
            show("wants, half B (held out)", dh[dh$half == "B", ]),
            show("sees,  half A (structure found here)", dh[dh$half == "A", ], "mn"),
            show("sees,  half B (held out)", dh[dh$half == "B", ], "mn")), row.names = FALSE)

cat("\n========== 7. Excluding respondents flagged for speeding or straight-lining ==========\n")
print(rbind(show("wants, clean", dat[!dat$flag_lowqual, ]),
            show("sees,  clean", dat[!dat$flag_lowqual, ], "mn")), row.names = FALSE)

saveRDS(list(ref = ref, stem = sg, loo = loo, prob = res5), "hnr_robustness.rds")
cat("\nSaved: hnr_robustness.rds\n")
