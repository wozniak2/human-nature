# ============================================================
# 31_place_agency_facets.R
# Facet check for place agency: which part of it carries each result?
#
# Place agency has two facets that correlate about .64 (script 18, section 6):
#   independence   who acts on the place: Q7 does it serve my will or is it
#                  independent (mp_emancipation), Q9 does it follow human rules
#                  or the laws of nature (mp_agency)
#   communication  what passes between person and place: Q8 dialogue is
#                  possible (mp_dialogue), Q10 it teaches (mp_learning), Q12 its
#                  change is a form of communication (mp_time)
#
# Why this matters: the dialogic motive items ("the place teaches me", "the
# place communicates with me") share wording with the communication facet, so
# the dialogic route "through place agency" could be carried by near-identical
# questions. The independence facet shares no wording with either motive.
#
#   1. how each place-agency item correlates with each motive item
#   2. one factor versus two facets (WLSMV, ordinal)
#   3. which facet is linked to the role wanted and the role seen
#   4. mediation with the two facets as parallel mediators (latent SEM), one
#      motive at a time, as in script 27
#   5. bootstrap cross-check on observed facet scores (ordered logit)
#   6. a zero-overlap test: the dialogic motive rebuilt from "animals to meet"
#      alone, through the independence facet alone
#
# Position runs 1 Master ... 6 Object; positive = a role further from Master.
# Cross-sectional: the decomposition is consistent with mediation, not proof.
#
# Requires: 01, 04, 13 already run. Packages: lavaan, MASS.
# ============================================================

suppressMessages({ library(lavaan); library(MASS) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
ind  <- c("mp_emancipation", "mp_agency")                    # independence facet
com  <- c("mp_dialogue", "mp_learning", "mp_time")          # communication facet
mp   <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ctl  <- grep("^ctl_", names(dat), value = TRUE)
dat$pos_now <- dat$typ_now; dat$pos_should <- dat$typ_should
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd   <- paste0("c_", others)
covs <- c("age_num", "gender_bin", "edu_primary", "edu_higher", "edu_na", cd)
cov_txt <- "age_num + gender_bin + edu_primary + edu_higher + edu_na + country"
zs <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)

cat("========== 1. Which place-agency items share content with which motive items? ==========\n")
cat("(Spearman correlations. The dialogic motive items are teaches, communicates, meet_animals.)\n")
mot_items <- c(g$dialogic, g$restorative)
M <- sapply(mot_items, function(m) sapply(mp, function(p) cor(dat[[p]], dat[[m]], use = "complete.obs", method = "spearman")))
dimnames(M) <- list(paste0(mp, ifelse(mp %in% ind, " [indep]", " [comm]")), sub("^rs_", "", mot_items))
print(round(M, 2))
dat$indep_s <- rowMeans(dat[, ind]); dat$comm_s <- rowMeans(dat[, com]); dat$plc <- rowMeans(dat[, mp])
dat$restorative_s <- rowMeans(dat[, g$restorative]); dat$dialogic_s <- rowMeans(dat[, g$dialogic])
cat("\nfacet scores against the motive scales:\n")
print(round(cor(dat[, c("indep_s", "comm_s")], dat[, c("restorative_s", "dialogic_s")], use = "complete.obs", method = "spearman"), 2))

cat("\n========== 2. One factor or two facets? ==========\n")
f1 <- cfa(paste("place =~", paste(mp, collapse = " + ")), dat, estimator = "WLSMV", ordered = mp)
f2 <- cfa(paste0("indep =~ ", paste(ind, collapse = " + "), "\ncomm =~ ", paste(com, collapse = " + ")),
          dat, estimator = "WLSMV", ordered = mp)
fm <- function(f) round(fitmeasures(f, c("chisq.scaled", "df", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")), 3)
print(rbind(one_factor = fm(f1), two_facets = fm(f2)))
r12 <- standardizedSolution(f2); r12 <- r12[r12$op == "~~" & r12$lhs == "indep" & r12$rhs == "comm", ]
cat(sprintf("latent correlation of the two facets: %.2f [%.2f, %.2f]\n", r12$est.std, r12$ci.lower, r12$ci.upper))

cat("\n========== 3. Which facet is linked to the role wanted and the role seen? ==========\n")
cat("(ordered logit, odds ratio per SD, adjusted for both motives, societal control, age, gender, education, country)\n")
for (v in c("plc", "restorative_s", "dialogic_s")) dat[[paste0(v, "_z")]] <- zs(dat[[v]])
dat$indep_z <- zs(dat$indep_s); dat$comm_z <- zs(dat$comm_s)
dat$ctl_z <- zs(dat$control_mean)
d0 <- dat[complete.cases(dat[, c("typ_should", "typ_now", "age_num", "gender_bin")]), ]
d0$w <- factor(d0$typ_should, levels = 1:6, ordered = TRUE); d0$s <- factor(d0$typ_now, levels = 1:6, ordered = TRUE)
or_row <- function(y, rhs, keep) {
  f <- polr(as.formula(paste(y, "~", rhs, "+ restorative_s_z + dialogic_s_z + ctl_z +", cov_txt)), d0, Hess = TRUE)
  s <- coef(summary(f))[keep, , drop = FALSE]
  sprintf("%.2f [%.2f, %.2f]", exp(s[, 1]), exp(s[, 1] - 1.96 * s[, 2]), exp(s[, 1] + 1.96 * s[, 2]))
}
tab3 <- data.frame(model = c("independence alone", "communication alone", "both facets: independence", "both facets: communication", "full scale (reference)"),
  wanted = c(or_row("w", "indep_z", "indep_z"), or_row("w", "comm_z", "comm_z"), or_row("w", "indep_z + comm_z", c("indep_z", "comm_z")), or_row("w", "plc_z", "plc_z")),
  seen   = c(or_row("s", "indep_z", "indep_z"), or_row("s", "comm_z", "comm_z"), or_row("s", "indep_z + comm_z", c("indep_z", "comm_z")), or_row("s", "plc_z", "plc_z")))
print(tab3, row.names = FALSE)

cat("\n========== 4. Mediation with the two facets as parallel mediators (latent SEM) ==========\n")
meas <- list(
  restorative = paste0("restorative =~ ", paste(g$restorative, collapse = " + ")),
  dialogic    = paste0("dialogic =~ ", paste(g$dialogic, collapse = " + ")),
  indep       = paste0("indep =~ ", paste(ind, collapse = " + ")),
  comm        = paste0("comm =~ ", paste(com, collapse = " + ")),
  control     = paste0("control =~ ", paste(ctl, collapse = " + ")))
ms <- c("indep", "comm", "control"); ys <- c("pos_should", "pos_now")
med_model <- function(x) {
  a_eq <- sapply(ms, function(m) paste0(m, " ~ a_", m, "*", x, " + ", paste(covs, collapse = " + ")))
  y_eq <- sapply(ys, function(y) paste0(y, " ~ d_", y, "*", x, " + ",
            paste(paste0("b_", y, "_", ms, "*", ms), collapse = " + "), " + ", paste(covs, collapse = " + ")))
  def <- unlist(lapply(ys, function(y) c(
    paste0("ind_", y, "_", ms, " := a_", ms, " * b_", y, "_", ms),
    paste0("tind_", y, " := ", paste(paste0("a_", ms, " * b_", y, "_", ms), collapse = " + ")),
    paste0("tot_", y, " := d_", y, " + tind_", y))))
  mod <- paste(c(unlist(meas[c(x, ms)]), a_eq, y_eq, "pos_now ~~ pos_should", "indep ~~ comm", def), collapse = "\n")
  fit <- sem(mod, dat, estimator = "WLSMV", ordered = c(ys, mp, ctl, g[[x]]))
  ps <- standardizedSolution(fit)
  reg <- ps[ps$op == "~" & !grepl("^c_|^age|^gender|^edu", ps$rhs), "est.std"]
  list(fit = fit, ps = ps, max_std = max(abs(reg), na.rm = TRUE))
}
star <- function(ps, l) { r <- ps[match(l, ps$label), ]; sprintf("%+.3f%s", r$est.std, ifelse(r$pvalue < .05, "*", " ")) }
ci   <- function(ps, l) { r <- ps[match(l, ps$label), ]; sprintf("%+.3f [%+.3f, %+.3f]", r$est.std, r$ci.lower, r$ci.upper) }
runs <- list()
for (x in c("restorative", "dialogic")) {
  t0 <- Sys.time(); runs[[x]] <- med_model(x)
  f <- fitmeasures(runs[[x]]$fit, c("cfi", "rmsea"))
  cat(sprintf("%-12s fitted in %3.0f s | CFI %.3f RMSEA %.3f | largest standardised path %.2f%s\n", x,
      as.numeric(difftime(Sys.time(), t0, units = "secs")), f[["cfi"]], f[["rmsea"]], runs[[x]]$max_std,
      ifelse(runs[[x]]$max_std > 1, "   <- UNSTABLE, do not interpret", "")))
}
cat("\n-- motive -> facet (a) and facet -> wanted position (b), standardised; * = p < .05 --\n")
for (x in names(runs)) {
  ps <- runs[[x]]$ps
  cat(sprintf("%-12s a: independence %s, communication %s | b (wanted): independence %s, communication %s | b (seen): independence %s, communication %s\n", x,
      star(ps, "a_indep"), star(ps, "a_comm"), star(ps, "b_pos_should_indep"), star(ps, "b_pos_should_comm"),
      star(ps, "b_pos_now_indep"), star(ps, "b_pos_now_comm")))
}
cat("\n-- indirect effect on the WANTED position through each facet, 95% CI --\n")
dec <- do.call(rbind, lapply(names(runs), function(x) { ps <- runs[[x]]$ps
  data.frame(motive = x, via_independence = ci(ps, "ind_pos_should_indep"), via_communication = ci(ps, "ind_pos_should_comm"),
             via_control = ci(ps, "ind_pos_should_control"), direct = ci(ps, "d_pos_should"), total = ci(ps, "tot_pos_should")) }))
print(dec, row.names = FALSE)

cat("\n========== 5. Bootstrap cross-check (observed facet scores, ordered logit, 500 resamples) ==========\n")
med_once <- function(d, x, mediators) {
  out <- c()
  f <- polr(as.formula(paste("w ~", x, "+", paste(mediators, collapse = " + "), "+", cov_txt)), d)
  for (m in mediators) {
    a <- coef(lm(as.formula(paste(m, "~", x, "+", cov_txt)), d))[[x]]
    out[paste0("via_", m)] <- a * coef(f)[[m]]
  }
  c(out, direct = coef(f)[[x]])
}
set.seed(20260929)
boot_tab <- function(x, mediators, label) {
  est <- med_once(d0, x, mediators)
  bs <- replicate(500, med_once(d0[sample(nrow(d0), replace = TRUE), ], x, mediators))
  q <- apply(bs, 1, quantile, c(.025, .975))
  data.frame(motive = label, effect = names(est),
             estimate = sprintf("%+.3f [%+.3f, %+.3f]", est, q[1, ], q[2, ]))
}
bt <- rbind(boot_tab("restorative_s_z", c("indep_z", "comm_z"), "restorative"),
            boot_tab("dialogic_s_z",    c("indep_z", "comm_z"), "dialogic"))
bt$effect <- sub("via_indep_z", "via independence", sub("via_comm_z", "via communication", bt$effect))
print(bt, row.names = FALSE)
cat("(log-odds per SD of the motive, wanted position; the two facets are entered together as mediators)\n")

cat("\n========== 6. Zero-overlap test ==========\n")
cat("The dialogic motive rebuilt from 'animals to meet' alone (no shared wording with any place-agency\n")
cat("item), and the independence facet alone as mediator (no shared wording with any motive item).\n")
d0$animals_z <- zs(d0$rs_meet_animals)
z6 <- rbind(boot_tab("animals_z", "indep_z", "dialogic (animals item only)"),
            boot_tab("restorative_s_z", "indep_z", "restorative"))
z6$effect <- sub("via_indep_z", "via independence", z6$effect)
print(z6, row.names = FALSE)

saveRDS(list(item_overlap = M, facet_fit = rbind(one_factor = fm(f1), two_facets = fm(f2)), facet_role = tab3,
             sem = lapply(runs, function(z) z[c("ps", "max_std")]), sem_decomposition = dec, bootstrap = bt, zero_overlap = z6),
        "hnr_place_agency_facets.rds")
cat("\nSaved: hnr_place_agency_facets.rds\n")
