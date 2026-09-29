# ============================================================
# 27_ordered_mediation.R
# Do the motives act on the ORDERED wanted role through place agency? This is
# script 21 redone on the outcome script 24 found appropriate, and narrowed to
# what the earlier runs supported:
#
#   X  one motive at a time (restorative, dialogic); entered together they
#      suppress one another (script 21)
#   M  place agency and societal control (general agency and reciprocal beliefs
#      added at most +/-.04 in script 21 and are dropped)
#   Y  position of the wanted role, and of the seen role, on the ordered scale
#
# Serviced is left out (script 25: weak, low reliability). Extremity is NOT
# decomposed: place agency has no clean path to it (unequal pole sizes), so an
# indirect effect through it would not be interpretable. Extremity is handled
# in script 26 as a direct outcome of the motives.
#
# Position runs 1 Master ... 6 Object; a POSITIVE effect means a role further
# from Master. Effects are standardised on the latent-response scale.
#
# CAUTION: cross-sectional, and the place-agency items were asked after the role
# question; the decomposition is consistent with mediation, not evidence of it.
#
# A bootstrap cross-check on observed composites (ordered logit) follows the SEM,
# because delta-method intervals are weak for indirect effects.
#
# Requires: 01, 13 already run. Packages: lavaan, MASS.
# ============================================================

suppressMessages({ library(lavaan); library(MASS) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ctl <- grep("^ctl_", names(dat), value = TRUE)
dat$pos_now <- dat$typ_now; dat$pos_should <- dat$typ_should
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd   <- paste0("c_", others)
covs <- c("age_num", "gender_bin", cd)
ms <- c("place", "control")
ys <- c("pos_should", "pos_now")

meas <- list(
  restorative = paste0("restorative =~ ", paste(g$restorative, collapse = " + ")),
  dialogic    = paste0("dialogic =~ ", paste(g$dialogic, collapse = " + ")),
  place       = paste0("place =~ ", paste(mp, collapse = " + ")),
  control     = paste0("control =~ ", paste(ctl, collapse = " + ")))

med_model <- function(x) {
  a_eq <- sapply(ms, function(m) paste0(m, " ~ a_", m, "*", x, " + ", paste(covs, collapse = " + ")))
  y_eq <- sapply(ys, function(y) paste0(y, " ~ d_", y, "*", x, " + ",
            paste(paste0("b_", y, "_", ms, "*", ms), collapse = " + "), " + ", paste(covs, collapse = " + ")))
  def <- unlist(lapply(ys, function(y) c(
    paste0("ind_", y, "_place := a_place * b_", y, "_place"),
    paste0("ind_", y, "_control := a_control * b_", y, "_control"),
    paste0("tind_", y, " := ind_", y, "_place + ind_", y, "_control"),
    paste0("tot_", y, " := d_", y, " + tind_", y))))
  mod <- paste(c(unlist(meas[c(x, ms)]), a_eq, y_eq, "pos_now ~~ pos_should", def), collapse = "\n")
  fit <- sem(mod, dat, estimator = "WLSMV", ordered = c(ys, mp, ctl, g[[x]]))
  ps <- standardizedSolution(fit)
  reg <- ps[ps$op == "~" & !grepl("^c_|^age|^gender", ps$rhs), "est.std"]
  list(fit = fit, ps = ps, max_std = max(abs(reg), na.rm = TRUE))
}
star <- function(ps, l) { r <- ps[match(l, ps$label), ]; sprintf("%+.3f%s", r$est.std, ifelse(r$pvalue < .05, "*", " ")) }

runs <- list()
for (x in c("restorative", "dialogic")) {
  t0 <- Sys.time(); runs[[x]] <- med_model(x)
  fm <- fitmeasures(runs[[x]]$fit, c("cfi", "rmsea"))
  cat(sprintf("%-12s fitted in %3.0f s | CFI %.3f RMSEA %.3f | largest standardised path %.2f%s\n", x,
      as.numeric(difftime(Sys.time(), t0, units = "secs")), fm[["cfi"]], fm[["rmsea"]], runs[[x]]$max_std,
      ifelse(runs[[x]]$max_std > 1, "   <- UNSTABLE, do not interpret", "")))
}

cat("\n========== 1. Motive -> mediator (a) and mediator -> outcome (b) paths ==========\n")
for (x in names(runs)) {
  ps <- runs[[x]]$ps
  cat(sprintf("-- %s --  a: place %s, control %s | b (wanted): place %s, control %s | b (seen): place %s, control %s\n", x,
      star(ps, "a_place"), star(ps, "a_control"),
      star(ps, "b_pos_should_place"), star(ps, "b_pos_should_control"),
      star(ps, "b_pos_now_place"), star(ps, "b_pos_now_control")))
}

cat("\n========== 2. Decomposition of the effect on position ==========\n")
cat("(positive = a role further from Master; * = p < .05)\n")
dec <- do.call(rbind, lapply(names(runs), function(x) do.call(rbind, lapply(ys, function(y) {
  ps <- runs[[x]]$ps
  data.frame(motive = x, outcome = ifelse(y == "pos_should", "wanted", "seen"),
             total = star(ps, paste0("tot_", y)), direct = star(ps, paste0("d_", y)),
             via_place = star(ps, paste0("ind_", y, "_place")), via_control = star(ps, paste0("ind_", y, "_control")),
             total_indirect = star(ps, paste0("tind_", y)))
}))))
print(dec, row.names = FALSE)

cat("\n========== 3. Bootstrap cross-check (observed composites, ordered logit, 500 resamples) ==========\n")
dat$plc <- rowMeans(dat[, mp]); dat$ctl <- rowMeans(dat[, ctl])
dat$restorative_s <- rowMeans(dat[, g$restorative]); dat$dialogic_s <- rowMeans(dat[, g$dialogic])
zs <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
for (v in c("plc", "restorative_s", "dialogic_s")) dat[[paste0(v, "_z")]] <- zs(dat[[v]])
d0 <- dat[complete.cases(dat[, c("typ_should", "age_num", "gender_bin")]), ]
d0$ranked <- factor(d0$typ_should, levels = 1:6, ordered = TRUE)
med_once <- function(d, x) {
  a <- coef(lm(as.formula(paste("plc_z ~", x, "+ age_num + gender_bin + country")), d))[[x]]
  f <- polr(as.formula(paste("ranked ~", x, "+ plc_z + age_num + gender_bin + country")), d)
  b <- coef(f)[["plc_z"]]; dr <- coef(f)[[x]]
  c(indirect = a * b, direct = dr)
}
set.seed(20260929)
for (x in c("restorative_s_z", "dialogic_s_z")) {
  est <- med_once(d0, x)
  bs <- replicate(500, med_once(d0[sample(nrow(d0), replace = TRUE), ], x))
  ci <- apply(bs, 1, quantile, c(.025, .975))
  cat(sprintf("%-14s indirect via place agency %+.3f [%+.3f, %+.3f] | direct %+.3f [%+.3f, %+.3f]   (log-odds per SD, wanted position)\n",
      sub("_s_z", "", x), est["indirect"], ci[1, "indirect"], ci[2, "indirect"], est["direct"], ci[1, "direct"], ci[2, "direct"]))
}
cat("(Percentile intervals. An indirect effect whose interval excludes zero alongside a direct\n")
cat(" effect that does not is complete mediation; opposite signs are suppression.)\n")

saveRDS(list(runs = lapply(runs, function(z) z[c("ps", "max_std")]), decomposition = dec), "hnr_ordered_mediation.rds")
cat("\nSaved: hnr_ordered_mediation.rds\n")
