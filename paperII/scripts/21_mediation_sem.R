# ============================================================
# 21_mediation_sem.R
# Do the motives act on the role people want THROUGH how they construe
# their place? A parallel-mediation SEM.
#
# The design follows a pattern that recurs in the human-nature literature:
# experience of nature -> a construal or bond -> an outcome stance. Nature
# connectedness is the usual mediator between nature contact and
# pro-environmental outcomes (Mackay & Schmitt 2019; Martin et al. 2020),
# and anthropomorphism of nature acts as a mediator through connectedness
# (Tam, Lee & Chao 2013; Epley, Waytz & Cacioppo 2007). Here:
#
#   X  the three motives for visiting a natural place   (experience)
#   M  place agency, general agency of non-human beings, reciprocal
#      person-place beliefs, societal control           (construal)
#   Y  wanting mastery / seeing mastery                 (stance)
#
# The classic mediators -- a nature-connectedness scale, a place-attachment
# scale, the New Ecological Paradigm -- are NOT in this questionnaire, so
# this tests the candidates that are.
#
# CAUTION. The data are cross-sectional. A mediation model states a causal
# order; it does not test it. Reversing the arrows (people who want a less
# dominant role come to grant places more agency) gives an equivalent model
# with identical fit. Read the indirect effects as "consistent with
# mediation", not as evidence of it.
#
# Requires: 01, 12, 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ag  <- grep("^ag_now_", names(dat), value = TRUE)
q1  <- c("presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
         "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person")
ctl <- grep("^ctl_", names(dat), value = TRUE)
dat$master_now    <- as.integer(dat$typ_now == 1)
dat$master_should <- as.integer(dat$typ_should == 1)
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd   <- paste0("c_", others)
covs <- c("age_num", "gender_bin", "edu_primary", "edu_higher", "edu_na", cd)

ms <- c("place", "agency", "relational_f", "control")
ys <- c("master_should", "master_now")

meas_all <- list(
  restorative  = paste0("restorative =~ ", paste(g$restorative, collapse = " + ")),
  dialogic     = paste0("dialogic =~ ", paste(g$dialogic, collapse = " + ")),
  serviced     = paste0("serviced =~ ", paste(g$serviced, collapse = " + ")),
  place        = paste0("place =~ ", paste(mp, collapse = " + ")),
  agency       = paste0("agency =~ ", paste(ag, collapse = " + ")),
  relational_f = paste0("relational_f =~ ", paste(q1, collapse = " + ")),
  control      = paste0("control =~ ", paste(ctl, collapse = " + ")))

# One mediation model for a given set of motives. The motives are entered ONE
# AT A TIME in the main analysis, because they are correlated (restorative with
# dialogic .62) and entering them together produces suppression: standardised
# paths above 1 and a direct effect offset almost exactly by an opposite
# indirect effect. The three-together model is kept as a sensitivity run.
med_model <- function(xs) {
  a_eq <- sapply(ms, function(m) paste0(m, " ~ ",
            paste(paste0("a_", m, "_", xs, "*", xs), collapse = " + "), " + ", paste(covs, collapse = " + ")))
  y_eq <- sapply(ys, function(y) paste0(y, " ~ ",
            paste(c(paste0("d_", y, "_", xs, "*", xs), paste0("b_", y, "_", ms, "*", ms)),
                  collapse = " + "), " + ", paste(covs, collapse = " + ")))
  grid <- expand.grid(y = ys, x = xs, m = ms, stringsAsFactors = FALSE)
  grid$name <- sprintf("ind%02d", seq_len(nrow(grid)))
  def <- paste0(grid$name, " := a_", grid$m, "_", grid$x, " * b_", grid$y, "_", grid$m)
  tg <- expand.grid(y = ys, x = xs, stringsAsFactors = FALSE)
  tdef <- unlist(lapply(seq_len(nrow(tg)), function(i) {
    ids <- grid$name[grid$y == tg$y[i] & grid$x == tg$x[i]]
    c(paste0("tind", i, " := ", paste(ids, collapse = " + ")),
      paste0("tot", i, " := d_", tg$y[i], "_", tg$x[i], " + tind", i)) }))
  mod <- paste(c(unlist(meas_all[c(xs, ms)]), a_eq, y_eq, "master_now ~~ master_should", def, tdef),
               collapse = "\n")
  items <- c(ys, mp, ag, q1, ctl, unlist(g[names(g) %in% xs]))
  fit <- sem(mod, dat, estimator = "WLSMV", ordered = items)
  ps <- standardizedSolution(fit)
  get <- function(l) ps[match(l, ps$label), c("est.std", "pvalue")]
  reg <- ps[ps$op == "~" & !grepl("^c_|^age|^gender|^edu", ps$rhs), "est.std"]
  rows <- do.call(rbind, lapply(seq_len(nrow(tg)), function(i) {
    y <- tg$y[i]; x <- tg$x[i]
    ids <- grid$name[grid$y == y & grid$x == x]; mm <- grid$m[grid$y == y & grid$x == x]
    v <- get(ids); ti <- get(paste0("tind", i)); to <- get(paste0("tot", i)); dr <- get(paste0("d_", y, "_", x))
    data.frame(motive = x, outcome = y, direct = dr$est.std, p_dir = dr$pvalue,
               place = v$est.std[mm == "place"], p_pl = v$pvalue[mm == "place"],
               agency = v$est.std[mm == "agency"], p_ag = v$pvalue[mm == "agency"],
               relational = v$est.std[mm == "relational_f"], p_re = v$pvalue[mm == "relational_f"],
               control = v$est.std[mm == "control"], p_co = v$pvalue[mm == "control"],
               ind_total = ti$est.std, p_ind = ti$pvalue, total = to$est.std, p_tot = to$pvalue)
  }))
  a_ <- do.call(rbind, lapply(xs, function(x) data.frame(motive = x, mediator = ms,
          beta = get(paste0("a_", ms, "_", x))$est.std, p = get(paste0("a_", ms, "_", x))$pvalue)))
  b_ <- do.call(rbind, lapply(ys, function(y) data.frame(outcome = y, mediator = ms,
          beta = get(paste0("b_", y, "_", ms))$est.std, p = get(paste0("b_", y, "_", ms))$pvalue)))
  list(fit = fit, rows = rows, a = a_, b = b_, max_std = max(abs(reg), na.rm = TRUE), ps = ps, grid = grid, tg = tg)
}
star <- function(b, p) sprintf("%+.3f%s", b, ifelse(p < .05, "*", " "))

runs <- list()
for (x in c("restorative", "dialogic", "serviced")) {
  t0 <- Sys.time(); runs[[x]] <- med_model(x)
  fm <- fitmeasures(runs[[x]]$fit, c("cfi", "rmsea"))
  cat(sprintf("%-12s fitted in %3.0f s | CFI %.3f RMSEA %.3f | largest standardised path %.2f\n", x,
      as.numeric(difftime(Sys.time(), t0, units = "secs")), fm[["cfi"]], fm[["rmsea"]], runs[[x]]$max_std))
}
cat("\n(Every standardised path should sit inside +/-1. Values beyond that mark an\n")
cat(" unstable model; the single-motive runs are the ones to read.)\n")

cat("\n========== 1. Mediator -> outcome paths (b), across the three runs ==========\n")
bb <- do.call(rbind, lapply(names(runs), function(x) cbind(run = x, runs[[x]]$b)))
bw <- reshape(bb[, c("run", "outcome", "mediator", "beta")], idvar = c("outcome", "mediator"),
              timevar = "run", direction = "wide")
names(bw) <- sub("beta.", "", names(bw)); bw[, -(1:2)] <- round(bw[, -(1:2)], 3)
print(bw, row.names = FALSE)
cat("(If the effect of a mediator on the outcome is stable across runs, it does not\n")
cat(" depend on which motive is examined; large differences would be a further sign\n")
cat(" of instability.)\n")

cat("\n========== 2. Motive -> mediator paths (a) ==========\n")
aa <- do.call(rbind, lapply(names(runs), function(x) runs[[x]]$a))
aa$beta <- star(aa$beta, aa$p); aa$p <- NULL
print(reshape(aa, idvar = "motive", timevar = "mediator", direction = "wide"), row.names = FALSE)

show <- function(y) {
  r <- do.call(rbind, lapply(runs, function(z) z$rows[z$rows$outcome == y, ])); stab <- sapply(runs, function(z) z$max_std <= 1)
  data.frame(motive = r$motive, total = star(r$total, r$p_tot), direct = star(r$direct, r$p_dir),
             via_place = star(r$place, r$p_pl), via_agency = star(r$agency, r$p_ag),
             via_relational = star(r$relational, r$p_re), via_control = star(r$control, r$p_co),
             total_indirect = star(r$ind_total, r$p_ind),
             stable = ifelse(stab[r$motive], "yes", "NO: do not interpret"),
             share_mediated = ifelse(abs(r$total) < .05, "n/a (total near 0)",
                                     sprintf("%.0f%%", 100 * r$ind_total / r$total)))
}
cat("\n========== 3. Decomposition for WANTING mastery (one motive at a time) ==========\n")
print(show("master_should"), row.names = FALSE)
cat("\n========== 4. Decomposition for SEEING mastery ==========\n")
print(show("master_now"), row.names = FALSE)
cat("\n(* = p < .05, delta-method SE. Standardised on the latent-response scale for the\n")
cat(" binary outcome. share_mediated = total indirect / total, reported only when the\n")
cat(" total effect is not near zero. A bootstrap would be the check before publication\n")
cat(" but is impractical under WLSMV at this size.)\n")

cat("\n========== 5. Sensitivity: the three motives entered together ==========\n")
t0 <- Sys.time(); all3 <- med_model(c("restorative", "dialogic", "serviced"))
cat(sprintf("fitted in %.0f s | largest standardised path %.2f\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), all3$max_std))
r3 <- all3$rows[all3$rows$outcome == "master_should", ]
print(data.frame(motive = r3$motive, direct = star(r3$direct, r3$p_dir),
                 via_place = star(r3$place, r3$p_pl), total_indirect = star(r3$ind_total, r3$p_ind),
                 total = star(r3$total, r3$p_tot)), row.names = FALSE)
cat("(Compare the direct and indirect columns with section 3: where they are large and\n")
cat(" nearly cancel, the decomposition is a suppression artefact of correlated motives.)\n")

saveRDS(list(runs = lapply(runs, function(z) z[c("rows", "a", "b", "max_std", "ps", "grid", "tg")]),
             all3 = all3[c("rows", "a", "b", "max_std", "ps", "grid", "tg")]), "hnr_mediation_sem.rds")
cat("\nSaved: hnr_mediation_sem.rds\n")
