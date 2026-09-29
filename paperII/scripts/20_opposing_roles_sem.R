# ============================================================
# 20_opposing_roles_sem.R
# Script 19 asked what predicts wanting and seeing MASTERY. This asks the
# same of the roles that oppose it, with the identical model, so the
# pattern can be read across the typology and not only at one pole.
#
#   Guardian  the role that gains most (171 see it, 339 want it)
#   Partner   the conceptual opposite of Master: humans need not intervene
#   Object    the far pole, humans subject to nature (too few to fit: skipped
#             unless the counts allow)
#
# Each role gets a joint model for the role people SEE and the role they
# WANT, with the same predictors as script 19 (three motives, place agency,
# societal control, age, gender, country) and a Wald test of every path
# across the two outcomes. Requires: 01, 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ctl <- grep("^ctl_", names(dat), value = TRUE)
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd <- paste0("c_", others)
lat   <- c("restorative", "dialogic", "serviced", "place", "control")
preds <- c(lat, "age_num", "gender_bin", cd)
shown <- c(lat, "age_num", "gender_bin")
eq <- function(out, tag) paste0(out, " ~ ", paste(paste0(tag, seq_along(preds), "*", preds), collapse = " + "))

roles <- c(Guardian = 4, Partner = 5, Object = 6)
cat("Respondents seeing / wanting each role:\n")
print(sapply(roles, function(r) c(now = sum(dat$typ_now == r), should = sum(dat$typ_should == r))))

fit_role <- function(code) {
  dd <- dat
  dd$out_now <- as.integer(dd$typ_now == code); dd$out_should <- as.integer(dd$typ_should == code)
  mod <- paste0(
   'restorative =~ ', paste(g$restorative, collapse = " + "), '
    dialogic    =~ ', paste(g$dialogic,    collapse = " + "), '
    serviced    =~ ', paste(g$serviced,    collapse = " + "), '
    place       =~ ', paste(mp,  collapse = " + "), '
    control     =~ ', paste(ctl, collapse = " + "), '
    ', eq("out_now", "a"), '
    ', eq("out_should", "b"), '
    out_now ~~ out_should')
  sem(mod, dd, estimator = "WLSMV", ordered = c("out_now", "out_should", mp, ctl))
}

results <- list()
for (rn in names(roles)) {
  if (min(sum(dat$typ_now == roles[[rn]]), sum(dat$typ_should == roles[[rn]])) < 100) {
    cat("\n##", rn, ": fewer than 100 in one of the two answers, not fitted.\n"); next }
  cat("\n==========", rn, "==========\n")
  f <- fit_role(roles[[rn]])
  cat("fit:", paste(names(fitmeasures(f, c("cfi","tli","rmsea","srmr"))),
                    round(fitmeasures(f, c("cfi","tli","rmsea","srmr")), 3), collapse = "  "), "\n")
  ps <- standardizedSolution(f); reg <- ps[ps$op == "~", ]
  get <- function(o) setNames(reg$est.std[reg$lhs == o], reg$rhs[reg$lhs == o])
  pv  <- function(o) setNames(reg$pvalue[reg$lhs == o],  reg$rhs[reg$lhs == o])
  wd <- do.call(rbind, lapply(shown, function(v) { k <- match(v, preds)
    r <- lavTestWald(f, paste0("a", k, " == b", k)); data.frame(p = r$p.value) }))
  tab <- data.frame(predictor = shown, beta_now = round(get("out_now")[shown], 3),
                    p_now = signif(pv("out_now")[shown], 2),
                    beta_should = round(get("out_should")[shown], 3),
                    p_should = signif(pv("out_should")[shown], 2),
                    differs = ifelse(wd$p < .05, "yes", "no"), wald_p = signif(wd$p, 2))
  print(tab, row.names = FALSE)
  r2 <- inspect(f, "r2")
  rc <- ps[ps$op == "~~" & ps$lhs == "out_now" & ps$rhs == "out_should", ]
  cat(sprintf("R2 now %.3f | R2 should %.3f | residual correlation %.3f\n",
              r2[["out_now"]], r2[["out_should"]], rc$est.std))
  results[[rn]] <- list(fit = f, table = tab)
}
saveRDS(results, "hnr_opposing_roles_sem.rds")
cat("\nSaved: hnr_opposing_roles_sem.rds\n")
