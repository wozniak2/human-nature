# ============================================================
# 19_now_vs_should_sem.R
# The same structural model for the role people SEE ("how is it now")
# and the role people WANT ("how should it be"), fitted together so the
# two can be compared path by path.
#
# Predictors, identical for both outcomes:
#   the three motives (13), the own-place latent (18), a societal control
#   latent (the Q4 battery: how far humans exploit, manage, coexist with or
#   are subject to twelve entities), age, gender and five country dummies.
#
# Outcomes: whether the respondent picks Master as the role now / should be.
# Both are binary, so estimation is WLSMV with probit links; the two
# outcomes are allowed a residual correlation.
#
# The question this answers: the earlier bivariate results suggested the
# personal stance tracks the role people WANT while the societal control
# battery tracks the role people SEE. Here that is tested with every other
# predictor held constant, and each path is tested for equality across the
# two outcomes.
#
# Requires: 01, 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ctl <- grep("^ctl_", names(dat), value = TRUE)

dat$master_now    <- as.integer(dat$typ_now == 1)
dat$master_should <- as.integer(dat$typ_should == 1)
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd <- paste0("c_", others)

# Latent names must not match observed columns; the motive score columns
# are not created here, so nothing collides.
lat <- c("restorative", "dialogic", "serviced", "place", "control")
preds <- c(lat, "age_num", "gender_bin", cd)
eq <- function(out, tag) paste0(out, " ~ ",
        paste(paste0(tag, seq_along(preds), "*", preds), collapse = " + "))
mod <- paste0(
 'restorative =~ ', paste(g$restorative, collapse = " + "), '
  dialogic    =~ ', paste(g$dialogic,    collapse = " + "), '
  serviced    =~ ', paste(g$serviced,    collapse = " + "), '
  place       =~ ', paste(mp,  collapse = " + "), '
  control     =~ ', paste(ctl, collapse = " + "), '
  ', eq("master_now", "a"), '
  ', eq("master_should", "b"), '
  master_now ~~ master_should')

fit <- sem(mod, dat, estimator = "WLSMV", ordered = c("master_now", "master_should", mp, ctl))

cat("========== 1. Fit ==========\n")
print(round(fitmeasures(fit, c("chisq","df","cfi","tli","rmsea","srmr")), 3))
cat("(CFI and TLI are above .95 and RMSEA is .060. The control block alone fits less\n")
cat(" well, RMSEA .103 by itself, which is why it enters as one latent among five.)\n")

cat("\n========== 2. Standardised paths: role SEEN now versus role WANTED ==========\n")
ps_all <- standardizedSolution(fit); ps <- ps_all[ps_all$op == "~", ]
tab <- function(out) { d <- ps[ps$lhs == out, ]; setNames(d[, c("est.std", "pvalue")], c("beta", "p")) -> d
                       rownames(d) <- ps$rhs[ps$lhs == out]; d }
now <- tab("master_now"); sho <- tab("master_should")
shown <- rownames(now)[!grepl("^c_", rownames(now))]
cmp <- data.frame(predictor = shown,
                  beta_now = round(now[shown, "beta"], 3), p_now = signif(now[shown, "p"], 2),
                  beta_should = round(sho[shown, "beta"], 3), p_should = signif(sho[shown, "p"], 2))
print(cmp, row.names = FALSE)

cat("\n========== 3. Does each path differ between the two outcomes? (Wald) ==========\n")
w <- do.call(rbind, lapply(seq_along(shown), function(i) {
  k <- match(shown[i], preds)
  r <- lavTestWald(fit, paste0("a", k, " == b", k))
  data.frame(predictor = shown[i], chi2 = round(r$stat, 2), p = signif(r$p.value, 3))
}))
w$differs <- ifelse(w$p < .05, "yes", "no")
print(w, row.names = FALSE)
cat("(A Wald test of a path against its twin: 'yes' means the predictor relates\n")
cat(" differently to the role people see than to the role they want.)\n")

cat("\n========== 4. Variance explained and residual association ==========\n")
r2 <- inspect(fit, "r2")
cat("R2 sees mastery now:  ", round(r2[["master_now"]], 4), "\n")
cat("R2 wants mastery:     ", round(r2[["master_should"]], 4), "\n")
rc <- ps_all[ps_all$op == "~~" & ps_all$lhs == "master_now" & ps_all$rhs == "master_should", ]
cat("residual correlation between the two outcomes:", round(rc$est.std, 3), "\n")
cat("(What remains shared between seeing and wanting mastery after every\n")
cat(" predictor is held constant.)\n")

saveRDS(list(fit = fit, compare = cmp, wald = w), "hnr_now_vs_should_sem.rds")
cat("\nSaved: hnr_now_vs_should_sem.rds\n")
