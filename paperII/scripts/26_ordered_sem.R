# ============================================================
# 26_ordered_sem.R
# The joint see-versus-want model of script 19, re-estimated on what script 24
# found the outcome to be: an ORDERED role (position on the dominance scale,
# all 2,489 respondents) and a second outcome, EXTREMITY (either pole versus
# the middle). Four outcomes in one model:
#
#   position now / position wanted    ordered, six categories
#   extremity now / extremity wanted  binary: Master or Object versus the rest
#
# Coding: position runs 1 Master ... 6 Object, so a POSITIVE path means a role
# further from Master (less human dominance). Extremity 1 = either pole.
#
# Predictors: restorative and dialogic motives (serviced is left out, see script
# 25), place agency, societal control, age, gender, five country dummies.
#
# CAUTION on extremity: place agency and control are entered as controls, but
# their own paths to extremity are not interpretable. Far more people want Master
# than Object, so any predictor that separates the two poles also looks like a
# predictor of "being at a pole". Read the MOTIVE paths to extremity, not the
# place-agency path.
#
# Requires: 01, 13 already run. Packages: lavaan.
# ============================================================

suppressMessages(library(lavaan))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ctl <- grep("^ctl_", names(dat), value = TRUE)

dat$pos_now    <- dat$typ_now
dat$pos_should <- dat$typ_should
dat$ext_now    <- as.integer(dat$typ_now %in% c(1, 6))
dat$ext_should <- as.integer(dat$typ_should %in% c(1, 6))
others <- setdiff(levels(dat$country), "Canada")
for (c_ in others) dat[[paste0("c_", c_)]] <- as.integer(dat$country == c_)
cd <- paste0("c_", others)

lat   <- c("restorative", "dialogic", "place", "control")
preds <- c(lat, "age_num", "gender_bin", cd)
outs  <- c(pos_now = "pn", pos_should = "pw", ext_now = "en", ext_should = "ew")
eq <- function(y, tag) paste0(y, " ~ ", paste(paste0(tag, seq_along(preds), "*", preds), collapse = " + "))
mod <- paste(c(
  paste0("restorative =~ ", paste(g$restorative, collapse = " + ")),
  paste0("dialogic =~ ", paste(g$dialogic, collapse = " + ")),
  paste0("place =~ ", paste(mp, collapse = " + ")),
  paste0("control =~ ", paste(ctl, collapse = " + ")),
  unlist(lapply(names(outs), function(y) eq(y, outs[[y]])))), collapse = "\n")

t0 <- Sys.time()
fit <- sem(mod, dat, estimator = "WLSMV",
           ordered = c(names(outs), mp, ctl, g$restorative, g$dialogic))
cat("fitted in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "seconds\n")

cat("========== 1. Fit ==========\n")
print(round(fitmeasures(fit, c("chisq", "df", "cfi", "tli", "rmsea", "srmr")), 3))

ps_all <- standardizedSolution(fit); ps <- ps_all[ps_all$op == "~", ]
shown <- c(lat, "age_num", "gender_bin")
star <- function(y, x) { r <- ps[ps$lhs == y & ps$rhs == x, ]; sprintf("%+.3f%s", r$est.std, ifelse(r$pvalue < .05, "*", " ")) }

cat("\n========== 2. Standardised paths ==========\n")
cat("position: positive = a role further from Master. extremity: positive = more likely at either pole.\n")
tab <- data.frame(predictor = shown,
                  position_now = sapply(shown, function(x) star("pos_now", x)),
                  position_wanted = sapply(shown, function(x) star("pos_should", x)),
                  extremity_now = sapply(shown, function(x) star("ext_now", x)),
                  extremity_wanted = sapply(shown, function(x) star("ext_should", x)))
print(tab, row.names = FALSE)

cat("\n========== 3. Does each path differ between seeing and wanting? (Wald) ==========\n")
wd <- function(a, b, label) do.call(rbind, lapply(shown, function(v) {
  k <- match(v, preds); r <- lavTestWald(fit, paste0(a, k, " == ", b, k))
  data.frame(outcome = label, predictor = v, chi2 = round(r$stat, 2), p = signif(r$p.value, 3),
             differs = ifelse(r$p.value < .05, "yes", "no")) }))
w <- rbind(wd("pn", "pw", "position"), wd("en", "ew", "extremity"))
print(w, row.names = FALSE)

cat("\n========== 4. Variance explained and residual associations ==========\n")
r2 <- inspect(fit, "r2")
cat(sprintf("R2  position now %.3f | position wanted %.3f | extremity now %.3f | extremity wanted %.3f\n",
            r2[["pos_now"]], r2[["pos_should"]], r2[["ext_now"]], r2[["ext_should"]]))
rc <- ps_all[ps_all$op == "~~" & ps_all$lhs %in% names(outs) & ps_all$rhs %in% names(outs) & ps_all$lhs != ps_all$rhs, ]
print(data.frame(pair = paste(rc$lhs, "~~", rc$rhs), r = round(rc$est.std, 3)), row.names = FALSE)
cat("(Position wanted and position seen share a large residual association: what people\n")
cat(" see and what they want move together beyond every predictor. That is the answer-\n")
cat(" consistency question raised by the survey order; it cannot be settled here.)\n")

saveRDS(list(fit = fit, table = tab, wald = w), "hnr_ordered_sem.rds")
cat("\nSaved: hnr_ordered_sem.rds\n")
