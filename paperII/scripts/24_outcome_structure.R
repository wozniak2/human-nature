# ============================================================
# 24_outcome_structure.R
# Is "wants Master versus everything else" the right way to analyse the
# role people choose? The dichotomy throws away five categories, and two
# findings suggest the structure is richer than one contrast:
#   - place agency rises steadily across the wanted roles (Master low, Object high)
#   - the dialogic motive raises BOTH poles, which no single ordered scale can hold
#
# The six roles are coded in the order of decreasing human influence over
# nature, taken from the narratives themselves:
#   1 Master    humans shape natural places and freely use them
#   2 Manager   humans manage them rationally, with care
#   3 User      humans treat them as resources to be kept for the future
#   4 Guardian  humans care for them because they are stronger
#   5 Partner   human intervention is unnecessary and undesirable
#   6 Object    humans have no influence at all
# This ordering is a reading of the text, not a result, and the middle of it
# (Manager, User, Guardian) is debatable; part 3 tests how much that matters.
#
# Questions:
#   1. Do the predictors line up with that ordering?
#   2. Does a single ordered scale describe the role, or do predictors act
#      differently at different cut points (proportional-odds check)?
#   3. Does it matter how the middle roles are ordered?
#   4. Is there a second dimension: DIRECTION (Master versus Object) and
#      EXTREMITY (either pole versus the middle)?
#
# Requires: 01, 13 already run. Packages: MASS, nnet.
# ============================================================

suppressMessages({ library(MASS); library(nnet) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
for (x in names(g)) dat[[x]] <- rowMeans(dat[, g[[x]], drop = FALSE], na.rm = TRUE)
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
dat$plc <- rowMeans(dat[, mp]); dat$ctl <- dat$control_mean
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
for (v in c("restorative", "dialogic", "serviced", "plc", "ctl")) dat[[paste0(v, "_z")]] <- zs(dat[[v]])
dat <- dat[complete.cases(dat[, c("typ_now", "typ_should", "age_num", "gender_bin")]), ]
roles <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")
preds <- c("plc_z", "restorative_z", "dialogic_z", "ctl_z")
cov   <- "age_num + gender_bin + country"
pname <- function(x) sub("_z", "", x)

cat("========== 1. Do the predictors line up with the ordering? (means by role) ==========\n")
prof <- function(var) t(sapply(c("plc", "restorative", "dialogic", "serviced", "ctl"), function(v)
  round(tapply(dat[[v]], factor(dat[[var]], 1:6, roles), mean), 2)))
cat("-- role people WANT --\n"); print(prof("typ_should"))
cat("-- role people SEE --\n");  print(prof("typ_now"))
cat("\neta-squared of role on each predictor (wanted / seen):\n")
eta <- function(v, r) { a <- anova(lm(dat[[v]] ~ factor(dat[[r]]))); a[1, 2] / sum(a[, 2]) }
print(round(t(sapply(c("plc", "restorative", "dialogic", "serviced", "ctl"),
      function(v) c(wanted = eta(v, "typ_should"), seen = eta(v, "typ_now")))), 3))
cat("(Place agency climbs from Master to Object; restorative rises toward Partner; dialogic\n")
cat(" is U-shaped, high at both ends. That is the pattern to explain.)\n")

cat("\n========== 2. One ordered scale? Slope of each predictor at each cut point ==========\n")
cut_or <- function(role_var) {
  do.call(rbind, lapply(1:5, function(k) {
    d <- dat; d$y <- as.integer(d[[role_var]] > k)
    m <- glm(as.formula(paste("y ~", paste(preds, collapse = " + "), "+", cov)), binomial, d)
    s <- summary(m)$coefficients[preds, , drop = FALSE]
    data.frame(cut = paste(roles[k], "|", roles[k + 1], "+"),
               t(setNames(sprintf("%.2f%s", exp(s[, 1]), ifelse(s[, 4] < .05, "*", " ")), pname(preds))))
  }))
}
cat("-- wanted role: odds ratio per SD of being at a HIGHER role than the cut (* p < .05) --\n")
print(cut_or("typ_should"), row.names = FALSE)
cat("\n-- seen role --\n"); print(cut_or("typ_now"), row.names = FALSE)
cat("(If one ordered scale held, each column would show roughly one odds ratio all the way\n")
cat(" down. Place agency does. Dialogic reverses sign across the cuts, so it is not\n")
cat(" describing position on the scale.)\n")

f_ord <- function(role_var, rank = 1:6) {
  d <- dat; d$y <- factor(rank[d[[role_var]]], levels = 1:6, ordered = TRUE)
  polr(as.formula(paste("y ~", paste(preds, collapse = " + "), "+", cov)), d, Hess = TRUE)
}
m_ord <- f_ord("typ_should")
tt <- coef(summary(m_ord))[preds, ]
cat("\nProportional-odds model, wanted role (odds ratio per SD, higher = a role further from Master):\n")
print(data.frame(predictor = pname(preds), OR = round(exp(tt[, "Value"]), 2),
                 p = signif(2 * pnorm(-abs(tt[, "t value"])), 3)), row.names = FALSE)
mm <- multinom(as.formula(paste("factor(typ_should) ~", paste(preds, collapse = " + "), "+", cov)), dat, trace = FALSE)
cat(sprintf("\nBIC: ordered logit %.0f | multinomial %.0f (lower is better; the multinomial is far more flexible)\n",
            BIC(m_ord), BIC(mm)))

cat("\n========== 3. Does the ordering of the middle roles matter? ==========\n")
perm <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i)
  lapply(perm(v[-i]), function(p) c(v[i], p))))
ords <- perm(c(2, 3, 4, 5))
res <- do.call(rbind, lapply(ords, function(o) {
  rank <- integer(6); rank[c(1, o, 6)] <- 1:6
  m <- f_ord("typ_should", rank)
  data.frame(order = paste(roles[c(1, o, 6)], collapse = " > "), BIC = round(BIC(m), 1))
}))
res <- res[order(res$BIC), ]; res$rank <- seq_len(nrow(res))
theo <- paste(roles, collapse = " > ")
cat("best five of the 24 orderings of the four middle roles (Master first, Object last):\n")
print(head(res, 5), row.names = FALSE)
cat(sprintf("\nthe theoretical ordering ranks %d of 24 (BIC %.1f, best %.1f)\n",
            res$rank[res$order == theo], res$BIC[res$order == theo], res$BIC[1]))
cat("(Ordering by fit is data-driven and partly circular, since place agency defines much of\n")
cat(" the fit. It shows whether the reading of the text is contradicted, not that it is right.)\n")

cat("\n========== 4. Two dimensions: extremity and direction ==========\n")
dd <- dat
for (rv in c("typ_should", "typ_now")) {
  dd$pole <- as.integer(dd[[rv]] %in% c(1, 6))
  m1 <- glm(as.formula(paste("pole ~", paste(preds, collapse = " + "), "+", cov)), binomial, dd)
  s1 <- summary(m1)$coefficients[preds, , drop = FALSE]
  sub <- dd[dd[[rv]] %in% c(1, 6), ]; sub$obj <- as.integer(sub[[rv]] == 6)
  m2 <- glm(as.formula(paste("obj ~", paste(preds, collapse = " + "), "+", cov)), binomial, sub)
  s2 <- summary(m2)$coefficients[preds, , drop = FALSE]
  cat(sprintf("\n-- %s role --  extremity: either pole (n = %d) versus the middle | direction: Object versus Master (n = %d)\n",
      ifelse(rv == "typ_should", "wanted", "seen"), sum(dd$pole), nrow(sub)))
  print(data.frame(predictor = pname(preds),
                   extremity_OR = sprintf("%.2f%s", exp(s1[, 1]), ifelse(s1[, 4] < .05, "*", " ")),
                   direction_OR = sprintf("%.2f%s", exp(s2[, 1]), ifelse(s2[, 4] < .05, "*", " "))), row.names = FALSE)
}
cat("\n(Reading: place agency separates Object from Master (direction), while the dialogic\n")
cat(" motive predicts being at EITHER pole (extremity) and not which one, and the restorative\n")
cat(" motive lowers extremity. A single ordered scale cannot hold both; place agency is the\n")
cat(" dominance dimension and the motives modulate how polarised a person is. Caution: the\n")
cat(" extremity odds ratio for place agency (below 1) is partly an artefact of unequal pole\n")
cat(" sizes, since far more people want Master than Object.)\n")

saveRDS(list(cuts_wanted = cut_or("typ_should"), cuts_seen = cut_or("typ_now"), orderings = res),
        "hnr_outcome_structure.rds")
cat("\nSaved: hnr_outcome_structure.rds\n")
