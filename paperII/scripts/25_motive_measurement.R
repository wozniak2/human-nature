# ============================================================
# 25_motive_measurement.R
# How solid are the motive measures? Three things are known to be shaky:
#   - the restorative effect is significant in one random half and not in the
#     other (script 23)
#   - the serviced factor is unstable in the structural models (script 21)
#   - the motive model fails scalar invariance across countries (script 16)
#
# This looks at each in turn, using the two dimensions found in script 24: the
# POSITION of the wanted role on the dominance scale (ordered logit) and its
# EXTREMITY (either pole versus the middle).
#
#   1. Item level: which items carry the effects?
#   2. Alternative scorings of the restorative and dialogic motives
#   3. Is the split-half instability just sampling noise? (repeated splits)
#   4. What is the serviced factor made of?
#   5. Which motives can be compared across countries?
#
# Requires: 01, 13 already run. Packages: MASS, lavaan.
# ============================================================

suppressMessages({ library(MASS); library(lavaan) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
dat$plc <- rowMeans(dat[, mp]); dat$ctl <- dat$control_mean
for (x in names(g)) dat[[x]] <- rowMeans(dat[, g[[x]], drop = FALSE], na.rm = TRUE)
dat <- dat[complete.cases(dat[, c("typ_should", "age_num", "gender_bin")]), ]
dat$pole <- as.integer(dat$typ_should %in% c(1, 6))
dat$ranked <- factor(dat$typ_should, levels = 1:6, ordered = TRUE)
dat$plc_z <- zs(dat$plc); dat$ctl_z <- zs(dat$ctl)
cov <- "age_num + gender_bin + edu_primary + edu_higher + edu_na + country"

ord_p <- function(d, terms) {
  m <- polr(as.formula(paste("ranked ~", paste(terms, collapse = " + "), "+", cov)), d, Hess = TRUE)
  cf <- coef(summary(m))[terms, , drop = FALSE]
  data.frame(term = terms, OR = exp(cf[, "Value"]), p = 2 * pnorm(-abs(cf[, "t value"])), row.names = NULL)
}
pole_p <- function(d, terms) {
  m <- glm(as.formula(paste("pole ~", paste(terms, collapse = " + "), "+", cov)), binomial, d)
  s <- summary(m)$coefficients[terms, , drop = FALSE]
  data.frame(term = terms, OR = exp(s[, 1]), p = s[, 4], row.names = NULL)
}
fmt <- function(x) sprintf("%.2f%s", x$OR, ifelse(x$p < .05, "*", " "))
cronbach <- function(m) { m <- m[complete.cases(m), , drop = FALSE]; k <- ncol(m)
  (k / (k - 1)) * (1 - sum(apply(m, 2, var)) / var(rowSums(m))) }

cat("========== 1. Item level: which items carry the effects? ==========\n")
items <- c("rs_relax", "rs_quiet", "rs_own_thoughts", "rs_beauty", "rs_watch_plants", "rs_active",
           "rs_teaches", "rs_communicates", "rs_meet_animals", "rs_comfort", "rs_photos", "rs_struggles",
           "rs_meet_people", "rs_cheap")
for (v in items) dat[[paste0(v, "_z")]] <- zs(dat[[v]])
it <- do.call(rbind, lapply(items, function(v) {
  z <- paste0(v, "_z")
  a <- ord_p(dat, c(z, "plc_z"))[1, ]; b <- pole_p(dat, c(z, "plc_z"))[1, ]
  data.frame(item = sub("rs_", "", v),
             factor = c(names(g)[sapply(g, function(x) v %in% x)], "dropped")[1],
             position_OR = fmt(a), extremity_OR = fmt(b))
}))
print(it, row.names = FALSE)
cat("(Each item alone, with place agency and demographics held constant. Position: odds ratio\n")
cat(" per SD of wanting a role further from Master. Extremity: odds of wanting either pole.)\n")

cat("\n========== 2. Alternative scorings ==========\n")
rest_items <- g$restorative; dia_items <- g$dialogic
variants <- list(
  "restorative, all six items"        = rest_items,
  "restorative, core four"            = c("rs_relax", "rs_quiet", "rs_own_thoughts", "rs_beauty"),
  "restorative, relax + quiet"        = c("rs_relax", "rs_quiet"),
  "restorative, without active"       = setdiff(rest_items, "rs_active"),
  "dialogic, all three items"         = dia_items,
  "dialogic, teaches + communicates"  = c("rs_teaches", "rs_communicates"))
rows <- do.call(rbind, lapply(names(variants), function(nn) {
  v <- variants[[nn]]; d <- dat; d$s <- zs(rowMeans(d[, v, drop = FALSE]))
  other <- if (grepl("^restorative", nn)) zs(rowMeans(d[, dia_items])) else zs(rowMeans(d[, rest_items]))
  d$o <- other
  a <- ord_p(d, c("s", "o", "plc_z"))[1, ]; b <- pole_p(d, c("s", "o", "plc_z"))[1, ]
  data.frame(scoring = nn, items = length(v), alpha = round(cronbach(d[, v, drop = FALSE]), 2),
             position_OR = fmt(a), extremity_OR = fmt(b))
}))
print(rows, row.names = FALSE)
cat("(The other motive and place agency are held constant in each row.)\n")

cat("\n========== 3. Is the split-half instability sampling noise? (300 random splits) ==========\n")
set.seed(20260929)
dat$rest_z <- zs(dat$restorative); dat$dia_z <- zs(dat$dialogic)
one <- function() {
  h <- unlist(lapply(split(seq_len(nrow(dat)), dat$country), function(i) sample(i, floor(length(i) / 2))))
  d <- dat[h, ]
  r <- ord_p(d, c("rest_z", "dia_z", "plc_z")); r$p
}
ps <- t(replicate(300, one())); colnames(ps) <- c("restorative", "dialogic", "place agency")
cat("share of random half-samples (n about 1,245) in which the effect on position is significant:\n")
print(round(colMeans(ps < .05), 2))
cat("median p-value:\n"); print(signif(apply(ps, 2, median), 2))
full <- ord_p(dat, c("rest_z", "dia_z", "plc_z"))
cat(sprintf("\nfull-sample odds ratios: restorative %.2f, dialogic %.2f, place agency %.2f\n", full$OR[1], full$OR[2], full$OR[3]))
cat("(A true effect of this size should be significant in most half-samples if power is\n")
cat(" adequate. A share near one half means a half is simply too small to detect it reliably,\n")
cat(" and one non-significant half is then not evidence against the effect.)\n")

cat("\n========== 4. What is the serviced factor made of? ==========\n")
sv <- c("rs_comfort", "rs_photos", "rs_struggles")
cat("correlations among the three items, and with the other motives and place agency:\n")
print(round(cor(cbind(dat[, sv], restorative = dat$restorative, dialogic = dat$dialogic, place_agency = dat$plc),
                use = "pairwise"), 2))
dat$amenity <- zs(rowMeans(dat[, c("rs_comfort", "rs_photos")])); dat$nuisance <- zs(dat$rs_struggles)
cat(sprintf("\nalpha: all three %.2f | comfort + photos %.2f\n", cronbach(dat[, sv]), cronbach(dat[, c("rs_comfort", "rs_photos")])))
dat$serv_z <- zs(rowMeans(dat[, sv]))
sp <- do.call(rbind, lapply(c("serv_z", "amenity", "nuisance"), function(v) {
  a <- ord_p(dat, c(v, "rest_z", "dia_z", "plc_z"))[1, ]; b <- pole_p(dat, c(v, "rest_z", "dia_z", "plc_z"))[1, ]
  data.frame(score = c(serv_z = "serviced, all three", amenity = "amenity (comfort + photos)", nuisance = "nuisance (struggles)")[v],
             position_OR = fmt(a), extremity_OR = fmt(b))
}))
print(sp, row.names = FALSE)
cat("(If amenity and nuisance behave differently, the serviced factor is two things and its\n")
cat(" instability in the structural models is unsurprising.)\n")

cat("\n========== 5. Which motives can be compared across countries? ==========\n")
two <- paste0("rest_f =~ ", paste(g$restorative, collapse = " + "),
              "\ndia_f =~ ", paste(g$dialogic, collapse = " + "))
fits <- list(configural = cfa(two, dat, group = "country", estimator = "MLR"),
             metric = cfa(two, dat, group = "country", estimator = "MLR", group.equal = "loadings"),
             scalar = cfa(two, dat, group = "country", estimator = "MLR", group.equal = c("loadings", "intercepts")))
t <- t(sapply(fits, function(f) fitmeasures(f, c("cfi", "rmsea", "srmr"))))
cat("restorative + dialogic only (serviced left out):\n")
print(cbind(round(t, 3), dCFI = round(c(NA, diff(t[, "cfi"])), 4)))
three <- paste0(two, "\nserv_f =~ ", paste(g$serviced, collapse = " + "))
f3 <- list(metric = cfa(three, dat, group = "country", estimator = "MLR", group.equal = "loadings"),
           scalar = cfa(three, dat, group = "country", estimator = "MLR", group.equal = c("loadings", "intercepts")),
           configural = cfa(three, dat, group = "country", estimator = "MLR"))
t3 <- t(sapply(f3[c("configural", "metric", "scalar")], function(f) fitmeasures(f, c("cfi", "rmsea", "srmr"))))
cat("\nall three motives, for comparison:\n")
print(cbind(round(t3, 3), dCFI = round(c(NA, diff(t3[, "cfi"])), 4)))
cat("(Chen 2007: a CFI drop beyond .010 counts against invariance. Without the serviced factor\n")
cat(" the loadings are equivalent across countries (metric holds), so the RELATIONS of the\n")
cat(" restorative and dialogic motives to other variables can be compared. Scalar invariance\n")
cat(" still fails, so country MEANS on any of the motives cannot.)\n")

saveRDS(list(items = it, scorings = rows, splits = ps), "hnr_motive_measurement.rds")
cat("\nSaved: hnr_motive_measurement.rds\n")
