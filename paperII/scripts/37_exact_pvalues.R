# ============================================================
# 37_exact_pvalues.R
# Exact p-values for the supplementary tables.
#
# The earlier scripts print many estimates with an asterisk for p < .05.
# The journal prefers exact p-values, so this script prints them, table by
# table, next to the estimate they belong to. Nothing new is estimated:
#   - the cut-point models of script 24 and the alternative scorings of
#     script 25 are refitted with the same specification (the estimates
#     printed here must equal those in the log of 24 and 25)
#   - the joint model (26), the mediation models (27), the single-role
#     models (20) and the indicator correlations (30) are read from the
#     objects those scripts saved
#
# p-values are printed as in the text: "<.001" below .001, otherwise three
# decimals.
#
# Requires: 01, 04, 13, 20, 24, 25, 26, 27, 30 already run. Packages: MASS, lavaan.
# ============================================================

suppressMessages({ library(MASS); library(lavaan) })
pf <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "<.001", sub("^0[.]", ".", sprintf("%.3f", p))))
bf <- function(b) { s <- sub("^(-?)0[.]", "\\1.", sprintf("%.2f", b)); ifelse(s == "-.00", ".00", s) }

dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
cov <- "age_num + gender_bin + edu_primary + edu_higher + edu_na + country"
roles <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")

cat("========== 1. Cut points of the six-role scale (script 24, section 2) ==========\n")
cat("(odds ratio per SD of being above the cut, with its exact p-value)\n")
d24 <- dat
for (x in names(g)) d24[[x]] <- rowMeans(d24[, g[[x]], drop = FALSE], na.rm = TRUE)
d24$plc <- rowMeans(d24[, mp]); d24$ctl <- d24$control_mean
for (v in c("restorative", "dialogic", "plc", "ctl")) d24[[paste0(v, "_z")]] <- zs(d24[[v]])
d24 <- d24[complete.cases(d24[, c("typ_now", "typ_should", "age_num", "gender_bin")]), ]
preds <- c("plc_z", "restorative_z", "dialogic_z", "ctl_z")
cut_p <- function(role_var) do.call(rbind, lapply(1:5, function(k) {
  d <- d24; d$y <- as.integer(d[[role_var]] > k)
  m <- glm(as.formula(paste("y ~", paste(preds, collapse = " + "), "+", cov)), binomial, d)
  s <- summary(m)$coefficients[preds, , drop = FALSE]
  data.frame(cut = paste(roles[k], "|", roles[k + 1]),
             t(setNames(sprintf("%.2f (%s)", exp(s[, 1]), pf(s[, 4])), sub("_z", "", preds))))
}))
cat("-- role wanted --\n"); print(cut_p("typ_should"), row.names = FALSE)
cat("-- role seen --\n");   print(cut_p("typ_now"), row.names = FALSE)
d24$ranked <- factor(d24$typ_should, levels = 1:6, ordered = TRUE)
po <- coef(summary(polr(as.formula(paste("ranked ~", paste(preds, collapse = " + "), "+", cov)), d24, Hess = TRUE)))[preds, ]
cat("ordered logit, role wanted: ",
    paste0(sub("_z", "", preds), " ", sprintf("%.2f", exp(po[, "Value"])), " (", pf(2 * pnorm(-abs(po[, "t value"]))), ")", collapse = " | "), "\n")

cat("\n========== 2. Alternative scorings of the motives (script 25, section 2) ==========\n")
d25 <- dat
d25$plc <- rowMeans(d25[, mp])
d25 <- d25[complete.cases(d25[, c("typ_should", "age_num", "gender_bin")]), ]
d25$pole <- as.integer(d25$typ_should %in% c(1, 6))
d25$ranked <- factor(d25$typ_should, levels = 1:6, ordered = TRUE)
d25$plc_z <- zs(d25$plc)
ord_p <- function(d, terms) {
  m <- polr(as.formula(paste("ranked ~", paste(terms, collapse = " + "), "+", cov)), d, Hess = TRUE)
  cf <- coef(summary(m))[terms, , drop = FALSE]
  data.frame(OR = exp(cf[, "Value"]), p = 2 * pnorm(-abs(cf[, "t value"])))
}
pole_p <- function(d, terms) {
  s <- summary(glm(as.formula(paste("pole ~", paste(terms, collapse = " + "), "+", cov)), binomial, d))$coefficients[terms, , drop = FALSE]
  data.frame(OR = exp(s[, 1]), p = s[, 4])
}
rest_items <- g$restorative; dia_items <- g$dialogic
variants <- list(
  "restorative, all six items"        = rest_items,
  "restorative, core four"            = c("rs_relax", "rs_quiet", "rs_own_thoughts", "rs_beauty"),
  "restorative, relax + quiet"        = c("rs_relax", "rs_quiet"),
  "restorative, without active"       = setdiff(rest_items, "rs_active"),
  "dialogic, all three items"         = dia_items,
  "dialogic, teaches + communicates"  = c("rs_teaches", "rs_communicates"))
sc <- do.call(rbind, lapply(names(variants), function(nn) {
  v <- variants[[nn]]; d <- d25; d$s <- zs(rowMeans(d[, v, drop = FALSE]))
  d$o <- if (grepl("^restorative", nn)) zs(rowMeans(d[, dia_items])) else zs(rowMeans(d[, rest_items]))
  a <- ord_p(d, c("s", "o", "plc_z"))[1, ]; b <- pole_p(d, c("s", "o", "plc_z"))[1, ]
  data.frame(scoring = nn, position = sprintf("%.2f (%s)", a$OR, pf(a$p)), extremity = sprintf("%.2f (%s)", b$OR, pf(b$p)))
}))
print(sc, row.names = FALSE)
cat("item level (each item alone, with place agency): position and extremity\n")
items <- c("rs_relax", "rs_quiet", "rs_own_thoughts", "rs_beauty", "rs_watch_plants", "rs_active",
           "rs_teaches", "rs_communicates", "rs_meet_animals")
it <- do.call(rbind, lapply(items, function(v) {
  d <- d25; d$z <- zs(d[[v]])
  a <- ord_p(d, c("z", "plc_z"))[1, ]; b <- pole_p(d, c("z", "plc_z"))[1, ]
  data.frame(item = sub("rs_", "", v), position = sprintf("%.2f (%s)", a$OR, pf(a$p)), extremity = sprintf("%.2f (%s)", b$OR, pf(b$p)))
}))
print(it, row.names = FALSE)

cat("\n========== 3. The joint model of seeing and wanting (script 26) ==========\n")
so <- readRDS("hnr_ordered_sem.rds")
ps <- standardizedSolution(so$fit)
vars <- c("place", "control", "dialogic", "restorative", "age_num", "gender_bin", "edu_primary", "edu_higher")
cell <- function(y) { r <- ps[ps$op == "~" & ps$lhs == y, ]; r <- r[match(vars, r$rhs), ]; sprintf("%s (%s)", bf(r$est.std), pf(r$pvalue)) }
wl <- function(o) { w <- so$wald[so$wald$outcome == o, ]; w <- w[match(vars, w$predictor), ]
  sprintf("%.1f (%s)%s", w$chi2, pf(w$p), ifelse(p.adjust(so$wald$p, "holm")[match(paste(o, vars), paste(so$wald$outcome, so$wald$predictor))] < .05, " H", "")) }
print(data.frame(predictor = vars, position_seen = cell("pos_now"), position_wanted = cell("pos_should"), wald_position = wl("position"),
                 extremity_seen = cell("ext_now"), extremity_wanted = cell("ext_should"), wald_extremity = wl("extremity")), row.names = FALSE)
cat("(standardised path (p); Wald chi2(1) (p) for seen versus wanted; H = remains after a Holm correction for the 16 tests)\n")

cat("\n========== 4. Decomposition of each motive's association with the position (script 27) ==========\n")
om <- readRDS("hnr_ordered_mediation.rds")
dec <- do.call(rbind, lapply(names(om$runs), function(x) do.call(rbind, lapply(c("pos_should", "pos_now"), function(y) {
  q <- om$runs[[x]]$ps; one <- function(l) { r <- q[match(l, q$label), ]; sprintf("%s (%s)", bf(r$est.std), pf(r$pvalue)) }
  data.frame(motive = x, outcome = ifelse(y == "pos_should", "wanted", "seen"), total = one(paste0("tot_", y)), direct = one(paste0("d_", y)),
             via_place = one(paste0("ind_", y, "_place")), via_control = one(paste0("ind_", y, "_control")))
}))))
print(dec, row.names = FALSE)

cat("\n========== 5. Guardian, Partner and Object (script 20) ==========\n")
op <- readRDS("hnr_opposing_roles_sem.rds")
for (r in names(op)) {
  shown <- op[[r]]$table$predictor                       # p-values are taken from the fit, not from the rounded table
  q <- standardizedSolution(op[[r]]$fit); q <- q[q$op == "~", ]
  one <- function(y) { z <- q[q$lhs == y, ]; z <- z[match(shown, z$rhs), ]; sprintf("%s (%s)", bf(z$est.std), pf(z$pvalue)) }
  cat("--", r, "--\n")
  print(data.frame(predictor = shown, seen = one("out_now"), wanted = one("out_should")), row.names = FALSE)
}

cat("\n========== 6. Each indicator against the role (script 30) ==========\n")
ic <- readRDS("hnr_indicator_connections.rds")$table
print(data.frame(indicator = ic$indicator, seen = sprintf("%s (%s)", bf(ic$seen), pf(ic$p_seen)),
                 wanted = sprintf("%s (%s)", bf(ic$wanted), pf(ic$p_wanted)), shift = sprintf("%s (%s)", bf(ic$shift), pf(ic$p_shift)),
                 extreme = sprintf("%s (%s)", bf(ic$extreme), pf(ic$p_ext))), row.names = FALSE)
cat("(Spearman rho (p), country differences removed)\n")

cat("\n========== 7. Robustness variants (script 23) ==========\n")
cat("(odds ratio per SD for wanting, or seeing, Master, with its exact p-value)\n")
d23 <- dat
for (x in names(g)) d23[[x]] <- rowMeans(d23[, g[[x]], drop = FALSE], na.rm = TRUE)
d23$plc <- rowMeans(d23[, mp]); d23$plc_comm <- rowMeans(d23[, c("mp_dialogue", "mp_learning", "mp_time")])
d23$ctl <- d23$control_mean
d23$mw <- as.integer(d23$typ_should == 1); d23$mn <- as.integer(d23$typ_now == 1)
for (v in c("restorative", "dialogic", "plc", "plc_comm", "ctl", "ARS", "MRS", "ERS")) d23[[paste0(v, "_z")]] <- zs(d23[[v]])
d23 <- d23[complete.cases(d23[, c("mw", "mn", "age_num", "gender_bin")]), ]
core <- c("plc_z", "restorative_z", "dialogic_z", "ctl_z")
rob <- function(label, d, y = "mw", terms = core, extra = character(0)) {
  rhs <- c(terms, extra, "age_num", "gender_bin", "edu_primary", "edu_higher", "edu_na", if (length(unique(d$country)) > 1) "country")
  s <- summary(glm(as.formula(paste(y, "~", paste(rhs, collapse = " + "))), binomial, d))$coefficients[terms, , drop = FALSE]
  data.frame(variant = label, n = nrow(d), t(setNames(sprintf("%.2f (%s)", exp(s[, 1]), pf(s[, 4])), c("place", "restorative", "dialogic", "control"))))
}
ann <- d23$country %in% c("Netherlands", "Sweden", "Poland")
set.seed(20260929)                                   # the same halves as scripts 13 and 23
d13 <- readRDS("hnr_data.rds")
half <- unlist(lapply(split(seq_len(nrow(d13)), d13$country), function(i) sample(i, floor(length(i) / 2))))
d13$half <- ifelse(seq_len(nrow(d13)) %in% half, "A", "B")
d23$half <- d13$half[match(rownames(d23), rownames(d13))]
sty <- c("ARS_z", "MRS_z", "ERS_z"); comm <- c("plc_comm_z", "restorative_z", "dialogic_z", "ctl_z")
cat("-- wanting Master --\n")
print(rbind(rob("pooled", d23), rob("second question announced (NL, SE, PL)", d23[ann, ]), rob("not announced (CA, ES, PA)", d23[!ann, ]),
            do.call(rbind, lapply(levels(d23$country), function(cn) rob(paste("without", cn), d23[d23$country != cn, ]))),
            rob("with response style", d23, extra = sty), rob("place agency: dialogue, learning, time", d23, terms = comm),
            rob("half A", d23[d23$half == "A", ]), rob("half B", d23[d23$half == "B", ]), rob("without flagged", d23[!d23$flag_lowqual, ])), row.names = FALSE)
cat("-- seeing Master --\n")
print(rbind(rob("pooled", d23, "mn"), rob("with response style", d23, "mn", extra = sty),
            rob("place agency: dialogue, learning, time", d23, "mn", terms = comm),
            rob("half A", d23[d23$half == "A", ], "mn"), rob("half B", d23[d23$half == "B", ], "mn"),
            rob("without flagged", d23[!d23$flag_lowqual, ], "mn")), row.names = FALSE)
