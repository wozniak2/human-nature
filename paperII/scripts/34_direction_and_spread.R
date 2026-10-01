# ============================================================
# 34_direction_and_spread.R
# One model for direction and spread of the role people want.
#
# Scripts 24 and 26 treated "position" and "extremity" of the same answer as two
# outcomes. This script replaces that with one location-scale ordered logit
# (McCullagh 1980): for the six ordered roles,
#
#     P(role <= j) = logistic( (threshold_j - x'beta) / exp(z'gamma) )
#
#   beta  (location, "direction"): which way a predictor moves the wanted role;
#         exp(beta) is an odds ratio for a role further from Master
#   gamma (scale, "spread"): whether a predictor widens or narrows the spread of
#         choices around that position; exp(gamma) > 1 means more choices at
#         BOTH ends, < 1 means more choices in the middle
#
#   1. check: with no scale part the model must reproduce MASS::polr
#   2. direction and spread for the wanted role
#   3. the same with response style added (acquiescence, midpoint, extreme
#      responding), in both parts, and with country in the spread part
#   4. what the spread effects mean in probabilities
#   5. is the spread only the Master pole? a three-category model (Master, the
#      four middle roles, Object) and the direction among extreme choosers
#   6. the role seen, for comparison
#   7. cross-check against ordinal::clm
#   8. the dialogic results without the wording that place agency and the dialogic
#      motive share (place agency as its independence facet; dialogic as one item)
#
# The model is fitted by maximum likelihood here (no extra package); if the
# `ordinal` package is installed, its clm() is fitted too and compared.
#
# Requires: 01, 04, 13 already run. Packages: MASS, nnet.
# ============================================================

suppressMessages({ library(MASS); library(nnet) })
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
zs <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
dat$place <- zs(rowMeans(dat[, mp])); dat$restful <- zs(rowMeans(dat[, g$restorative]))
dat$dialogic <- zs(rowMeans(dat[, g$dialogic])); dat$control <- zs(dat$control_mean)
# for section 8: versions without the wording shared by place agency and the dialogic motive
dat$place_indep <- zs(rowMeans(dat[, c("mp_emancipation", "mp_agency")])); dat$dia_animals <- zs(dat$rs_meet_animals)
for (v in c("ARS", "MRS", "ERS")) dat[[paste0(v, "_z")]] <- zs(dat[[v]])
main  <- c("place", "restful", "dialogic", "control")
style <- c("ARS_z", "MRS_z", "ERS_z")
lab <- c(place = "place agency", restful = "restful motive", dialogic = "dialogic motive", control = "societal control",
         ARS_z = "acquiescence", MRS_z = "midpoint responding", ERS_z = "extreme responding",
         place_indep = "place agency (independence facet)", dia_animals = "dialogic ('animals to meet' only)")
cov_txt <- "age_num + gender_bin + edu_primary + edu_higher + edu_na + country"
d <- dat[complete.cases(dat[, c("typ_now", "typ_should", "age_num", "gender_bin", main, style)]), ]
cat("respondents:", nrow(d), "\n")

# ---- location-scale ordered logit by maximum likelihood ----
ls_fit <- function(y, loc, scale = NULL, data = d) {
  X <- model.matrix(as.formula(paste("~", loc)), data)[, -1, drop = FALSE]
  Z <- if (is.null(scale)) NULL else model.matrix(as.formula(paste("~", scale)), data)[, -1, drop = FALSE]
  K <- max(y); p <- ncol(X); q <- if (is.null(Z)) 0 else ncol(Z); n <- length(y)
  unpack <- function(par) {
    th <- cumsum(c(par[1], exp(par[2:(K - 1)])))
    list(th = th, beta = par[K:(K + p - 1)], gamma = if (q) par[(K + p):(K + p + q - 1)] else numeric(0))
  }
  probs <- function(par, X_ = X, Z_ = Z) {
    u <- unpack(par); eta <- as.vector(X_ %*% u$beta); s <- if (q) exp(as.vector(Z_ %*% u$gamma)) else rep(1, nrow(X_))
    cum <- sapply(u$th, function(t) plogis((t - eta) / s))
    cbind(cum, 1) - cbind(0, cum)
  }
  nll <- function(par) { P <- probs(par); -sum(log(pmax(P[cbind(seq_len(n), y)], 1e-300))) }
  po <- polr(as.formula(paste("factor(y, ordered = TRUE) ~", loc)), data = cbind(data, y = y))
  start <- c(po$zeta[1], log(diff(po$zeta)), coef(po)[colnames(X)], rep(0, q))
  o <- nlminb(start, nll, control = list(iter.max = 5000, eval.max = 20000))
  o2 <- optim(o$par, nll, method = "BFGS", control = list(maxit = 2000, reltol = 1e-12))   # polish
  if (o2$value < o$objective) { o$par <- o2$par; o$objective <- o2$value }
  gr <- function(par, h = 1e-5) sapply(seq_along(par), function(i) { e <- replace(numeric(length(par)), i, h); (nll(par + e) - nll(par - e)) / (2 * h) })
  for (it in 1:5) {                                     # Newton steps until the gradient is flat
    H <- optimHess(o$par, nll); grad <- gr(o$par)
    if (max(abs(grad)) < 1e-4) break
    cand <- o$par - solve(H, grad)
    if (nll(cand) <= o$objective) { o$par <- cand; o$objective <- nll(cand) } else break
  }
  H <- optimHess(o$par, nll); grad <- gr(o$par); se <- sqrt(diag(solve(H)))
  ok <- all(eigen(H, symmetric = TRUE, only.values = TRUE)$values > 0) && max(abs(grad)) < 0.01   # a maximum: flat gradient, curved downward
  u <- unpack(o$par); idx_b <- K:(K + p - 1); idx_g <- if (q) (K + p):(K + p + q - 1) else integer(0)
  list(logLik = -o$objective, k = length(o$par), conv = ok, grad = max(abs(grad)), par = o$par, probs = probs, X = X, Z = Z,
       beta = setNames(u$beta, colnames(X)), se_beta = setNames(se[idx_b], colnames(X)),
       gamma = if (q) setNames(u$gamma, colnames(Z)) else NULL, se_gamma = if (q) setNames(se[idx_g], colnames(Z)) else NULL,
       polr = po, loc = loc, scale = scale)
}
eff <- function(est, se, keep) { e <- est[keep]; s <- se[keep]
  data.frame(predictor = lab[keep], ratio = sprintf("%.2f [%.2f, %.2f]", exp(e), exp(e - 1.96 * s), exp(e + 1.96 * s)),
             p = signif(2 * pnorm(-abs(e / s)), 2), row.names = NULL) }
lrt <- function(a, b) { x <- 2 * (b$logLik - a$logLik); df <- b$k - a$k; sprintf("chi2(%d) = %.1f, p = %.2g", df, x, pchisq(x, df, lower.tail = FALSE)) }
ic <- function(f, n = nrow(d)) sprintf("AIC %.1f, BIC %.1f", -2 * f$logLik + 2 * f$k, -2 * f$logLik + log(n) * f$k)

yw <- d$typ_should; ys <- d$typ_now
loc1 <- paste(c(main, cov_txt), collapse = " + "); sc1 <- paste(main, collapse = " + ")
loc2 <- paste(c(main, style, cov_txt), collapse = " + "); sc2 <- paste(c(main, style), collapse = " + ")

cat("\n========== 1. Check: with no spread part the model reproduces MASS::polr ==========\n")
A <- ls_fit(yw, loc1)
cat(sprintf("largest difference from polr in a coefficient: %.2e | in the log-likelihood: %.2e | converged: %s\n",
            max(abs(A$beta - coef(A$polr)[names(A$beta)])), abs(A$logLik - as.numeric(logLik(A$polr))), A$conv))

cat("\n========== 2. Direction and spread of the WANTED role ==========\n")
B <- ls_fit(yw, loc1, sc1)
cat("direction (odds ratio per SD for a role further from Master):\n"); print(eff(B$beta, B$se_beta, main), row.names = FALSE)
cat("spread (ratio per SD; above 1 = more choices at both ends, below 1 = more in the middle):\n"); print(eff(B$gamma, B$se_gamma, main), row.names = FALSE)
cat("adding the spread part to the ordered logit:", lrt(A, B), "\n")
cat("ordered logit:", ic(A), "| with spread:", ic(B), "\n")

cat("\n========== 3. With response style in both parts, and with country in the spread part ==========\n")
A2 <- ls_fit(yw, loc2); C <- ls_fit(yw, loc2, sc2)
cat("direction:\n"); print(eff(C$beta, C$se_beta, c(main, style)), row.names = FALSE)
cat("spread:\n");    print(eff(C$gamma, C$se_gamma, c(main, style)), row.names = FALSE)
cat("spread part, given response style:", lrt(A2, C), "\n")
D <- ls_fit(yw, loc2, paste(sc2, "+ country"))
cat("\nwith country also allowed to change the spread:\n")
print(eff(D$gamma, D$se_gamma, main), row.names = FALSE)

cat("\n========== 4. What the spread effects mean in probabilities (model with response style) ==========\n")
pp <- function(f, var) {
  q <- quantile(d[[var]], c(.1, .9))
  t(sapply(q, function(v) { nd <- d; nd[[var]] <- v
    X_ <- model.matrix(as.formula(paste("~", f$loc)), nd)[, -1, drop = FALSE]
    Z_ <- model.matrix(as.formula(paste("~", f$scale)), nd)[, -1, drop = FALSE]
    P <- unname(colMeans(f$probs(f$par, X_, Z_))); c(Master = P[1], Object = P[6], either_pole = P[1] + P[6], middle_four = sum(P[2:5])) }))
}
for (v in c("dialogic", "restful", "place")) {
  r <- round(100 * pp(C, v), 1); rownames(r) <- c("low (10th pct)", "high (90th pct)")
  cat(sprintf("-- %s: average predicted %% choosing each --\n", lab[v])); print(r)
}

cat("\n========== 5. Is the spread only the Master pole? ==========\n")
d$three <- factor(ifelse(yw == 1, "Master", ifelse(yw == 6, "Object", "middle")), levels = c("middle", "Master", "Object"))
cat("wanted role:", paste(names(table(d$three)), table(d$three), collapse = " | "), "\n")
mn <- multinom(as.formula(paste("three ~", loc2)), d, trace = FALSE)
co <- coef(mn); se <- summary(mn)$standard.errors
t5 <- do.call(rbind, lapply(main, function(v) data.frame(predictor = lab[v],
  Master_vs_middle = sprintf("%.2f [%.2f, %.2f]", exp(co["Master", v]), exp(co["Master", v] - 1.96 * se["Master", v]), exp(co["Master", v] + 1.96 * se["Master", v])),
  Object_vs_middle = sprintf("%.2f [%.2f, %.2f]", exp(co["Object", v]), exp(co["Object", v] - 1.96 * se["Object", v]), exp(co["Object", v] + 1.96 * se["Object", v])))))
cat("relative risk ratios per SD (response style, age, gender, education, country in the model):\n"); print(t5, row.names = FALSE)
cat("(A predictor of spread raises, or lowers, BOTH columns; a predictor of direction moves them in opposite ways.)\n")
pp3 <- function(var) { q <- quantile(d[[var]], c(.1, .9))
  r <- t(sapply(q, function(v) { nd <- d; nd[[var]] <- v; colMeans(predict(mn, nd, type = "probs"))[c("Master", "Object", "middle")] }))
  rownames(r) <- c("low (10th pct)", "high (90th pct)"); round(100 * r, 1) }
cat("\nthe same three-category model in probabilities (average predicted %):\n")
for (v in c("dialogic", "restful")) { cat("--", lab[v], "--\n"); print(pp3(v)) }
ex <- d[yw %in% c(1, 6), ]; ex$object <- as.integer(ex$typ_should == 6)
fd <- glm(as.formula(paste("object ~", loc2)), binomial, ex); sd_ <- coef(summary(fd))
cat(sprintf("\namong the %d extreme choosers, Object rather than Master (odds ratio per SD):\n", nrow(ex)))
print(data.frame(predictor = lab[main], OR = sprintf("%.2f [%.2f, %.2f]", exp(sd_[main, 1]), exp(sd_[main, 1] - 1.96 * sd_[main, 2]), exp(sd_[main, 1] + 1.96 * sd_[main, 2])),
                 p = signif(sd_[main, 4], 2), row.names = NULL), row.names = FALSE)

cat("\n========== 6. The role SEEN, for comparison (with response style) ==========\n")
S <- ls_fit(ys, loc2, sc2); S0 <- ls_fit(ys, loc2)
cat("direction:\n"); print(eff(S$beta, S$se_beta, main), row.names = FALSE)
cat("spread:\n");    print(eff(S$gamma, S$se_gamma, main), row.names = FALSE)
cat("spread part:", lrt(S0, S), "\n")

cat(sprintf("\nall models at a maximum (flat gradient, negative-definite curvature): %s | largest gradient %.1e\n",
            all(c(A$conv, B$conv, A2$conv, C$conv, D$conv, S$conv, S0$conv)), max(A$grad, B$grad, A2$grad, C$grad, D$grad, S$grad, S0$grad)))

cat("\n========== 7. Cross-check against ordinal::clm ==========\n")
if (requireNamespace("ordinal", quietly = TRUE)) {
  dd <- d; dd$w <- factor(yw, ordered = TRUE)
  chk <- function(f, loc, sc, name) {
    cl <- ordinal::clm(as.formula(paste("w ~", loc)), scale = as.formula(paste("~", sc)), data = dd)
    se <- sqrt(diag(vcov(cl))); se <- setNames(tail(se, length(f$gamma)), names(cl$zeta))   # spread terms come last; names repeat those of the direction part
    cat(sprintf("%-28s largest difference: direction %.1e | spread %.1e | standard error of a spread term %.1e | log-likelihood %.1e\n", name,
                max(abs(f$beta - cl$beta[names(f$beta)])), max(abs(f$gamma - cl$zeta[names(f$gamma)])),
                max(abs(f$se_gamma - se[names(f$gamma)])), abs(f$logLik - as.numeric(logLik(cl)))))
  }
  chk(B, loc1, sc1, "wanted role"); chk(C, loc2, sc2, "wanted role + response style")
} else cat("(ordinal is not installed; the check against MASS::polr in section 1 stands in for it.)\n")

cat("\n========== 8. Without the wording shared by place agency and the dialogic motive ==========\n")
cat("Two place-agency items (dialogue, learning/teaching) resemble two dialogic items ('communicates', 'teaches').\n",
    "(a) place agency as its independence facet alone (emancipation, agency), which shares no wording with any motive item;\n",
    "(b) in addition, the dialogic motive as its one item without shared wording ('animals to meet').\n", sep = "")
overlap_free <- function(pl, dia, tag) {
  mm <- c(pl, "restful", dia, "control")
  loc <- paste(c(mm, style, cov_txt), collapse = " + "); sc <- paste(c(mm, style), collapse = " + ")
  f <- ls_fit(yw, loc, sc)
  m3 <- multinom(as.formula(paste("three ~", loc)), d, trace = FALSE); c3 <- coef(m3); s3 <- summary(m3)$standard.errors
  rr <- function(k) sprintf("%.2f [%.2f, %.2f]", exp(c3[k, dia]), exp(c3[k, dia] - 1.96 * s3[k, dia]), exp(c3[k, dia] + 1.96 * s3[k, dia]))
  cat(sprintf("\n-- %s (converged: %s) --\n", tag, f$conv))
  x <- eff(f$beta, f$se_beta, mm)
  print(data.frame(predictor = x$predictor, direction = x$ratio, spread = eff(f$gamma, f$se_gamma, mm)$ratio), row.names = FALSE)
  cat(sprintf("dialogic, relative risk ratio against the four middle roles: Master %s | Object %s\n", rr("Master"), rr("Object")))
}
overlap_free("place_indep", "dialogic", "(a) place agency = independence facet, full dialogic motive")
overlap_free("place_indep", "dia_animals", "(b) place agency = independence facet, dialogic = 'animals to meet' only")
cat("(columns: direction = odds ratio for a role further from Master; spread = ratio, above 1 = more choices at both ends)\n")

saveRDS(list(wanted = list(direction = eff(B$beta, B$se_beta, main), spread = eff(B$gamma, B$se_gamma, main)),
             wanted_style = list(direction = eff(C$beta, C$se_beta, c(main, style)), spread = eff(C$gamma, C$se_gamma, c(main, style))),
             seen_style = list(direction = eff(S$beta, S$se_beta, main), spread = eff(S$gamma, S$se_gamma, main)),
             three_category = t5, gamma = C$gamma, se_gamma = C$se_gamma, beta = C$beta, se_beta = C$se_beta),
        "hnr_direction_spread.rds")
cat("\nSaved: hnr_direction_spread.rds\n")
