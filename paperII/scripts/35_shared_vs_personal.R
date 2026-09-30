# ============================================================
# 35_shared_vs_personal.R
# Is the role people SEE more shared, and the role they WANT more personal?
#
# "Seeing is societal, wanting is personal" was an interpretation. This script
# turns it into four measured indicators, each compared between the two answers
# with a bootstrap interval for the difference (respondents resampled within
# country, both answers kept together):
#
#   1. agreement     people in the same country agree more on what they see
#                    (concentration of the answers: agreement probability and
#                    normalised entropy)
#   2. country       the role seen differs more between countries (Cramer's V;
#                    share of variance in choosing Master; spread of Master shares)
#   3. personal ties the role seen is less predictable from personal predictors
#                    (place agency, motives, age, gender, education)
#   4. societal view the role seen tracks beliefs about societal control more,
#                    and place agency less
#
# Section 5 recomputes, from the microdata, the country figures that earlier
# notes derived by hand from rounded percentages (exit and entry rates,
# counterfactual gaps, the link between the gap and the level of seen mastery,
# the share of extreme choices that are Master).
#
# Requires: 01, 04, 13 already run. Packages: MASS.
# ============================================================

suppressMessages(library(MASS))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
zs <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
dat$place <- zs(rowMeans(dat[, mp])); dat$restful <- zs(rowMeans(dat[, g$restorative]))
dat$dialogic <- zs(rowMeans(dat[, g$dialogic])); dat$control <- zs(dat$control_mean)
roles <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")
ctry <- levels(dat$country)
set.seed(20260930)
strat <- split(seq_len(nrow(dat)), dat$country)
resample <- function(idx_list) unlist(lapply(idx_list, function(i) sample(i, replace = TRUE)), use.names = FALSE)
ci <- function(b) quantile(b, c(.025, .975), na.rm = TRUE)
row <- function(name, seen, wanted, b) data.frame(indicator = name, seen = round(seen, 3), wanted = round(wanted, 3),
  difference = sprintf("%+.3f [%+.3f, %+.3f]", seen - wanted, ci(b)[1], ci(b)[2]), row.names = NULL)

cat("respondents:", nrow(dat), "(all), of whom", sum(complete.cases(dat[, c("age_num", "gender_bin")])), "with complete demographics\n")

# ---------------- 1. agreement within countries ----------------
cat("\n========== 1. Do people in the same country agree more on what they see? ==========\n")
agree <- function(x, cty) {            # chance that two people from the same country give the same answer
  w <- table(cty); p <- sapply(split(x, cty), function(v) sum((table(factor(v, 1:6)) / length(v))^2)); sum(p * w[names(p)]) / sum(w) }
entropy <- function(x, cty) {          # normalised Shannon entropy, averaged over countries (1 = answers spread evenly)
  w <- table(cty); h <- sapply(split(x, cty), function(v) { p <- table(factor(v, 1:6)) / length(v); p <- p[p > 0]; -sum(p * log(p)) / log(6) })
  sum(h * w[names(h)]) / sum(w) }
b1 <- replicate(1000, { i <- resample(strat); c(agree(dat$typ_now[i], dat$country[i]) - agree(dat$typ_should[i], dat$country[i]),
                                                 entropy(dat$typ_now[i], dat$country[i]) - entropy(dat$typ_should[i], dat$country[i])) })
t1 <- rbind(row("agreement probability (same country)", agree(dat$typ_now, dat$country), agree(dat$typ_should, dat$country), b1[1, ]),
            row("normalised entropy (lower = more concentrated)", entropy(dat$typ_now, dat$country), entropy(dat$typ_should, dat$country), b1[2, ]))
print(t1, row.names = FALSE)
pc <- t(sapply(ctry, function(k) { s <- dat$country == k
  c(agree_seen = agree(dat$typ_now[s], dat$country[s, drop = TRUE]), agree_wanted = agree(dat$typ_should[s], dat$country[s, drop = TRUE])) }))
cat("\nby country (agreement probability):\n"); print(round(pc, 3))
cat(sprintf("the role seen is the more concentrated answer in %d of 6 countries\n", sum(pc[, 1] > pc[, 2])))
cat("pooled distributions (%):\n")
print(round(100 * rbind(seen = table(factor(dat$typ_now, 1:6, roles)), wanted = table(factor(dat$typ_should, 1:6, roles))) / nrow(dat), 1))

# ---------------- 2. differences between countries ----------------
cat("\n========== 2. Does the role seen differ more between countries? ==========\n")
cramer <- function(x, cty) { t <- table(cty, factor(x, 1:6)); t <- t[, colSums(t) > 0, drop = FALSE]
  sqrt(suppressWarnings(chisq.test(t, correct = FALSE)$statistic) / (sum(t) * (min(dim(t)) - 1))) }
eta2 <- function(y, cty) { a <- anova(lm(y ~ cty)); a[1, 2] / sum(a[, 2]) }
sd_master <- function(x, cty) sd(100 * tapply(x == 1, cty, mean))
b2 <- replicate(1000, { i <- resample(strat); cc <- dat$country[i]; n_ <- dat$typ_now[i]; w_ <- dat$typ_should[i]
  c(cramer(n_, cc) - cramer(w_, cc), eta2(as.numeric(n_ == 1), cc) - eta2(as.numeric(w_ == 1), cc),
    eta2(n_, cc) - eta2(w_, cc), sd_master(n_, cc) - sd_master(w_, cc)) })
t2 <- rbind(row("Cramer's V, country by role", cramer(dat$typ_now, dat$country), cramer(dat$typ_should, dat$country), b2[1, ]),
            row("share of variance in choosing Master due to country", eta2(as.numeric(dat$typ_now == 1), dat$country), eta2(as.numeric(dat$typ_should == 1), dat$country), b2[2, ]),
            row("share of variance in role position due to country", eta2(dat$typ_now, dat$country), eta2(dat$typ_should, dat$country), b2[3, ]),
            row("SD of the Master share across countries (points)", sd_master(dat$typ_now, dat$country), sd_master(dat$typ_should, dat$country), b2[4, ]))
print(t2, row.names = FALSE)
cat("(The bootstrap adds sampling noise to every country, so resampled between-country measures are slightly\n")
cat(" inflated for both answers alike; the interval is for the DIFFERENCE, where that inflation largely cancels.)\n")

# ---------------- 3 and 4. personal predictors, societal control, place agency ----------------
d <- dat[complete.cases(dat[, c("typ_now", "typ_should", "age_num", "gender_bin", "place", "restful", "dialogic", "control")]), ]
strat_d <- split(seq_len(nrow(d)), d$country)
personal <- "place + restful + dialogic + age_num + gender_bin + edu_primary + edu_higher + edu_na"
full     <- paste("place + restful + dialogic + control + age_num + gender_bin + edu_primary + edu_higher + edu_na + country")
fitr2 <- function(y, rhs, data) { data$yy <- factor(data[[y]], levels = 1:6, ordered = TRUE)
  f <- polr(as.formula(paste("yy ~", rhs)), data); f0 <- polr(yy ~ 1, data); 1 - as.numeric(logLik(f)) / as.numeric(logLik(f0)) }
coefs <- function(y, data) { data$yy <- factor(data[[y]], levels = 1:6, ordered = TRUE); coef(polr(as.formula(paste("yy ~", full)), data))[c("control", "place")] }
cat("\n========== 3. Is the role seen less predictable from personal predictors? ==========\n")
cat("(ordered logit, McFadden pseudo-R2; personal predictors = place agency, the two motives, age, gender, education; no country)\n")
b3 <- replicate(300, { i <- resample(strat_d); x <- d[i, ]
  cn <- coefs("typ_now", x); cw <- coefs("typ_should", x)
  c(fitr2("typ_now", personal, x) - fitr2("typ_should", personal, x), cn["control"] - cw["control"], cn["place"] - cw["place"]) })
t3 <- row("pseudo-R2 from personal predictors", fitr2("typ_now", personal, d), fitr2("typ_should", personal, d), b3[1, ])
print(t3, row.names = FALSE)

cat("\n========== 4. Does the role seen track societal-control beliefs more, and place agency less? ==========\n")
cat("(ordered logit with all predictors and country; log-odds per SD, positive = a role further from Master)\n")
cn <- coefs("typ_now", d); cw <- coefs("typ_should", d)
t4 <- rbind(row("societal control (general belief)", cn["control"], cw["control"], b3[2, ]),
            row("place agency (personal tie)", cn["place"], cw["place"], b3[3, ]))
print(t4, row.names = FALSE)
cat(sprintf("as odds ratios: societal control %.2f (seen) vs %.2f (wanted) | place agency %.2f (seen) vs %.2f (wanted)\n",
            exp(cn["control"]), exp(cw["control"]), exp(cn["place"]), exp(cw["place"])))
cat("(See also script 33: societal control carries 30% of the explained variation for the role seen and 8% for the role wanted.)\n")

cat("\n========== Summary: four indicators, one direction? ==========\n")
summ <- data.frame(indicator = c("1 agreement within country", "2 difference between countries (Cramer's V)", "3 predictability from personal predictors", "4 weight of societal-control beliefs"),
  seen = c(t1$seen[1], t2$seen[1], t3$seen, t4$seen[1]), wanted = c(t1$wanted[1], t2$wanted[1], t3$wanted, t4$wanted[1]),
  difference = c(t1$difference[1], t2$difference[1], t3$difference, t4$difference[1]),
  says_seeing_is_more_shared = c(t1$seen[1] > t1$wanted[1], t2$seen[1] > t2$wanted[1], t3$seen < t3$wanted, t4$seen[1] > t4$wanted[1]),
  interval_excludes_zero = c(prod(ci(b1[1, ])) > 0, prod(ci(b2[1, ])) > 0, prod(ci(b3[1, ])) > 0, prod(ci(b3[2, ])) > 0))
print(summ, row.names = FALSE)

# ---------------- 5. country figures recomputed from the microdata ----------------
cat("\n========== 5. Country figures recomputed from the microdata ==========\n")
cty_tab <- t(sapply(ctry, function(k) { x <- dat[dat$country == k, ]; sm <- x$typ_now == 1; wm <- x$typ_should == 1
  c(n = nrow(x), seen_master = 100 * mean(sm), wanted_master = 100 * mean(wm), net_gap = 100 * (mean(sm) - mean(wm)),
    exit_rate = 100 * mean(!wm[sm]), entry_rate = 100 * mean(wm[!sm])) }))
pool_exit <- mean(dat$typ_should[dat$typ_now == 1] != 1); pool_entry <- mean(dat$typ_should[dat$typ_now != 1] == 1)
cty_tab <- cbind(cty_tab, gap_at_pooled_rates = cty_tab[, "seen_master"] * pool_exit - (100 - cty_tab[, "seen_master"]) * pool_entry)
print(round(cty_tab, 1))
cat(sprintf("pooled exit rate (leave Master) %.1f%% | pooled entry rate (move to Master) %.1f%%\n", 100 * pool_exit, 100 * pool_entry))
cat("(gap_at_pooled_rates = the net gap a country would have with its own level of seen mastery but the pooled exit and entry rates)\n")
cat(sprintf("across the six countries, the net gap correlates %.2f with the share seeing Master and %.2f with the share wanting it (Spearman; n = 6, descriptive only)\n",
            cor(cty_tab[, "net_gap"], cty_tab[, "seen_master"], method = "spearman"), cor(cty_tab[, "net_gap"], cty_tab[, "wanted_master"], method = "spearman")))
ent <- chisq.test(table(dat$country[dat$typ_now != 1], dat$typ_should[dat$typ_now != 1] == 1)); ext <- chisq.test(table(dat$country[dat$typ_now == 1], dat$typ_should[dat$typ_now == 1] != 1))
cat(sprintf("do the rates differ between countries? exit: chi2(5) = %.1f, p = %.3g | entry: chi2(5) = %.1f, p = %.3g\n", ext$statistic, ext$p.value, ent$statistic, ent$p.value))
ex_w <- dat$typ_should %in% c(1, 6); ex_s <- dat$typ_now %in% c(1, 6)
cat(sprintf("extreme choices that are Master: wanted %d of %d (%.1f%%) | seen %d of %d (%.1f%%)\n",
            sum(dat$typ_should == 1), sum(ex_w), 100 * mean(dat$typ_should[ex_w] == 1), sum(dat$typ_now == 1), sum(ex_s), 100 * mean(dat$typ_now[ex_s] == 1)))

saveRDS(list(agreement = t1, by_country = pc, country = t2, personal = t3, control_place = t4, summary = summ, country_table = cty_tab),
        "hnr_shared_vs_personal.rds")
cat("\nSaved: hnr_shared_vs_personal.rds\n")
