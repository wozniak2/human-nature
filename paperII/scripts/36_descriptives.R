# ============================================================
# 36_descriptives.R
# Descriptive statistics for the manuscript's Method section.
#
#   1. the six samples: n, gender, age, education (three harmonised levels)
#   2. the four main scales, pooled: items, range, mean, SD, reliability
#      (Cronbach's alpha, and alpha from polychoric correlations because the
#      items are ordinal), and their correlations with each other and with
#      the position of the role seen and the role wanted
#
# The scale descriptives are pooled on purpose. Place agency is not scalar
# invariant (script 22), so country means on it cannot be compared and are
# not tabulated.
#
# Requires: 01, 04, 13 already run. Packages: psych.
# ============================================================

suppressMessages(library(psych))
dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
mp  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
ctl <- grep("^ctl_", names(dat), value = TRUE)
pct <- function(x) 100 * mean(x, na.rm = TRUE)

cat("respondents:", nrow(dat), "| with age and gender (the sample used in the models):",
    sum(complete.cases(dat[, c("age_num", "gender_bin")])), "\n")

cat("\n========== 1. The six samples ==========\n")
cat("gender as answered (1 woman, 2 man, 3 transgender, 4 non-binary, 5 prefer not to answer):\n")
print(table(dat$country, dat$gender_raw, useNA = "ifany"))
cat("\nage as answered (1 = 18-25, 2 = 26-35, 3 = 36-45, 4 = 46-55, 5 = 56-65, 6 = 65+, 7 = prefer not to answer):\n")
print(table(dat$country, dat$age_raw, useNA = "ifany"))

one <- function(d) data.frame(
  n = nrow(d),
  women = pct(d$gender_raw == 1), men = pct(d$gender_raw == 2),
  other_or_ns = sum(!d$gender_raw %in% 1:2),
  age_18_35 = pct(d$age_raw %in% 1:2), age_36_55 = pct(d$age_raw %in% 3:4), age_56plus = pct(d$age_raw %in% 5:6),
  age_ns = sum(!d$age_raw %in% 1:6),
  edu_primary = pct(d$edu_primary == 1),
  edu_highschool = pct(d$edu_primary == 0 & d$edu_higher == 0 & d$edu_na == 0),
  edu_higher = pct(d$edu_higher == 1), edu_ns = sum(d$edu_na == 1))
smp <- do.call(rbind, c(lapply(split(dat, dat$country), one), list(All = one(dat))))
cat("\nshares in % of all respondents in the sample; other_or_ns, age_ns and edu_ns are counts\n")
print(round(smp, 1))
cat("(Age: 18-35 = the two youngest answer bands, 36-55 the middle two, 56+ the two oldest.\n",
    "Education: three harmonised levels, see 04_mimic_model.R; shares do not reach 100 where some did not state it.)\n")

cat("\n========== 2. The four main scales, pooled ==========\n")
items <- list("Place agency" = mp, "Restorative motive" = g$restorative, "Dialogic motive" = g$dialogic,
              "Societal control" = ctl)
sc <- as.data.frame(lapply(items, function(v) rowMeans(dat[, v])), check.names = FALSE)
al <- function(v) suppressWarnings(suppressMessages(psych::alpha(dat[, v], check.keys = FALSE)$total$raw_alpha))
oal <- function(v) suppressWarnings(suppressMessages(
  psych::alpha(polychoric(dat[, v])$rho, check.keys = FALSE)$total$raw_alpha))
desc <- data.frame(items = lengths(items),
                   scale_min = sapply(items, function(v) min(dat[, v], na.rm = TRUE)),
                   scale_max = sapply(items, function(v) max(dat[, v], na.rm = TRUE)),
                   n = sapply(sc, function(x) sum(!is.na(x))),
                   mean = sapply(sc, mean, na.rm = TRUE), sd = sapply(sc, sd, na.rm = TRUE),
                   alpha = sapply(items, al), ordinal_alpha = sapply(items, oal))
print(round(desc, 2))
cat("reliabilities to three decimals:\n"); print(round(desc[, c("alpha", "ordinal_alpha")], 3))
cat("(Scores are item means. Higher = more agency granted to the place; more agreement with the motive;\n",
    "less human control over natural entities. alpha = Cronbach; ordinal_alpha = from polychoric correlations.)\n")

cat("\ncorrelations among the scales (Pearson, all respondents):\n")
R <- cor(sc, use = "pairwise.complete.obs")
print(round(R, 2))

cat("\ncorrelation of each scale with the position of the role, Master (1) to Object (6) (Spearman):\n")
rr <- t(sapply(sc, function(x) c(seen = cor(x, dat$typ_now, method = "spearman", use = "pairwise.complete.obs"),
                                 wanted = cor(x, dat$typ_should, method = "spearman", use = "pairwise.complete.obs"))))
print(round(rr, 2))
cat(sprintf("role seen with role wanted (Spearman): %.2f\n",
            cor(dat$typ_now, dat$typ_should, method = "spearman", use = "pairwise.complete.obs")))

cat("\nthe same correlations with country differences removed (scores centred within country):\n")
cen <- function(x) x - ave(x, dat$country, FUN = function(z) mean(z, na.rm = TRUE))
scc <- as.data.frame(lapply(sc, cen), check.names = FALSE)
print(round(cor(scc, use = "pairwise.complete.obs"), 2))
rrc <- t(sapply(scc, function(x) c(seen = cor(x, cen(dat$typ_now), method = "spearman", use = "pairwise.complete.obs"),
                                   wanted = cor(x, cen(dat$typ_should), method = "spearman", use = "pairwise.complete.obs"))))
print(round(rrc, 2))

saveRDS(list(sample = smp, scales = desc, cor = R, cor_role = rr), "hnr_descriptives.rds")
cat("\nSaved: hnr_descriptives.rds\n")
