# ============================================================
# 30_indicator_connections.R
# How does the role item relate to every other indicator in the questionnaire?
#
# For each indicator a summary score is built and correlated (Spearman) with
#   - the role SEEN now (position 1 Master ... 6 Object),
#   - the role WANTED,
#   - the shift (wanted minus seen; negative = moves toward Master),
#   - extremity of the wanted role (either pole = 1).
# Everything is adjusted for country first (scores and role are residualised on
# country dummies), so that differences in level between countries do not
# create or hide a link. Then the indicators are correlated with each other, to
# show which ones move together.
#
# Indicators (questionnaire numbers): Q1 relational entanglement; Q2/Q3 agency of
# non-human beings now/should; Q4 societal control; Q7-Q10 and Q12 place agency;
# Q11 destroyed-place stances; Q13 motives (restful, dialogic, serviced);
# Q14 Indigenous-knowledge stance; Q15 climate stances; Q16 tourism and
# Indigenous communities.
#
# Requires: 01, 13 already run. No extra packages.
# ============================================================

dat <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
stopifnot("ik_choice" %in% names(dat))
nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
g <- re$groups
names(g) <- vapply(g, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
  unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
rm_ <- function(cols) rowMeans(dat[, cols, drop = FALSE], na.rm = TRUE)

q1 <- c("presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
        "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person")
S <- data.frame(
  `Q1 Relational entanglement`         = rm_(q1),
  `Q2 Agency of non-humans, now`       = rm_(grep("^ag_now_", names(dat), value = TRUE)),
  `Q3 Agency of non-humans, should`    = rm_(grep("^ag_fut_", names(dat), value = TRUE)),
  `Q4 Societal control`                = dat$control_mean,
  `Q7-12 Place agency`                 = rm_(c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")),
  `Q11 Not a message (destroyed place)`= dat$sc_not_message,
  `Q11 Technology prevents it`         = dat$sc_tech_prevents,
  `Q11 Heed its signals`               = dat$sc_heed_signals,
  `Q11 Nature is a partner`            = dat$sc_nature_partner,
  `Q11 Humans always lose`             = dat$sc_humans_lose,
  `Q13 Restful motive`                 = rm_(g$restorative),
  `Q13 Dialogic motive`                = rm_(g$dialogic),
  `Q13 Serviced motive`                = rm_(g$serviced),
  `Q14 Indigenous knowledge: should be shared` = as.numeric(dat$ik_choice == 1),
  `Q14 Indigenous knowledge: mostly myth`      = as.numeric(dat$ik_choice == 3),
  `Q15 Climate: denial`                = dat$cl_denial,
  `Q15 Climate: technology can stop it`= dat$cl_tech_stops,
  `Q15 Climate: cannot be stopped`     = dat$cl_cannot_stop,
  `Q15 Climate: will end humanity`     = dat$cl_extinction,
  `Q16 Tourism: favourable (3 items)`  = rm_(c("tour_respect", "tour_benefit", "tour_educates")),
  `Q16 Tourism: critical (3 items)`    = rm_(c("tour_ban", "tour_money", "tour_destroys")),
  check.names = FALSE)
R <- data.frame(seen = dat$typ_now, wanted = dat$typ_should)
R$shift <- R$wanted - R$seen
R$ext_wanted <- as.integer(dat$typ_should %in% c(1, 6))
cat("respondents:", nrow(dat), "| Q14 answered:", sum(!is.na(dat$ik_choice)), "| Q16 answered:", sum(!is.na(S[[20]])), "\n")
cat("Q14 choices (1 shared, 2 sometimes useful, 3 myth):", paste(names(table(dat$ik_choice)), table(dat$ik_choice), collapse = " | "), "\n")

# adjust everything for country (residuals from country dummies)
adj <- function(x) { ok <- !is.na(x); r <- rep(NA_real_, length(x)); r[ok] <- resid(lm(x[ok] ~ dat$country[ok])); r }
Sa <- as.data.frame(lapply(S, adj), check.names = FALSE); Ra <- as.data.frame(lapply(R, adj))
cc <- function(x, y) { ok <- complete.cases(x, y); if (sum(ok) < 50) return(c(NA, NA)); t <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman")); c(unname(t$estimate), t$p.value) }

cat("\n========== 1. Each indicator against the role seen, wanted, the shift and extremity ==========\n")
cat("(Spearman rho after removing country differences; * = p < .001, + = p < .05)\n")
tab <- do.call(rbind, lapply(names(Sa), function(v) {
  o <- lapply(names(Ra), function(r) cc(Sa[[v]], Ra[[r]]))
  data.frame(indicator = v, seen = o[[1]][1], wanted = o[[2]][1], shift = o[[3]][1], extreme = o[[4]][1],
             p_seen = o[[1]][2], p_wanted = o[[2]][2], p_shift = o[[3]][2], p_ext = o[[4]][2])
}))
star <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "*", ifelse(p < .05, "+", "")))
show <- data.frame(indicator = tab$indicator,
  seen = sprintf("%+.2f%s", tab$seen, star(tab$p_seen)), wanted = sprintf("%+.2f%s", tab$wanted, star(tab$p_wanted)),
  shift = sprintf("%+.2f%s", tab$shift, star(tab$p_shift)), extreme_wanted = sprintf("%+.2f%s", tab$extreme, star(tab$p_ext)))
print(show, row.names = FALSE, right = FALSE)
o <- order(-abs(tab$wanted)); cat("\nStrongest links to the role WANTED:\n")
print(data.frame(indicator = tab$indicator[o][1:6], rho = round(tab$wanted[o][1:6], 2)), row.names = FALSE)
o <- order(-abs(tab$seen)); cat("\nStrongest links to the role SEEN:\n")
print(data.frame(indicator = tab$indicator[o][1:6], rho = round(tab$seen[o][1:6], 2)), row.names = FALSE)

cat("\n========== 2. Q14: which statement about Indigenous knowledge, by the role wanted ==========\n")
print(round(prop.table(table(role_wanted = factor(dat$typ_should, 1:6, c("Master", "Manager", "User", "Guardian", "Partner", "Object")),
                               statement = factor(dat$ik_choice, 1:3, c("should be shared", "sometimes useful", "mostly myth"))), 1), 2))
t14 <- suppressWarnings(chisq.test(table(dat$typ_should, dat$ik_choice)))
cat(sprintf("chi-squared = %.1f, df = %d, p = %.3g\n", t14$statistic, t14$parameter, t14$p.value))

cat("\n========== 3. How the indicators relate to each other (country-adjusted Pearson) ==========\n")
keep <- c("Q1 Relational entanglement", "Q2 Agency of non-humans, now", "Q3 Agency of non-humans, should", "Q4 Societal control",
          "Q7-12 Place agency", "Q13 Restful motive", "Q13 Dialogic motive", "Q13 Serviced motive",
          "Q15 Climate: denial", "Q16 Tourism: favourable (3 items)", "Q16 Tourism: critical (3 items)")
M <- round(cor(Sa[, keep], use = "pairwise.complete.obs"), 2)
short <- c("Q1 relational", "Q2 agency now", "Q3 agency should", "Q4 control", "Q7-12 place agency", "Q13 restful", "Q13 dialogic", "Q13 serviced", "Q15 denial", "Q16 favourable", "Q16 critical")
dimnames(M) <- list(short, sub("^Q", "", short)); print(M)

saveRDS(list(table = tab, matrix = M), "hnr_indicator_connections.rds")
cat("\nSaved: hnr_indicator_connections.rds\n")
