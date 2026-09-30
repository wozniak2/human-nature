# ============================================================
# 10_figures.R
# Figures for the perceived-vs-ideal role analysis (ggplot2).
#
# Figures are written to figures/ as PNG (to look at) and PDF (to submit).
#
# Fig 1  now -> should slopes, six roles x six countries (headline)
# Fig 2  net gap in mastery with 95% CIs, countries ordered
# Fig 3  odds ratios: perceived mastery, ideal mastery, rejecting it
# Fig 4  belief scale, raw country means vs invariance-corrected means
#
# Requires: 01, 02 and 08 already run. Packages: ggplot2, lavaan.
# ============================================================

suppressMessages({ library(ggplot2); library(ggalluvial); library(lavaan) })

dat  <- readRDS("hnr_data.rds")
fits <- readRDS("hnr_cfa_fits.rds")
# 08 builds these but does not save them back into hnr_data.rds
dat$master_now    <- as.integer(dat$typ_now == 1)
dat$master_should <- as.integer(dat$typ_should == 1)

typ  <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")
ctry <- levels(dat$country)

# --- theme ---------------------------------------------------------------
# Publication figures: white ground, quiet grid, colour used sparingly.
# Master and Guardian carry the argument and keep the two strong hues;
# the other four roles are deliberately muted. Okabe-Ito, colour-blind safe.
pal <- c(Master = "#D55E00", Manager = "#0072B2", User = "#E69F00",
         Guardian = "#009E73", Partner = "#CC79A7", Object = "#56B4E9")
INK <- "#1A1A1A"; MUTE <- "#5E5E5E"; GRID <- "#EAEAEA"; BG <- "#FFFFFF"
ACCENT <- "#D55E00"; BLUE <- "#0072B2"; GREEN <- "#009E73"

hnr_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = BG, colour = NA),
      panel.background  = element_rect(fill = BG, colour = NA),
      legend.key        = element_rect(fill = BG, colour = NA),
      panel.grid.major  = element_line(colour = GRID, linewidth = .35),
      panel.grid.minor  = element_blank(),
      text          = element_text(colour = INK),
      axis.text     = element_text(colour = MUTE),
      axis.title    = element_text(colour = MUTE, size = 10.5),
      strip.text    = element_text(colour = INK, size = 11.5, hjust = 0),
      plot.title    = element_text(colour = INK, size = 13),
      plot.subtitle = element_text(colour = MUTE, size = 10),
      plot.caption  = element_text(colour = MUTE, size = 9, hjust = 0),
      legend.position = "bottom", legend.title = element_blank(),
      plot.margin = margin(12, 16, 10, 12)
    )
}

# PNG to look at, PDF (vector) for submission
save_fig <- function(name, build, w, h) {
  dir.create("figures", showWarnings = FALSE)
  p <- build()
  ggsave(file.path("figures", paste0(name, ".png")), p, width = w, height = h,
         dpi = 300, bg = BG)
  ggsave(file.path("figures", paste0(name, ".pdf")), p, width = w, height = h,
         bg = BG)
  cat("wrote figures/", name, ".png and .pdf\n", sep = "")
}

# --- path diagrams: a small ggplot2 engine (sem_plot_helpers.R) draws estimates read from the lavaan fits
source("sem_plot_helpers.R")
save_sem <- function(g, name, w, h) {
  dir.create("figures", showWarnings = FALSE)
  ggsave(file.path("figures", paste0(name, ".png")), g, width = w, height = h, dpi = 300, bg = BG)
  ggsave(file.path("figures", paste0(name, ".pdf")), g, width = w, height = h, bg = BG)
  cat("wrote figures/", name, ".png and .pdf\n", sep = "")
}

# --- Fig 1: now -> should slopes -----------------------------------------
d1 <- do.call(rbind, lapply(ctry, function(cn) {
  x <- subset(dat, country == cn)
  data.frame(country = cn, role = rep(typ, 2),
             when = rep(c("is now", "should be"), each = 6),
             pct = 100 * c(prop.table(table(factor(x$typ_now, 1:6))),
                           prop.table(table(factor(x$typ_should, 1:6)))))
}))
d1$role    <- factor(d1$role, levels = typ)
d1$country <- factor(d1$country, levels = ctry)
d1$when    <- factor(d1$when, levels = c("is now", "should be"))
d1$lead    <- d1$role %in% c("Master", "Guardian")

fig1 <- function() {
  ggplot(d1, aes(when, pct, group = role, colour = role)) +
    geom_line(aes(linewidth = lead, alpha = lead)) +
    geom_point(aes(alpha = lead), size = 1.5) +
    facet_wrap(~country, nrow = 2) +
    scale_colour_manual(values = pal) +
    scale_linewidth_manual(values = c("FALSE" = .5, "TRUE" = 1.5), guide = "none") +
    scale_alpha_manual(values = c("FALSE" = .7, "TRUE" = 1), guide = "none") +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    guides(colour = guide_legend(nrow = 1,
           override.aes = list(linewidth = 1.2, alpha = 1))) +
    labs(x = NULL, y = "% of respondents",
         title = "The role people see, and the role they would choose",
         subtitle = "Guardianship rises in all six countries; mastery falls in every country except Canada",
         caption = "Master and Guardian emphasised. Source: 08_mastery_paradox.R") +
    hnr_theme()
}
save_fig("fig1_now_should_slopes", fig1, 10, 6)

# --- Fig 2: net gap with 95% CI ------------------------------------------
gap <- dat$master_now - dat$master_should
d2 <- do.call(rbind, lapply(split(gap, dat$country), function(v) {
  se <- sd(v) / sqrt(length(v))
  data.frame(est = 100 * mean(v), lo = 100 * (mean(v) - 1.96 * se),
             hi = 100 * (mean(v) + 1.96 * se))
}))
d2$country  <- rownames(d2)
d2$detected <- d2$lo > 0

fig2 <- function() {
  ggplot(d2, aes(est, reorder(country, est), colour = detected)) +
    geom_vline(xintercept = 0, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.1) +
    geom_point(size = 2.6) +
    scale_colour_manual(values = c("FALSE" = MUTE, "TRUE" = ACCENT),
                        guide = "none") +
    labs(x = "Net change in mastery, now minus should (percentage points)", y = NULL,
         title = "How far each country moves away from mastery",
         subtitle = "Bars are 95% confidence intervals; the Canada interval crosses zero and overlaps Sweden",
         caption = "Source: 08_mastery_paradox.R, sections 2 and 10") +
    hnr_theme()
}
save_fig("fig2_gap_ci", fig2, 8, 4.6)

# --- Fig 3: odds ratios --------------------------------------------------
or_of <- function(m, label) {
  s <- coef(summary(m))[-1, , drop = FALSE]
  d <- data.frame(term = rownames(s), or = exp(s[, 1]),
                  lo = exp(s[, 1] - 1.96 * s[, 2]), hi = exp(s[, 1] + 1.96 * s[, 2]))
  # Canada has no coefficient: it is the reference every OR is measured
  # against, so show it at 1 rather than leaving the anchor implied.
  d <- rbind(data.frame(term = "Canada (ref.)", or = 1, lo = NA, hi = NA), d)
  d$model <- label
  d
}
mn <- subset(dat, typ_now == 1); mn$rej <- as.integer(mn$typ_should != 1)
d3 <- rbind(
  or_of(glm(master_now ~ country + age_num + gender_bin, binomial, dat),
        "Perceives mastery"),
  or_of(glm(master_should ~ country + age_num + gender_bin, binomial, dat),
        "Wants mastery"),
  or_of(glm(rej ~ country + age_num + gender_bin, binomial, mn),
        "Rejects it, of those who perceive it"))
d3$term <- sub("^country", "", d3$term)
d3$term <- sub("age_num", "Age (per band)", d3$term)
d3$term <- sub("gender_bin", "Man (vs woman)", d3$term)
lab <- c("Canada (ref.)", ctry[-1], "Age (per band)", "Man (vs woman)")
d3$term  <- factor(d3$term, levels = rev(lab))
d3$model <- factor(d3$model, levels = unique(d3$model))
d3$kind  <- ifelse(is.na(d3$lo), "ref",
             ifelse(d3$lo > 1 | d3$hi < 1, "sig", "ns"))

fig3 <- function() {
  ggplot(d3, aes(or, term, colour = kind)) +
    geom_vline(xintercept = 1, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_hline(yintercept = 2.5, colour = GRID, linewidth = .5) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1, na.rm = TRUE) +
    geom_point(aes(shape = kind), size = 2.2, na.rm = TRUE, fill = BG) +
    facet_wrap(~model, nrow = 1) +
    scale_x_log10(limits = c(.35, 4.3), breaks = c(.5, 1, 2, 4)) +
    scale_colour_manual(values = c(ref = MUTE, ns = MUTE,
                                   sig = BLUE), guide = "none") +
    scale_shape_manual(values = c(ref = 21, ns = 19, sig = 19), guide = "none") +
    labs(x = "Odds ratio (log scale)", y = NULL,
         title = "Where countries differ: in what people see, not in what they want",
         subtitle = "Coloured intervals exclude 1; the hollow point marks the reference category",
         caption = "Reference: Canada, women. Third panel covers only the 776 respondents who perceive mastery.") +
    hnr_theme()
}
save_fig("fig3_odds_ratios", fig3, 10.5, 4.8)

# --- Fig 4: belief scale, raw vs corrected -------------------------------
raw <- tapply(dat$relational, dat$country, mean)
pe  <- parameterEstimates(fits$partial)
lat <- pe[pe$op == "~1" & pe$lhs == "relational_f", ]
d4  <- data.frame(country = ctry,
                  raw = as.numeric(raw[ctry] - raw["Canada"]),
                  corrected = lat$est[match(seq_along(ctry), lat$group)])

fig4 <- function() {
  ggplot(d4, aes(y = reorder(country, raw))) +
    geom_vline(xintercept = 0, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_segment(aes(x = raw, xend = corrected, yend = reorder(country, raw)),
                 colour = MUTE, linewidth = .5,
                 arrow = arrow(length = unit(.07, "in"), type = "closed")) +
    geom_point(aes(x = raw, shape = "raw mean"), size = 2.4,
               colour = MUTE, fill = BG) +
    geom_point(aes(x = corrected, shape = "corrected for non-invariance"),
               size = 2.4, colour = GREEN) +
    scale_shape_manual(values = c("raw mean" = 21,
                                  "corrected for non-invariance" = 19)) +
    labs(x = "Difference from Canada (scale points)", y = NULL,
         title = "Belief scale: what the invariance correction changes",
         subtitle = "Every country shifts a little; only the Netherlands and Sweden differ from Canada",
         caption = "Source: 02_cfa_invariance.R, partial scalar model") +
    hnr_theme()
}
save_fig("fig4_raw_vs_corrected", fig4, 8, 4.6)


# --- Fig 5: alluvial, where individuals actually move -------------------
# Fig 1 shows net shares; this shows the moves behind them. Master and
# Guardian ribbons are saturated and every other origin is grey, matching
# the emphasis used in Fig 1. Strata are reversed so the largest group
# (Master) sits at the bottom, where its fan reads without crossing down.
d5 <- as.data.frame(table(now = factor(dat$typ_now, 1:6, typ),
                          should = factor(dat$typ_should, 1:6, typ)),
                    responseName = "n")
d5$now    <- factor(d5$now, levels = rev(typ))
d5$should <- factor(d5$should, levels = rev(typ))
d5$hl <- ifelse(as.character(d5$now) %in% c("Master", "Guardian"),
                as.character(d5$now), "other")

fig5 <- function() {
  ggplot(d5, aes(y = n, axis1 = now, axis2 = should)) +
    geom_alluvium(aes(fill = now, alpha = hl), width = .12, knot.pos = .32,
                  curve_type = "sigmoid") +
    geom_stratum(width = .12, fill = "grey97", colour = "grey55", linewidth = .4) +
    geom_text(stat = "stratum", size = 3.1, colour = INK,
              aes(label = paste0(after_stat(stratum), "  ", after_stat(count)))) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_alpha_manual(values = c(Master = .92, Guardian = .92, other = .68),
                       guide = "none") +
    scale_x_discrete(limits = c("Role they see now", "Role they think there should be"),
                     expand = expansion(mult = c(.16, .16))) +
    labs(x = NULL, y = "Respondents",
         title = "Where people move between the role they see and the role they want",
         subtitle = "Each ribbon is coloured by the role people see now; Master and Guardian are emphasised",
         caption = "Master shrinks from 776 to 496 and Guardian grows from 171 to 339: 480 respondents leave Master, 200 arrive (Bowker chi-squared = 184.5, p < .001).") +
    hnr_theme() +
    theme(panel.grid = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(colour = INK, size = 11))
}
save_fig("fig5_alluvial", fig5, 9, 6.4)

# --- Fig 5b (supplementary): the same flows as exact counts -------------
# Fig 1 shows net shares; this shows the moves behind them. The diagonal is
# outlined (same answer twice) and the two mastery margins are annotated,
# because the asymmetry between them is the paper's central evidence.
t6 <- table(now = factor(dat$typ_now, 1:6, typ), should = factor(dat$typ_should, 1:6, typ))
d5 <- as.data.frame(t6, responseName = "n")
d5$now    <- factor(d5$now, levels = rev(typ))
d5$should <- factor(d5$should, levels = typ)
d5$same   <- as.character(d5$now) == as.character(d5$should)
d5$pct_of_row <- 100 * d5$n / ave(d5$n, d5$now, FUN = sum)

fig5b <- function() {
  ggplot(d5, aes(should, now)) +
    geom_tile(aes(fill = pct_of_row), colour = BG, linewidth = .8) +
    geom_tile(data = subset(d5, same), fill = NA, colour = INK, linewidth = .7) +
    geom_text(aes(label = n, colour = pct_of_row > 32), size = 3.4) +
    scale_fill_gradient(low = "#F5F5F5", high = ACCENT, name = "% of row") +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = INK), guide = "none") +
    coord_fixed() +
    labs(x = "Role they think there should be", y = "Role they see now",
         title = "Transition matrix: exact counts behind Figure 5",
         subtitle = "Cells are respondents; shading is the share of each row. Outlined cells gave the same answer twice",
         caption = "480 respondents leave Master and 200 arrive at it (Bowker chi-squared = 184.5, p < .001). Source: 08_mastery_paradox.R") +
    hnr_theme() +
    theme(panel.grid.major = element_blank(), legend.position = "right",
          legend.title = element_text(colour = MUTE, size = 9))
}
save_fig("fig5b_transition_matrix", fig5b, 8.6, 6.4)

# --- Fig 6: the gap against questionnaire language ----------------------
# The paper cannot separate country from language; this shows both at once:
# the language ordering, and the spread between countries sharing a version.
d6 <- d2
d6$language <- dat$language[match(d6$country, dat$country)]
d6 <- d6[order(d6$language, d6$est), ]
# spread the countries within each language so their intervals do not overlap
d6$lx <- as.numeric(factor(d6$language))
d6$x  <- unlist(lapply(split(d6$lx, d6$language), function(v)
           v + if (length(v) == 1) 0 else seq(-.2, .2, length.out = length(v))))
lang_mean <- aggregate(est ~ language, d6, mean)
lang_mean$lx <- as.numeric(factor(lang_mean$language))

fig6 <- function() {
  ggplot(d6, aes(x, est)) +
    geom_hline(yintercept = 0, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_segment(data = lang_mean, aes(x = lx - .34, xend = lx + .34,
                 y = est, yend = est), colour = MUTE, linewidth = .6) +
    geom_linerange(aes(ymin = lo, ymax = hi, colour = language), linewidth = .8) +
    geom_point(aes(colour = language), size = 3) +
    geom_text(aes(label = country), vjust = -1.1, size = 3.2, colour = INK) +
    scale_colour_manual(values = c(English = BLUE, Polish = ACCENT,
                                   Spanish = GREEN), guide = "none") +
    scale_x_continuous(breaks = 1:3, labels = levels(factor(d6$language)),
                       limits = c(.5, 3.5)) +
    labs(x = "Language version of the questionnaire",
         y = "Gap: sees mastery minus wants mastery (pp)",
         title = "Country or language? The design cannot tell them apart",
         subtitle = "Rules mark the language mean; Polish was fielded in one country only",
         caption = "Countries sharing a version still differ, so country is not only language. Source: 08_mastery_paradox.R") +
    hnr_theme()
}
save_fig("fig6_gap_by_language", fig6, 8.6, 5.4)


# --- Fig 7: what the loglinear models estimate --------------------------
# Two parameters from 11_square_table_models.R, on a common log scale:
# how far each role is preferred over Master once the symmetric
# association is accounted for, and how strongly each role retains its
# own. Master is the reference for the first panel and sits at zero.
sq <- readRDS("hnr_square_table_models.rds")
sh <- rbind(sq$shift[, c("role", "log_shift", "se")],
            data.frame(role = "Master", log_shift = 0, se = NA))
names(sh) <- c("role", "est", "se")
sh$panel <- "Preferred over Master (quasi-symmetry margins)"
st <- sq$stay[, c("role", "log_odds_of_staying", "se")]
names(st) <- c("role", "est", "se")
st$panel <- "Tendency to keep the same role (quasi-independence)"
d7 <- rbind(sh, st)
d7$lo <- d7$est - 1.96 * d7$se
d7$hi <- d7$est + 1.96 * d7$se
d7$role <- factor(d7$role, levels = rev(c("Guardian", "Partner", "User",
                                          "Manager", "Object", "Master")))
d7$panel <- factor(d7$panel, levels = unique(d7$panel))

fig7 <- function() {
  ggplot(d7, aes(est, role, colour = role)) +
    geom_vline(xintercept = 0, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = .9, na.rm = TRUE) +
    geom_point(aes(shape = is.na(se)), size = 2.6, fill = BG, na.rm = TRUE) +
    facet_wrap(~panel, nrow = 1, scales = "free_x") +
    scale_colour_manual(values = pal, guide = "none") +
    scale_shape_manual(values = c("FALSE" = 19, "TRUE" = 21), guide = "none") +
    labs(x = "Log scale, with 95% confidence intervals", y = NULL,
         title = "What the transition table looks like as a model",
         subtitle = "Every role is preferred over Master; the middle roles are the ones people most readily leave",
         caption = "Quasi-symmetry fits (G2 = 22.0, df = 10) where symmetry does not (G2 = 192.4, df = 15). Hollow point marks the reference. Source: 11_square_table_models.R") +
    hnr_theme()
}
save_fig("fig7_loglinear_parameters", fig7, 10, 4.2)


# --- Fig 8: what people go to a natural place for --------------------------
# Loadings from 13_reasons_efa.R. Factors are named by the item that defines
# them rather than by ML number, so the labels survive a re-run.
re <- readRDS("hnr_reasons_efa.rds")
Lm <- unclass(re$efa$loadings)
nm <- function(j) {
  top <- rownames(Lm)[which.max(abs(Lm[, j]))]
  c(rs_relax = "Restorative", rs_teaches = "Dialogic", rs_comfort = "Serviced")[top]
}
colnames(Lm) <- vapply(seq_len(ncol(Lm)), nm, character(1))
d8 <- expand.grid(item = rownames(Lm), factor = colnames(Lm), stringsAsFactors = FALSE)
d8$loading <- as.vector(Lm)
d8$item <- sub("^rs_", "", d8$item)
d8$shown <- ifelse(abs(d8$loading) >= 0.30, sprintf("%.2f", d8$loading), "")
assign_to <- colnames(Lm)[apply(abs(Lm), 1, which.max)]
ord <- rownames(Lm)[order(match(assign_to, c("Restorative", "Dialogic", "Serviced")),
                          -apply(abs(Lm), 1, max))]
d8$item <- factor(d8$item, levels = rev(sub("^rs_", "", ord)))
d8$factor <- factor(d8$factor, levels = c("Restorative", "Dialogic", "Serviced"))
dropped <- c("meet_people", "cheap")
d8$item_lab <- ifelse(as.character(d8$item) %in% dropped,
                      paste0(as.character(d8$item), " *"), as.character(d8$item))

fig8 <- function() {
  ggplot(d8, aes(factor, item, fill = loading)) +
    geom_tile(colour = BG, linewidth = .8) +
    geom_text(aes(label = shown, colour = abs(loading) > .55), size = 3.2) +
    scale_fill_gradient2(low = BLUE, mid = "#F7F7F7", high = ACCENT,
                         midpoint = 0, limits = c(-.4, .9), name = "loading") +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = INK), guide = "none") +
    scale_y_discrete(labels = function(x) ifelse(x %in% dropped, paste0(x, " *"), x)) +
    labs(x = NULL, y = NULL,
         title = "What people go to a natural place for",
         subtitle = "Factor loadings from the exploratory half; values below .30 are left blank",
         caption = "Oblimin rotation, n = 1,255; * dropped for weak or split loadings.
Restorative and Serviced are uncorrelated (r = -.01). Source: 13_reasons_efa.R") +
    hnr_theme() +
    theme(panel.grid = element_blank(), legend.position = "right",
          legend.title = element_text(colour = MUTE, size = 9),
          axis.text.x = element_text(colour = INK, size = 11))
}
save_fig("fig8_reasons_loadings", fig8, 7.6, 5.4)


# --- Fig 9: what each motive does to the role people want ------------------
# Relative risk ratios from the multinomial model in 14_motives_and_roles.R,
# each role against Manager, the modal ideal. Manager is drawn at 1 as the
# reference so the anchor is visible rather than implied.
mr <- readRDS("hnr_motives_roles.rds")
d9 <- mr$rrr
d9 <- rbind(d9, data.frame(motive = unique(d9$motive), role = "Manager (ref.)",
                           rrr = 1, lo = NA, hi = NA, p = NA))
d9$role <- factor(d9$role, levels = rev(c("Manager (ref.)", "Master", "User",
                                          "Guardian", "Partner", "Object")))
d9$motive <- factor(d9$motive, levels = c("restorative", "dialogic", "serviced"),
                    labels = c("Restorative", "Dialogic", "Serviced"))
d9$kind <- ifelse(is.na(d9$lo), "ref", ifelse(d9$lo > 1 | d9$hi < 1, "sig", "ns"))

fig9 <- function() {
  ggplot(d9, aes(rrr, role, colour = kind)) +
    geom_vline(xintercept = 1, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1, na.rm = TRUE) +
    geom_point(aes(shape = kind), size = 2.2, fill = BG, na.rm = TRUE) +
    facet_wrap(~motive, nrow = 1) +
    scale_x_log10(breaks = c(.5, .75, 1, 1.5, 2, 3)) +
    scale_colour_manual(values = c(ref = MUTE, ns = MUTE, sig = BLUE), guide = "none") +
    scale_shape_manual(values = c(ref = 21, ns = 19, sig = 19), guide = "none") +
    labs(x = "Relative risk ratio against Manager (log scale)", y = NULL,
         title = "What each motive does to the role people want",
         subtitle = "Restorative pulls away from both poles; the dialogic motive raises both of them",
         caption = "Multinomial logit, n = 2,489, adjusted for country, age and gender. Coloured intervals exclude 1.\nAdding the motives improves on country, age and gender alone: LR chi-squared(15) = 137.0. Source: 14_motives_and_roles.R") +
    hnr_theme()
}
save_fig("fig9_motive_rrr", fig9, 10, 4.2)


# --- Fig 10: path diagram for the structural model -------------------------
# Latents are drawn with rounded corners, observed covariates square. Only
# the structural paths are shown; indicator counts stand in for the
# measurement model, which would otherwise need thirty more boxes.
sm <- readRDS("hnr_structural_model.rds")
pp <- sm$paths; ni <- sm$n_items
lab10 <- c(restorative = "Restorative", dialogic = "Dialogic", serviced = "Serviced",
           agency = "Agency of\nnon-human beings", relational_f = "Reciprocal\nperson-place belief",
           age_num = "Age", gender_bin = "Man (vs woman)")
nodes <- data.frame(
  key = names(lab10), label = unname(lab10),
  x = 0, y = c(5.2, 4.2, 3.2, 1.8, 0.8, -0.6, -1.5),
  latent = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)
nodes$label <- ifelse(nodes$latent,
                      paste0(nodes$label, "  (", ni[nodes$key], ")"), nodes$label)
nodes <- rbind(nodes, data.frame(key = "country", label = "Country\n(5 dummies)",
                                 x = 0, y = -2.5, latent = FALSE))
out <- data.frame(x = 4.2, y = 1.6, label = "Wants mastery\nas the ideal role")

arr <- merge(nodes[nodes$key != "country", ], pp, by.x = "key", by.y = "predictor")
arr$sig <- arr$p < .05
arr <- rbind(arr, data.frame(key = "country", label = "", x = 0, y = -2.5,
                             latent = FALSE, beta = NA, se = NA, p = NA, sig = FALSE))
arr$xend <- out$x - 0.78; arr$yend <- out$y
arr$lx <- arr$x + 0.62 * (arr$xend - arr$x)
arr$ly <- arr$y + 0.62 * (arr$yend - arr$y) + 0.13
arr$txt <- ifelse(is.na(arr$beta), "n.s.", sprintf("%+.2f", arr$beta))

arr$yend <- out$y + seq(0.42, -0.42, length.out = nrow(arr))   # fan into the box edge
tfrac <- 0.13   # label near the source end, where the arrows are still apart
arr$lx <- (arr$x + 0.9) + tfrac * (arr$xend - (arr$x + 0.9))
arr$ly <- arr$y + tfrac * (arr$yend - arr$y)
arr$lx <- arr$x + 0.9 + 0.66 * (arr$xend - (arr$x + 0.9))

fig10 <- function() {
  ggplot() +
    geom_segment(data = arr, aes(x = x + 0.9, y = y, xend = xend, yend = yend,
                 colour = sig, linetype = sig), linewidth = .5,
                 arrow = arrow(length = unit(.09, "in"), type = "closed")) +
    geom_label(data = arr, aes(lx, ly + 0.17, label = txt, colour = sig), size = 3.1,
               label.size = 0, fill = BG, label.padding = unit(.08, "lines")) +
    geom_label(data = nodes, aes(x, y, label = label, fill = latent), size = 3.2,
               colour = INK, label.r = unit(.28, "lines"), label.size = .3,
               label.padding = unit(.42, "lines")) +
    geom_label(data = out, aes(x, y, label = label), size = 3.4, colour = "white",
               fill = ACCENT, label.r = unit(.1, "lines"), label.size = 0,
               label.padding = unit(.55, "lines"), fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = "#EAF3F9", "FALSE" = "#F2F2F2"), guide = "none") +
    scale_colour_manual(values = c("TRUE" = BLUE, "FALSE" = MUTE), guide = "none") +
    scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 2), guide = "none") +
    scale_x_continuous(limits = c(-1.15, 5.4)) +
    scale_y_continuous(limits = c(-3.1, 5.9)) +
    labs(x = NULL, y = NULL,
         title = "When every measure competes, the motives carry the prediction",
         subtitle = "Standardised paths to wanting mastery; latent variables are rounded, with their indicator counts",
         caption = "WLSMV, n = 2,489. Dashed grey paths do not reach p < .05. Adding the agency and belief scales to the motives raises R-squared from\n.1006 to .1012; no country differs significantly from Canada (the country effect as a whole is tested in 22_geography.R). The absorbed scales correlate .59 (belief with restorative) and .50 (agency with dialogic).\nSource: 15_structural_model.R") +
    hnr_theme() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank())
}
save_fig("fig10_path_diagram", fig10, 9.6, 6.4)


# ---- Fig 11 (ggplot path engine): measurement + structural model, place model ----
fs   <- readRDS("hnr_place_relationship.rds")$structural
ld11 <- sem_loads(fs); sp11 <- sem_paths(fs)
ind_lab <- function(v) gsub("_", " ", sub("^rs_", "", sub("^mp_", "", v)))
grp <- list(place = grep("^mp_", unique(ld11$rhs), value = TRUE),
            serviced = c("rs_struggles", "rs_comfort", "rs_photos"),
            dialogic = c("rs_meet_animals", "rs_communicates", "rs_teaches"),
            restorative = c("rs_relax", "rs_beauty", "rs_watch_plants", "rs_own_thoughts", "rs_quiet", "rs_active"))
lat_lab <- c(place = "Place agency", serviced = "Serviced", dialogic = "Dialogic", restorative = "Restorative")
yy <- 8.3; nd11 <- list(); lat_y <- c()
for (gname in names(grp)) {
  ys <- yy - 0.42 * (seq_along(grp[[gname]]) - 1)
  nd11[[length(nd11) + 1]] <- data.frame(id = grp[[gname]], label = ind_lab(grp[[gname]]), x = 1.0, y = ys, shape = "box",
                                         w = 1.7, h = 0.34, fill = "white", border = "#A8A8A8", size = 2.9, lwd = NA_real_)
  lat_y[gname] <- mean(ys); yy <- min(ys) - 0.78
}
LATF <- "#E3EEF7"
nd11[[length(nd11) + 1]] <- data.frame(id = names(grp), label = lat_lab[names(grp)], x = 4.6, y = unname(lat_y[names(grp)]),
  shape = "ellipse", w = 2.0, h = 0.85, fill = LATF, border = "#5B87A6", size = 3.6, lwd = NA_real_)
out_y <- mean(lat_y)
nd11[[length(nd11) + 1]] <- data.frame(id = "master_should", label = "Wants\nmastery", x = 8.6, y = out_y, shape = "box",
  w = 1.7, h = 1.5, fill = "white", border = "#1A1A1A", size = 3.8, lwd = 0.9)
nd11[[length(nd11) + 1]] <- data.frame(id = c("age_num", "gender_bin"), label = c("Age", "Man"), x = c(7.1, 9.2), y = 0.55,
  shape = "box", w = 1.3, h = 0.5, fill = "#F4F4F4", border = "#8A8A8A", size = 3.3, lwd = NA_real_)
n11 <- do.call(rbind, nd11)
n11$face <- ifelse(n11$id == "master_should", "bold", "plain")
pth11 <- function(from, to, off, t, side = "left") { r <- sp11[sp11$lhs == to & sp11$rhs == from, ]
  data.frame(from = from, to = to, label = fmt_b(r$est.std), kind = "path", beta = r$est.std, sig = r$pvalue < .05,
             t = t, end_side = side, end_off = off) }
lod11 <- function(f, i) { r <- ld11[ld11$lhs == f & ld11$rhs == i, ]
  data.frame(from = f, to = i, label = fmt_b(r$est.std), kind = "load", beta = r$est.std, sig = TRUE, t = 0.5,
             end_side = NA_character_, end_off = NA_real_) }
e11 <- rbind(do.call(rbind, lapply(names(grp), function(g_) do.call(rbind, lapply(grp[[g_]], function(i) lod11(g_, i))))),
  pth11("place", "master_should", 0.55, 0.5), pth11("serviced", "master_should", 0.2, 0.5),
  pth11("dialogic", "master_should", -0.2, 0.5), pth11("restorative", "master_should", -0.55, 0.5),
  pth11("age_num", "master_should", -0.35, 0.5, "bottom"), pth11("gender_bin", "master_should", 0.35, 0.5, "bottom"))
g11 <- sem_plot(n11, e11, xlim = c(0, 10.2), ylim = c(-0.45, 9.0), legend_at = c(0.3, -0.2),
  title = "The role people want, from motives and from how much agency they grant a place",
  subtitle = "Standardised estimates. Line width grows with the size of the path; grey lines are loadings.
Higher place agency = the place is granted more independence and influence.",
  caption = paste0("WLSMV, n = 2,489, ordinal indicators. Education and country dummies are in the model but not drawn (no country differs significantly from Canada).\n",
    "The place-agency latent correlates .36 with restorative, .36 with dialogic and −.28 with serviced. Place agency is the personal counterpart of the role item,\n",
    "so its path is partly the same construct measured twice; the motive paths shift once it is included. Source: 18_place_relationship.R"))
save_sem(g11, "fig11_semplot_place_model", 10.2, 10.0)

# ---- Figs 12-14, 16, 17 (ggplot path engine): the role SEEN and WANTED, side by side ----
show <- c("place", "control", "serviced", "dialogic", "restorative", "age_num", "gender_bin", "edu_primary", "edu_higher")
n_ind <- c(place = 5, control = 12, serviced = 3, dialogic = 3, restorative = 6)
lab12 <- c(place = "Place agency", control = "Societal control", serviced = "Serviced",
           dialogic = "Dialogic", restorative = "Restorative", age_num = "Age", gender_bin = "Man")
lab12[names(n_ind)] <- paste0(lab12[names(n_ind)], "  (", n_ind, ")")

pair_sem_fig <- function(fit, out_now, out_should, differs, role, headline, file, src, vars = show,
                         lab_now = NULL, lab_should = NULL, note = "") {
  psn <- sem_paths(fit); pall <- lavaan::standardizedSolution(fit); ld <- sem_loads(fit)
  r2 <- lavaan::inspect(fit, "r2")
  rc <- pall[pall$op == "~~" & pall$lhs == out_now & pall$rhs == out_should, "est.std"]
  latent <- vars %in% names(n_ind)
  ilab <- function(v) gsub("_", " ", sub("^(rs|mp|ctl)_", "", v))
  lab_lat <- c(place = "Place agency", control = "Societal control", serviced = "Serviced",
               dialogic = "Dialogic", restorative = "Restorative", age_num = "Age", gender_bin = "Man",
               edu_primary = "Primary education", edu_higher = "Higher education")

  # vertical layout, shared by both panels: each latent sits at the mean height of its indicators
  dy <- 0.235; gap <- 0.32; top <- 0
  ys <- numeric(length(vars)); ind_y <- list()
  for (i in seq_along(vars)) {
    v <- vars[i]
    if (latent[i]) {
      its <- ld$rhs[ld$lhs == v]; yi <- top - dy * (seq_along(its) - 1)
      ind_y[[v]] <- setNames(yi, its); ys[i] <- mean(yi); top <- min(yi) - gap
    } else { ys[i] <- top - 0.15; top <- ys[i] - 0.38 }
  }
  shift <- -min(ys, unlist(ind_y)) + 0.55; ys <- ys + shift; ind_y <- lapply(ind_y, function(z) z + shift)
  ymax <- max(ys, unlist(ind_y)); oy <- mean(range(ys))

  X <- list(ind = 0.9, lat = 3.75, out = 7.85); PW <- 9.3
  nodes <- list(); edges <- list(); texts <- list(); bands <- list()
  for (k in 1:2) {
    outc <- c(out_now, out_should)[k]; sfx <- c("_L", "_R")[k]; ox <- (k - 1) * PW
    nodes[[length(nodes) + 1]] <- data.frame(id = paste0(vars, sfx), label = unname(lab_lat[vars]), x = X$lat + ox, y = ys,
      shape = ifelse(latent, "ellipse", "box"), w = ifelse(latent, 2.2, 1.5), h = ifelse(latent, 0.62, 0.42),
      fill = ifelse(latent, "#E3EEF7", "#F4F4F4"), border = ifelse(latent, "#5B87A6", "#8A8A8A"), size = 3.3,
      lwd = NA_real_, face = "plain")
    for (v in names(ind_y)) {
      its <- names(ind_y[[v]])
      nodes[[length(nodes) + 1]] <- data.frame(id = paste0(its, sfx), label = ilab(its), x = X$ind + ox, y = unname(ind_y[[v]]),
        shape = "box", w = 1.5, h = 0.2, fill = "white", border = "#A8A8A8", size = 2.4, lwd = NA_real_, face = "plain")
      lo <- ld[ld$lhs == v, ]
      edges[[length(edges) + 1]] <- data.frame(from = paste0(v, sfx), to = paste0(lo$rhs, sfx), label = fmt_b(lo$est.std),
        kind = "load", beta = lo$est.std, sig = TRUE, t = 0.6, end_side = "right", end_off = 0)
    }
    nodes[[length(nodes) + 1]] <- data.frame(id = paste0("out", sfx),
      label = if (k == 1) { if (is.null(lab_now)) paste0("Sees ", tolower(role), "\nnow") else lab_now }
              else { if (is.null(lab_should)) paste0("Wants\n", tolower(role)) else lab_should },
      x = X$out + ox, y = oy, shape = "box", w = 1.75, h = 1.5, fill = "white", border = "#1A1A1A",
      size = 3.7, lwd = 0.9, face = "bold")
    r <- psn[psn$lhs == outc, ]; b <- r$est.std[match(vars, r$rhs)]; pv <- r$pvalue[match(vars, r$rhs)]
    edges[[length(edges) + 1]] <- data.frame(from = paste0(vars, sfx), to = paste0("out", sfx),
      label = paste0(fmt_b(b), ifelse(!is.na(differs[vars]) & differs[vars], "*", "")), kind = "path", beta = b, sig = pv < .05,
      t = 0.5, end_side = "left", end_off = seq(0.55, -0.55, length.out = length(vars)))
    texts[[length(texts) + 1]] <- data.frame(x = 0.2 + ox, y = ymax + 0.75,
      label = sprintf("The role people %s   (R² = %.2f)", c("SEE", "WANT")[k], r2[[outc]]),
      size = 4.1, colour = sem_col$ink, hjust = 0, face = "bold")
    bands[[length(bands) + 1]] <- data.frame(x = 4.55 + ox, y = (ymax + 0.35 + 0.1) / 2, w = 8.8, h = ymax + 0.25, fill = "#F6F7F9")
  }
  ylo <- -0.1; yhi <- ymax + 1.1
  g <- sem_plot(do.call(rbind, nodes), do.call(rbind, edges), xlim = c(0, 2 * PW - 0.3), ylim = c(ylo - 0.45, yhi),
    texts = do.call(rbind, texts), bands = do.call(rbind, bands), legend_at = c(0.3, ylo - 0.2),
    title = headline,
    subtitle = "Standardised estimates. Line width grows with the size of the path; grey lines are loadings; * = the path differs between the two panels (Wald p < .05).",
    caption = paste0(sprintf("WLSMV, n = 2,489; country dummies and a not-stated education flag are in the model, not drawn. The two outcomes keep a residual correlation of %.2f. Source: %s", rc, src),
                     if (nzchar(note)) paste0("\n", note) else ""))
  save_sem(g, file, 2 * PW - 0.3, (yhi - ylo + 0.45) + 1.7)
}

ns <- readRDS("hnr_now_vs_should_sem.rds")
d_m <- setNames(ns$wald$differs == "yes", ns$wald$predictor)
pair_sem_fig(ns$fit, "master_now", "master_should", d_m, "Mastery",
  "Same model, two outcomes: the personal relationship to a place matters far more for wanting mastery than for seeing it",
  "fig12_sem_now_vs_should", "19_now_vs_should_sem.R")

op <- readRDS("hnr_opposing_roles_sem.rds")
dd <- function(r) setNames(op[[r]]$table$differs == "yes", op[[r]]$table$predictor)
pair_sem_fig(op$Object$fit, "out_now", "out_should", dd("Object"), "Object",
  "The far pole is the mirror image of Master: place agency raises seeing and wanting Object alike, where it lowers both for Master",
  "fig13_sem_object", "20_opposing_roles_sem.R")
pair_sem_fig(op$Guardian$fit, "out_now", "out_should", dd("Guardian"), "Guardian",
  "Guardian is barely predictable from these measures: only place agency and gender reach significance",
  "fig14_sem_guardian", "20_opposing_roles_sem.R")


# --- Fig 15: how the motives act on wanting mastery, by pathway ------------
# From 21_mediation_sem.R. Each motive is fitted on its own (entered together
# they suppress one another and the paths exceed 1). The serviced run is
# unstable (largest standardised path 1.62, negative variances) and is left
# out; the omission is stated on the figure.
md <- readRDS("hnr_mediation_sem.rds")
one_motive <- function(x) {
  r <- md$runs[[x]]; ps <- r$ps; g_ <- r$grid; t_ <- r$tg
  i <- which(t_$y == "master_should" & t_$x == x)
  ids <- g_$name[g_$y == "master_should" & g_$x == x]; mm <- g_$m[g_$y == "master_should" & g_$x == x]
  lab <- c(paste0("tot", i), paste0("d_master_should_", x), paste0("tind", i), ids)
  nice <- c("Total effect", "Direct effect", "All indirect",
            paste0("via ", c(place = "place agency", agency = "general agency",
                             relational_f = "reciprocal belief", control = "societal control")[mm]))
  k <- match(lab, ps$label)
  data.frame(motive = x, pathway = nice, est = ps$est.std[k], lo = ps$ci.lower[k], hi = ps$ci.upper[k])
}
d15 <- rbind(one_motive("restorative"), one_motive("dialogic"))
d15$pathway <- factor(d15$pathway, levels = rev(c("Total effect", "Direct effect", "All indirect",
                      "via place agency", "via societal control", "via general agency",
                      "via reciprocal belief")))
d15$motive <- factor(d15$motive, levels = c("restorative", "dialogic"),
                     labels = c("Restorative motive", "Dialogic motive"))
d15$sig <- d15$lo > 0 | d15$hi < 0
d15$kind <- ifelse(d15$pathway == "Total effect", "total", ifelse(d15$sig, "sig", "ns"))

fig15 <- function() {
  ggplot(d15, aes(est, pathway, colour = kind)) +
    geom_vline(xintercept = 0, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1) +
    geom_point(aes(shape = kind), size = 2.4, fill = BG) +
    facet_wrap(~motive, nrow = 1) +
    scale_colour_manual(values = c(total = INK, sig = BLUE, ns = MUTE), guide = "none") +
    scale_shape_manual(values = c(total = 18, sig = 19, ns = 1), guide = "none") +
    labs(x = "Standardised effect on wanting mastery, with 95% confidence interval", y = NULL,
         title = "Place agency carries almost all of the indirect effect of the motives",
         subtitle = "Restorative acts almost entirely through place agency; the dialogic motive raises the wish directly and lowers it through place agency, netting to about zero. Other mediators add +/-.04 at most",
         caption = "One motive per model (together they suppress one another). The serviced motive is omitted: its model is unstable (largest standardised path 1.62).\nCross-sectional data: these are decompositions consistent with mediation, not evidence of causal order. Delta-method intervals. Source: 21_mediation_sem.R") +
    hnr_theme()
}
save_fig("fig15_mediation", fig15, 11, 4.6)


# --- Figs 16-18: the ordered outcome, extremity, and mediation on position ---
# From 26_ordered_sem.R and 27_ordered_mediation.R, after script 24 showed the
# wanted role is one ordered dominance scale plus a second dimension, extremity.
so <- readRDS("hnr_ordered_sem.rds")
show2 <- c("place", "control", "dialogic", "restorative", "age_num", "gender_bin", "edu_primary", "edu_higher")
wd <- function(o) { w <- so$wald[so$wald$outcome == o, ]; setNames(w$differs == "yes", w$predictor) }
pair_sem_fig(so$fit, "pos_now", "pos_should", wd("position"), "position",
  "The wanted role sits further from Master the more agency a person grants a place, and that link is stronger for wanting than for seeing",
  "fig16_sem_position", "26_ordered_sem.R", vars = show2,
  lab_now = "Position\nseen now", lab_should = "Position\nwanted",
  note = "Position runs from Master (1) to Object (6): positive = a role further from Master.")
pair_sem_fig(so$fit, "ext_now", "ext_should", wd("extremity"), "extremity",
  "The motives act on how polarised the wanted role is: restorative experience lowers it, dialogic experience raises it",
  "fig17_sem_extremity", "26_ordered_sem.R", vars = show2,
  lab_now = "Extremity\nseen now", lab_should = "Extremity\nwanted",
  note = "Extremity = choosing either Master or Object. The place-agency and control paths are not interpretable (unequal pole sizes); read the motive paths.")

om <- readRDS("hnr_ordered_mediation.rds")
one_ord <- function(x) {
  ps <- om$runs[[x]]$ps
  lab <- c("tot_pos_should", "d_pos_should", "tind_pos_should", "ind_pos_should_place", "ind_pos_should_control")
  nice <- c("Total effect", "Direct effect", "All indirect", "via place agency", "via societal control")
  k <- match(lab, ps$label)
  data.frame(motive = x, pathway = nice, est = ps$est.std[k], lo = ps$ci.lower[k], hi = ps$ci.upper[k])
}
d18 <- rbind(one_ord("restorative"), one_ord("dialogic"))
d18$pathway <- factor(d18$pathway, levels = rev(c("Total effect", "Direct effect", "All indirect",
                      "via place agency", "via societal control")))
d18$motive <- factor(d18$motive, levels = c("restorative", "dialogic"),
                     labels = c("Restorative motive", "Dialogic motive"))
d18$sig <- d18$lo > 0 | d18$hi < 0
d18$kind <- ifelse(d18$pathway == "Total effect", "total", ifelse(d18$sig, "sig", "ns"))

fig18 <- function() {
  ggplot(d18, aes(est, pathway, colour = kind)) +
    geom_vline(xintercept = 0, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1) +
    geom_point(aes(shape = kind), size = 2.4, fill = BG) +
    facet_wrap(~motive, nrow = 1) +
    scale_colour_manual(values = c(total = INK, sig = BLUE, ns = MUTE), guide = "none") +
    scale_shape_manual(values = c(total = 18, sig = 19, ns = 1), guide = "none") +
    labs(x = "Standardised effect on the position of the wanted role (positive = further from Master)", y = NULL,
         title = "Both motives move the wanted role away from Master through place agency",
         subtitle = "For the dialogic motive a direct effect pulls the other way and the two nearly cancel; for the restorative motive the direct effect is small",
         caption = "One motive per model. Serviced is omitted (weak, low reliability) and extremity is not decomposed (no clean place-agency path).\nCross-sectional data, and the place-agency items were asked after the role question: consistent with mediation, not evidence of it. Source: 27_ordered_mediation.R") +
    hnr_theme()
}
save_fig("fig18_mediation_ordered", fig18, 11, 4.4)

# --- Fig 19: one ordered scale? Odds ratio at each cut point of the wanted role ---
# From the analysis in 24_outcome_structure.R, recomputed here with confidence
# intervals. Each cut asks: what predicts wanting a role ABOVE this point on the
# ordered scale (Master, Manager, User, Guardian, Partner, Object)? If one ordered
# scale describes the role, a predictor has the same odds ratio at every cut.
cp <- local({
  d <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
  nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
  gg <- re$groups
  names(gg) <- vapply(gg, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
    unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
  mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
  z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  d$Restorative <- z(rowMeans(d[, gg$restorative])); d$Dialogic <- z(rowMeans(d[, gg$dialogic]))
  d$PlaceAgency <- z(rowMeans(d[, mp])); d$Control <- z(d$control_mean)
  d <- d[complete.cases(d[, c("typ_should", "age_num", "gender_bin")]), ]
  cuts <- c("Master | Manager", "Manager | User", "User | Guardian", "Guardian | Partner", "Partner | Object")
  do.call(rbind, lapply(1:5, function(k) {
    d$y <- as.integer(d$typ_should > k)
    m <- glm(y ~ PlaceAgency + Restorative + Dialogic + Control + age_num + gender_bin + edu_primary + edu_higher + edu_na + country, binomial, d)
    s <- summary(m)$coefficients[c("PlaceAgency", "Restorative", "Dialogic", "Control"), , drop = FALSE]
    data.frame(cut = cuts[k],
               predictor = c("Place agency", "Restorative motive", "Dialogic motive", "Societal control"),
               or = exp(s[, 1]), lo = exp(s[, 1] - 1.96 * s[, 2]), hi = exp(s[, 1] + 1.96 * s[, 2]))
  }))
})
cp$cut <- factor(cp$cut, levels = unique(cp$cut))
cp$predictor <- factor(cp$predictor, levels = c("Place agency", "Restorative motive", "Dialogic motive", "Societal control"))
cols19 <- c("Place agency" = BLUE, "Restorative motive" = GREEN, "Dialogic motive" = ACCENT, "Societal control" = MUTE)

fig19 <- function() {
  ggplot(cp, aes(cut, or, group = predictor, colour = predictor)) +
    geom_hline(yintercept = 1, linetype = 2, colour = MUTE, linewidth = .4) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = predictor), alpha = .15, colour = NA) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    facet_wrap(~predictor, nrow = 1) +
    scale_y_log10(breaks = c(.5, .7, 1, 1.5, 2, 3)) +
    scale_colour_manual(values = cols19, guide = "none") +
    scale_fill_manual(values = cols19, guide = "none") +
    scale_x_discrete(labels = function(x) gsub(" | ", " |\n", x, fixed = TRUE)) +
    labs(x = "Cut point on the ordered scale (wanting a role above it)", y = "Odds ratio per standard deviation",
         title = "One ordered scale: place agency acts alike at every cut, while both motives reverse between the two ends",
         subtitle = "Odds ratio of wanting a role above each cut point, adjusted for the other predictors, age, gender and country",
         caption = "A flat line means one ordered scale describes the role. The restorative motive falls from above 1 to below 1 and the dialogic motive rises from below 1 to above 1, so neither describes position. Source: 24_outcome_structure.R") +
    hnr_theme() +
    theme(axis.text.x = element_text(size = 7.5))
}
save_fig("fig19_cutpoints", fig19, 11, 4.4)

# ---- Fig 20 (ggplot path engine): the whole story as one path diagram ----
# From 28_story_model.R: both motives, place agency and societal control as mediators, and position
# and extremity of the wanted role, with indicators. Age, gender and country are in the model and not drawn.
st <- readRDS("hnr_story_model.rds")$fit
ld <- sem_loads(st); sp <- sem_paths(st)
ind_lab <- function(v) gsub("_", " ", sub("^rs_", "", sub("^mp_", "", v)))
it_rest <- c("rs_relax", "rs_beauty", "rs_watch_plants", "rs_own_thoughts", "rs_quiet", "rs_active")
it_dia  <- c("rs_teaches", "rs_communicates", "rs_meet_animals")
it_plc  <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
LAT <- "#E3EEF7"; OBS <- "#F4F4F4"; OUT <- "#FFFFFF"
mk <- function(...) { d <- data.frame(...); cols <- c("id","label","x","y","shape","w","h","fill","border","size","lwd"); for (c in cols) if (is.null(d[[c]])) d[[c]] <- NA; d[, cols] }
n20 <- rbind(
  mk(id = it_rest, label = ind_lab(it_rest), x = 0.9, y = seq(6.65, 3.9, length.out = 6), shape = "box", w = 1.5, h = 0.34, fill = "white", border = "#A8A8A8", size = 2.9),
  mk(id = it_dia,  label = ind_lab(it_dia),  x = 0.9, y = c(2.85, 2.3, 1.75), shape = "box", w = 1.5, h = 0.34, fill = "white", border = "#A8A8A8", size = 2.9),
  mk(id = it_plc,  label = ind_lab(it_plc),  x = seq(4.7, 9.9, length.out = 5), y = 7.0, shape = "box", w = 1.2, h = 0.34, fill = "white", border = "#A8A8A8", size = 2.9),
  mk(id = c("restorative", "dialogic", "place"), label = c("Restorative", "Dialogic", "Place\nagency"),
             x = c(3.75, 3.75, 7.3), y = c(5.28, 2.3, 5.85), shape = "ellipse", w = c(1.7, 1.7, 1.7), h = c(0.9, 0.9, 1.0),
             fill = LAT, border = "#5B87A6", size = 3.6),
  mk(id = "ctl_mean", label = "Societal\ncontrol", x = 7.3, y = 1.25, shape = "box", w = 1.4, h = 0.66, fill = OBS, border = "#8A8A8A", size = 3.4),
  mk(id = c("pos_should", "ext_should"), label = c("Position of\nwanted role", "Extremity of\nwanted role"),
             x = 11.75, y = c(4.6, 2.9), shape = "box", w = 1.9, h = 0.85, fill = OUT, border = "#1A1A1A", size = 3.6, lwd = 0.9))
n20$face <- ifelse(n20$id %in% c("pos_should", "ext_should"), "bold", "plain")
pth <- function(from, to, off = NA, t = 0.5, side = NA) {
  r <- sp[sp$lhs == to & sp$rhs == from, ]
  data.frame(from = from, to = to, label = fmt_b(r$est.std), kind = "path", beta = r$est.std, sig = r$pvalue < .05,
             t = t, end_side = if (is.na(side)) NA_character_ else side, end_off = off)
}
lod <- function(f, i, t) { r <- ld[ld$lhs == f & ld$rhs == i, ]
  data.frame(from = f, to = i, label = fmt_b(r$est.std), kind = "load", beta = r$est.std, sig = TRUE, t = t, end_side = NA_character_, end_off = NA_real_) }
e20 <- rbind(
  do.call(rbind, lapply(it_rest, function(i) lod("restorative", i, 0.5))),
  do.call(rbind, lapply(it_dia,  function(i) lod("dialogic", i, 0.5))),
  do.call(rbind, lapply(it_plc,  function(i) lod("place", i, 0.6))),
  pth("restorative", "place", t = 0.5), pth("dialogic", "place", t = 0.66),
  pth("restorative", "ctl_mean", t = 0.78), pth("dialogic", "ctl_mean", t = 0.5),
  pth("place", "pos_should", 0.30, 0.5, "left"), pth("restorative", "pos_should", 0.10, 0.60, "left"),
  pth("dialogic", "pos_should", -0.10, 0.36, "left"), pth("ctl_mean", "pos_should", -0.30, 0.55, "left"),
  pth("place", "ext_should", 0.30, 0.40, "left"), pth("restorative", "ext_should", 0.10, 0.72, "left"),
  pth("dialogic", "ext_should", -0.10, 0.62, "left"), pth("ctl_mean", "ext_should", -0.30, 0.55, "left"))
hd <- data.frame(x = c(2.4, 7.75, 11.75), y = 7.62, label = c("MOTIVES FOR VISITING A PLACE", "CONSTRUAL OF THE PLACE", "ROLE PEOPLE WANT"))
bd <- data.frame(x = c(2.4, 7.75, 11.75), y = c(4.4, 4.4, 3.75), w = c(4.6, 5.9, 2.4), h = c(6.4, 6.4, 3.0), fill = "#F6F7F9")
g20 <- sem_plot(n20, e20, xlim = c(0, 13), ylim = c(0.05, 7.95), headers = hd, bands = bd, legend_at = c(0.3, 0.32),
  title = "How experience of a place reaches the role people want: through the agency they grant it",
  subtitle = "Standardised estimates, both motives entered together. Line width grows with the size of the path.",
  caption = "WLSMV, n = 2,489, ordinal indicators. Position runs from Master (1) to Object (6): positive = a role further from Master. Extremity = choosing either pole.\nAge, gender, education and country are in the model and not drawn: age and gender go with place agency (+.18, −.20)
but have no significant path to the wanted role, and education has only a borderline path to extremity (higher −.06, primary +.05).
Societal control is its mean score. Source: 28_story_model.R")
save_sem(g20, "fig20_story_model", 13, 8.6)


# --- Fig 21: who rejects the mastery they see, and who moves toward it ---------
# From the analysis in 32_who_rejects_mastery.R, recomputed here for the curve.
# Left: people who see Master, the chance of wanting another role. Right: people who
# do not see Master, the chance of wanting it. Lines are average predicted
# probabilities across place agency (all other predictors as observed), with a 95%
# bootstrap band (300 resamples); dots are the observed shares in fifths of place agency.
wr <- local({
  d <- readRDS("hnr_data.rds"); re <- readRDS("hnr_reasons_efa.rds")
  nm <- c(rs_relax = "restorative", rs_teaches = "dialogic", rs_comfort = "serviced")
  gg <- re$groups
  names(gg) <- vapply(gg, function(v) { L <- unclass(re$efa$loadings)[v, , drop = FALSE]
    unname(nm[rownames(L)[which.max(apply(abs(L), 1, max))]]) }, character(1))
  mp <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")
  z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  d$plc_z <- z(rowMeans(d[, mp])); d$rest_z <- z(rowMeans(d[, gg$restorative])); d$dia_z <- z(rowMeans(d[, gg$dialogic]))
  d$ctl_z <- z(d$control_mean)
  d <- d[complete.cases(d[, c("typ_now", "typ_should", "age_num", "gender_bin", "plc_z", "rest_z", "dia_z", "ctl_z")]), ]
  cov <- "plc_z + rest_z + dia_z + ctl_z + age_num + gender_bin + edu_primary + edu_higher + edu_na + country"
  grp <- list(
    list(data = transform(d[d$typ_now == 1, ], y = as.integer(typ_should != 1)), f = paste("y ~", cov),
         label = "People who see Master
Chance of wanting another role"),
    list(data = transform(d[d$typ_now != 1, ], y = as.integer(typ_should == 1)), f = paste("y ~ factor(typ_now) +", cov),
         label = "People who do not see Master
Chance of wanting Master"))
  set.seed(20260930)
  out <- lapply(grp, function(g_) {
    dd <- g_$data; grid <- seq(quantile(dd$plc_z, .025), quantile(dd$plc_z, .975), length.out = 25)
    q <- quantile(dd$plc_z, c(.1, .9))
    avg <- function(fit, data, at) sapply(at, function(v) { nd <- data; nd$plc_z <- v; mean(predict(fit, nd, type = "response")) })
    fit <- glm(as.formula(g_$f), binomial, dd)
    est <- avg(fit, dd, grid)
    bs <- replicate(300, { b <- dd[sample(nrow(dd), replace = TRUE), ]
      avg(suppressWarnings(glm(as.formula(g_$f), binomial, b)), b, grid) })
    lab <- sub("
", sprintf(" (n = %s)
", format(nrow(dd), big.mark = ",")), g_$label, fixed = TRUE)
    fifth <- cut(dd$plc_z, quantile(dd$plc_z, seq(0, 1, .2)), include.lowest = TRUE)
    list(curve = data.frame(panel = lab, x = grid, p = est, lo = apply(bs, 1, quantile, .025), hi = apply(bs, 1, quantile, .975)),
         obs = data.frame(panel = lab, x = as.vector(tapply(dd$plc_z, fifth, mean)), p = as.vector(tapply(dd$y, fifth, mean))),
         mark = data.frame(panel = lab, x = as.vector(q), p = avg(fit, dd, q), which = c("low place agency\n(10th percentile)", "high place agency\n(90th percentile)")))
  })
  list(curve = do.call(rbind, lapply(out, `[[`, "curve")), obs = do.call(rbind, lapply(out, `[[`, "obs")),
       mark = do.call(rbind, lapply(out, `[[`, "mark")))
})
for (nm_ in names(wr)) wr[[nm_]]$panel <- factor(wr[[nm_]]$panel, levels = unique(wr$curve$panel))
cols21 <- setNames(c(BLUE, ACCENT), levels(wr$curve$panel))
wr$mark$txt <- sprintf("%.0f%%", 100 * wr$mark$p)
wr$mark$vj  <- c(-1.1, -1.1, 2.0, -1.1)     # above the point, except where an observed dot sits above it
wr$mark$hj  <- c(0.5, 0.5, 0.5, 0.5)
wr$mark$hy  <- c(0.03, 0.03, 0.985, 0.985)   # percentile captions: bottom of the left panel, top of the right one
wr$mark$hv  <- c(0, 0, 1, 1)

fig21 <- function() {
  ggplot(wr$curve, aes(x, p)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = panel), alpha = .15) +
    geom_line(aes(colour = panel), linewidth = 1) +
    geom_point(data = wr$obs, aes(colour = panel), size = 2.6, shape = 21, fill = BG, stroke = 1) +
    geom_point(data = wr$mark, aes(colour = panel), size = 3) +
    geom_text(data = wr$mark, aes(label = txt, vjust = vj, hjust = hj), colour = INK, size = 3.6, fontface = "bold") +
    geom_text(data = wr$mark, aes(y = hy, label = which, vjust = hv), colour = MUTE, size = 2.7, lineheight = .95) +
    facet_wrap(~panel, nrow = 1) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25), labels = function(v) paste0(round(100 * v), "%"), expand = c(0, 0)) +
    scale_colour_manual(values = cols21, guide = "none") + scale_fill_manual(values = cols21, guide = "none") +
    labs(x = "Place agency (standard deviations from the mean)", y = "Probability",
         title = "Place agency separates the people who reject the mastery they see from those who keep it",
         subtitle = "Average predicted probability across place agency, with the motives, societal control, age, gender, education and country as observed",
         caption = "Line and band: model prediction with a 95% bootstrap interval. Open dots: observed shares in fifths of place agency.\nFilled dots: the 10th and 90th percentiles of place agency in each group. Source: 32_who_rejects_mastery.R") +
    hnr_theme() + theme(panel.spacing = unit(1.6, "lines"))
}
save_fig("fig21_who_rejects", fig21, 11, 4.8)

cat("Figures written to the figures/ folder", fill = TRUE)
