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

cat("Figures written to the figures/ folder", fill = TRUE)
