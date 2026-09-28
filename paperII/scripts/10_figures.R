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

suppressMessages({ library(ggplot2); library(lavaan) })

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
pal <- c(Master = "#D55E00", Manager = "#0072B2", User = "#8C8C8C",
         Guardian = "#009E73", Partner = "#CC79A7", Object = "#5A5A5A")
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

cat("Figures written to the figures/ folder", fill = TRUE)
