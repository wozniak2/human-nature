# ============================================================
# 01_load_and_prepare.R
# Loads all 6 country survey exports, extracts the comparable
# blocks (Q1 relational-entanglement items, Q4/Q5 role typology,
# demographics), screens response quality, and stacks everything
# into one long data frame.
#
# IMPORTANT CAVEATS (checked 2026-08-03, re-verify if files change):
#  - Q1 items (cols 10-15) and the Q4/Q5 typology (cols 54-55) are
#    identically coded (1-5 and 1-6) and positioned across all 6
#    files -- safe to pool as-is.
#  - Education is NOT on the same scale everywhere: Canada/Netherlands/
#    Sweden use a 9-point scale; Poland and Spain only go up to 7;
#    Panama's codes run 2-7. DO NOT pool education_raw across
#    countries without recoding into a harmonized ordinal scheme --
#    confirm the actual per-country answer-option wording first.
#    (Done in 04_mimic_model.R: harmonised to primary / high school / higher, with a
#    "not stated" flag; see edu_level() there.)
#  - Gender granularity differs: Netherlands shows only codes 1-2
#    (suggests binary-only recruitment), the other 5 countries show
#    4-5 categories.
#  - residence_raw (urban/rural "Place of Residence") is loaded for
#    completeness but is OUT OF SCOPE for the current research design
#    -- the project pivoted to country-of-residence as the sole
#    grouping variable; do not reintroduce residence comparisons
#    without deciding to do so again.
#  - Fielding windows are NOT simultaneous across countries (see the
#    printed report and hnr_fielding_dates.rds below) -- country is
#    partly confounded with survey timing/season. Disclose this
#    wherever a country effect is reported (see PaperII_Outline.docx).
# ============================================================

suppressMessages(library(readxl))

# Raw survey exports live in ../data (this script lives in paperII/scripts/).
# Change DATA_DIR if you move things around again.
DATA_DIR <- "../data"

files <- c(
  Canada      = "Canada Baza danych.xlsx",
  Panama      = "Panama Baza danych.xlsx",
  Poland      = "Poland Baza danych.xlsx",
  Netherlands = "Research in the Netherlands Human-Natural Place Relations project number UMO-202351BHS400279.xlsx",
  Spain       = "Spain Baza danych.xlsx",
  Sweden      = "Sweden.xlsx"
)
files <- setNames(file.path(DATA_DIR, files), names(files))  # file.path() drops names -- restore them

q1_labels <- c(
  "presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
  "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person"
)
typ_labels <- c("Master", "Manager", "User", "Guardian", "Partner", "Object")

load_one_country <- function(path, country_name) {
  raw <- read_excel(path, sheet = "Sheet", col_names = TRUE)
  d <- raw[-1, ]  # row 1 is a sub-header (item labels), not a respondent
  n <- ncol(d)

  q1 <- as.data.frame(lapply(d[, 10:15], as.numeric))
  names(q1) <- q1_labels

  typ_now    <- as.numeric(d[[54]])
  typ_should <- as.numeric(d[[55]])

  # demographics are always the last 4 columns, or last 5 if the country
  # also asked the Indigenous/First-Nations question (Canada, Panama only)
  has_indigenous <- n == 95
  demo_start <- if (has_indigenous) n - 4 else n - 3
  gender_raw     <- as.numeric(d[[demo_start]])
  age_raw        <- as.numeric(d[[demo_start + 1]])
  education_raw  <- as.numeric(d[[demo_start + 2]])
  residence_raw  <- as.numeric(d[[demo_start + 3]])
  indigenous_raw <- if (has_indigenous) as.numeric(d[[demo_start + 4]]) else NA_real_

  # --- response-quality screen: speeding + straight-lining ---
  # (same logic used on Canada alone earlier in the project; now run for
  # all 6 countries, closing a Next-Steps item from PaperII_Outline.docx)
  duration_sec <- as.numeric(difftime(d$`End Date`, d$`Start Date`, units = "secs"))
  q3  <- as.data.frame(lapply(d[, 42:53], as.numeric))   # 12-item control-perception matrix
  q12 <- as.data.frame(lapply(d[, 66:79], as.numeric))   # 14-item purpose-of-visit matrix
  q3_sd  <- apply(q3, 1, sd, na.rm = TRUE)
  q12_sd <- apply(q12, 1, sd, na.rm = TRUE)
  flag_lowqual <- (duration_sec < 120) | (q3_sd == 0) | (q12_sd == 0)

  # --- other survey batteries, kept for construct-validity checks (09) ---
  g <- function(i) suppressWarnings(as.numeric(d[[i]]))
  validity <- data.frame(
    dial_nature      = rowMeans(sapply(16:27, g), na.rm = TRUE),  # Q2 now: dialogue possible with natural entities
    dial_human       = g(28),                                     # Q2 now: dialogue with another human
    control_mean     = rowMeans(q3, na.rm = TRUE),                # Q3: 1 exploit .. 4 subject to
    q10_message      = g(63),                                     # Q10: flood = message from autonomous nature
    q11_dialogue     = g(65),                                     # Q11: changes in place (4 = form of dialogue)
    q12_relax        = g(66),
    q12_communicates = g(73),
    q12_teaches      = g(74),
    q12_comfort      = g(77),
    q12_cheap        = g(79)
  )

  # --- agency of non-human beings (Q2 now, Q3 future): the same 12 targets
  #     rated twice, so this block supports a latent change model that the
  #     single-item role question cannot. Item 13 of each block (another
  #     human) is kept apart as a discriminant anchor, not as an indicator.
  agency_labels <- c("wild_animals", "pets", "plants", "forces", "river", "dunes",
                     "forest", "lake", "sea", "soil", "mountain", "place")
  agency_now    <- as.data.frame(lapply(16:27, g)); names(agency_now)    <- paste0("ag_now_", agency_labels)
  agency_future <- as.data.frame(lapply(29:40, g)); names(agency_future) <- paste0("ag_fut_", agency_labels)

  # --- two scenarios that state positions on the same control continuum as
  #     the role item: a flooded/destroyed place (Q11) and climate change
  #     (Q15). They are mutually exclusive stances, not indicators of one
  #     factor, so they are stored as items and never summed.
  scen <- as.data.frame(lapply(c(60:64, 81:84), g))
  names(scen) <- c("sc_not_message", "sc_tech_prevents", "sc_heed_signals",
                   "sc_nature_partner", "sc_humans_lose",
                   "cl_denial", "cl_tech_stops", "cl_cannot_stop", "cl_extinction")

  # --- relationship to one's OWN favourite place: four ordered stances, each
  #     a sub-indicator of the integrated Me-Place indicator (Q7 emancipation,
  #     Q8 dialogue, Q9 agency, Q10 learning), plus Q12 (does the place change,
  #     and can that change be communication). Ordinal 1-4, never summed
  #     without checking they form a scale (alpha .61).
  meplace <- as.data.frame(lapply(c(56:59, 65), g))
  names(meplace) <- c("mp_emancipation", "mp_dialogue", "mp_agency", "mp_learning", "mp_time")

  # --- societal control battery (Q4): for twelve entities, how far do humans
  #     exploit / manage / coexist with / are subject to them. Ordinal 1-4.
  #     This is the SOCIETAL counterpart of the personal me-place stances.
  ctl_labels <- c("wild_animals", "pets", "plants", "forces", "river", "dunes",
                  "forest", "lake", "sea", "soil", "mountain", "place")
  control <- as.data.frame(lapply(42:53, g)); names(control) <- paste0("ctl_", ctl_labels)

  # --- reasons for staying in a natural place (Q13, 14 items) ---
  reason_labels <- c("relax", "beauty", "meet_people", "watch_plants", "meet_animals",
                     "own_thoughts", "quiet", "communicates", "teaches", "struggles",
                     "active", "comfort", "photos", "cheap")
  reasons <- as.data.frame(lapply(66:79, g)); names(reasons) <- paste0("rs_", reason_labels)

  # --- response-style indices from 10 agree/disagree items with opposing content:
  #     Q14 (3 pro- and 3 anti-tourism), Q10 "normal event" vs "message from nature",
  #     Q13 "can be stopped" vs "cannot be stopped" (Billiet & McClendon, 2000 logic) ---
  rs <- sapply(c(85:90, 60, 63, 82, 83), g)
  style <- data.frame(
    ARS = rowMeans(rs >= 4, na.rm = TRUE),              # acquiescence: share agreeing
    MRS = rowMeans(rs == 3, na.rm = TRUE),              # midpoint ("difficult to say")
    ERS = rowMeans(rs == 1 | rs == 5, na.rm = TRUE)     # extreme responding
  )

  out <- cbind(
    data.frame(
      country        = country_name,
      start_date     = d$`Start Date`,
      end_date       = d$`End Date`,
      duration_sec   = duration_sec,
      flag_lowqual   = flag_lowqual,
      typ_now        = typ_now,
      typ_should     = typ_should,
      gender_raw     = gender_raw,
      age_raw        = age_raw,
      education_raw  = education_raw,
      residence_raw  = residence_raw,
      indigenous_raw = indigenous_raw
    ),
    q1, validity, style, agency_now, agency_future, reasons, scen, meplace, control
  )
  out
}

all_data <- do.call(rbind, lapply(names(files), function(cn) load_one_country(files[[cn]], cn)))
all_data$country      <- factor(all_data$country, levels = c("Canada","Panama","Poland","Netherlands","Spain","Sweden"))
all_data$typ_now_f     <- factor(all_data$typ_now, levels = 1:6, labels = typ_labels)
all_data$typ_should_f  <- factor(all_data$typ_should, levels = 1:6, labels = typ_labels)
all_data$relational    <- rowMeans(all_data[, q1_labels])
# language of the questionnaire version each country received
all_data$language <- factor(ifelse(all_data$country %in% c("Canada","Netherlands","Sweden"), "English",
                             ifelse(all_data$country == "Poland", "Polish", "Spanish")))

cat("Loaded", nrow(all_data), "respondents across", nlevels(all_data$country), "countries:\n")
print(table(all_data$country))

cat("\n--- Fielding windows per country (NOT simultaneous -- see caveats above) ---\n")
fielding_dates <- do.call(rbind, lapply(split(all_data, all_data$country), function(x) {
  data.frame(country = x$country[1], start = min(x$start_date), end = max(x$end_date))
}))
print(fielding_dates[, c("country","start","end")], row.names = FALSE)
saveRDS(fielding_dates, "hnr_fielding_dates.rds")

cat("\n--- Response-quality screen (speeding < 120s OR straight-lining on Q3/Q12) ---\n")
print(table(all_data$country, all_data$flag_lowqual, dnn = c("country","flagged")))
cat("Total flagged:", sum(all_data$flag_lowqual), "of", nrow(all_data), "\n")
cat("Flagged respondents are kept in hnr_data.rds (flag_lowqual column) rather than\n")
cat("dropped -- filter them out per-analysis (e.g. subset(dat, !flag_lowqual)) if you\n")
cat("want the excluded-flagged version; the Canada-only check earlier in the project\n")
cat("found results unchanged either way, but that hasn't been re-confirmed for the\n")
cat("other 5 countries yet.\n")

saveRDS(all_data, "hnr_data.rds")
cat("\nSaved: hnr_data.rds, hnr_fielding_dates.rds\n")
