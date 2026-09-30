# Paper II — replication scripts

Analysis pipeline for *Seeing Mastery, Wanting Less: Place Agency and the Role of Humans in Nature* (working title, Paper II). The repository front page is `../../README.md`.

## Requirements
- R 4.4.1 (tested). Packages: readxl, lavaan, nnet, sirt, psych, GPArotation, ggplot2, ggalluvial (and MASS, which ships with R) — install with `source("00_setup.R")`.
- Raw survey exports (SurveyMonkey xlsx, one per country) in `../data/`:
  Canada, Panama, Poland, Netherlands, Spain, Sweden (file names are set in `01_load_and_prepare.R`).

## How to run
From this folder:
```
Rscript 00_setup.R
Rscript 00_run_pipeline.R
```
or open `00_run_pipeline.R` in RStudio and Source it. It deletes old `hnr_*.rds` outputs, runs every step, and writes `pipeline_log.txt` (all printed results) and `sessionInfo.txt`. Runtime: about ten minutes (script 21 alone takes about four).

Always run script files. On this Windows machine, `Rscript -e "..."` crashes when reading xlsx.

## Steps
| Script | What it does | Output |
|---|---|---|
| 01_load_and_prepare.R | Reads the 6 files by column position, stacks them, adds QC flags (speeding < 120 s, straight-lining), response-style indices (ARS/MRS/ERS), validity batteries, questionnaire language, fielding dates | hnr_data.rds, hnr_fielding_dates.rds |
| 02_cfa_invariance.R | Belief scale (Q1, 6 items): multi-group CFA, configural → metric → scalar → partial scalar | hnr_cfa_fits.rds |
| 04_mimic_model.R | Belief scale: MIMIC model with country, age, gender (+ direct effects for 2 non-invariant items) | hnr_mimic_fit.rds |
| 05_mimic_interactions.R | Belief scale: country × age / country × gender (nested LRT) | hnr_mimic_interact_fit.rds |
| 06_qc_robustness.R | Belief scale: steps 02/04 without flagged respondents | hnr_qc_robustness_fits.rds |
| 07_alignment_check.R | Belief scale: alignment method (sirt) as a check on partial invariance | hnr_alignment_fit.rds |
| 08_mastery_paradox.R | **Core analysis**: perceived (Q4 "now") vs ideal (Q5 "should") human role — McNemar, Stuart–Maxwell, logistic models, language checks, QC robustness | hnr_mastery_paradox.rds |
| 09_response_style_validity.R | Belief scale: construct validity vs other batteries, response-style controls, 1- vs 2-factor check | hnr_style_validity.rds |
| 12_agency_scale.R | Agency of non-human beings (Q2/Q3): descriptives and reliability, one- vs two-factor structure, multi-group invariance, latent change from present to future, validity against the relational scale and the role item | hnr_agency_scale.rds |
| 13_reasons_efa.R | Reasons for staying (Q13, 14 items): factorability, parallel analysis, EFA in a country-stratified half, CFA in the held-out half, subscale reliability and relation to the role item | hnr_reasons_efa.rds |
| 14_motives_and_roles.R | The three motives as outcomes (country, age, gender) and as predictors of the ideal role: multinomial logit against a Manager reference, plus mastery-as-ideal and rejection-of-perceived-mastery contrasts | hnr_motives_roles.rds |
| 15_structural_model.R | One WLSMV model in which the three motives, the agency scale and the belief scale all predict mastery as the ideal, alongside age, gender and country; standardised paths and latent correlations | hnr_structural_model.rds |
| 16_motive_invariance.R | Multi-group CFA of the three-factor motive model across the six countries: metric invariance is borderline and scalar invariance fails, so motive levels cannot be compared between countries | hnr_motive_invariance.rds |
| 17_scenarios_and_roles.R | Two scenario blocks (a destroyed natural place, climate change) that state positions on the same control continuum as the role item: agreement by ideal role, and whether they add to the motives | hnr_scenarios.rds |
| 18_place_relationship.R | The me-place block (Q7-Q10) and the time-in-dialogue item (Q12): scale structure, personal versus societal control, whether they track the motives and agency, whether they change what predicts the role, and a structural model with a place latent | hnr_place_relationship.rds |
| 19_now_vs_should_sem.R | One WLSMV model with the same predictors (three motives, place-agency latent, societal-control latent, age, gender, country) for the role people SEE and the role they WANT, with a Wald test of each path across the two outcomes | hnr_now_vs_should_sem.rds |
| 20_opposing_roles_sem.R | The joint see-versus-want model of script 19 repeated for Guardian, Partner and Object, with a Wald test of every path across the two outcomes | hnr_opposing_roles_sem.rds |
| 21_mediation_sem.R | Parallel-mediation SEM: do the motives act on wanting or seeing mastery through place agency, general agency, reciprocal beliefs or societal control? Motives entered one at a time; a three-together run is kept as a sensitivity check. Slow: about four minutes | hnr_mediation_sem.rds |
| 22_geography.R | Geography as a possible mediator, moderator or predictor: whether the constructs account for the country differences in wanting mastery (they do not), whether country changes any relation (it does not), and whether reported urban or rural residence matters (it does not) | none |
| 23_robustness.R | Seven tests of the central result: stem-group split and leave-one-country-out, response-style covariates, place agency rebuilt without the control items, bootstrapped mediation, effects in probabilities, split-half replication, and exclusion of low-quality respondents | hnr_robustness.rds |
| 24_outcome_structure.R | Is the wanted role one ordered scale? Means by role, cut-point odds ratios, proportional-odds model, the ordering of the middle roles tested against alternatives, and a split into direction (Master versus Object) and extremity (either pole versus the middle) | hnr_outcome_structure.rds |
| 25_motive_measurement.R | How solid are the motive measures: item-level effects, alternative scorings, 300 random half-samples to test whether the restorative instability is noise, what the serviced factor is made of, and which motives can be compared across countries | hnr_motive_measurement.rds |
| 26_ordered_sem.R | One joint WLSMV model with four outcomes: the ordered position of the role seen and wanted, and its extremity (either pole), with a Wald test of every path across seeing and wanting | hnr_ordered_sem.rds |
| 27_ordered_mediation.R | Mediation of the restorative and dialogic motives on the ordered role through place agency and societal control, one motive at a time, with an ordered-logit bootstrap cross-check | hnr_ordered_mediation.rds |
| 28_story_model.R | The central story as one WLSMV model: restorative and dialogic motives, place agency and societal control as mediators, position and extremity of the wanted role as outcomes, with indirect and total effects | hnr_story_model.rds |
| 29_education_check.R | Education (three harmonised levels, built in 04): its distribution, what it goes with, the central estimates with and without it, and a test of moderation of place agency | hnr_education_check.rds |
| 30_indicator_connections.R | Country-adjusted links between the role seen, wanted, the shift and extremity and every other indicator (Q1-Q4, Q7-Q16), plus the indicators against each other | hnr_indicator_connections.rds |
| 31_place_agency_facets.R | Facet check for place agency: independence (Q7, Q9) versus communication (Q8, Q10, Q12); item overlap with the motives, one factor versus two facets, which facet is linked to the role, mediation through each facet (latent and bootstrap), and a zero-overlap test | hnr_place_agency_facets.rds |
| 32_who_rejects_mastery.R | The gap within each person: among the people who see Master, who keeps it and who rejects it (and where they go); the reverse move toward Master; the wanted role given the seen role; a clean-sample check | hnr_who_rejects.rds |
| 11_square_table_models.R | Loglinear models for the 6x6 perceived-vs-ideal table: independence, quasi-independence, symmetry, quasi-symmetry; marginal homogeneity as the QS-vs-S contrast; fitted per country | hnr_square_table_models.rds |
| 10_figures.R | Figures 1-20, PNG and PDF. The path diagrams (11, 12-14, 16, 17, 20) are drawn from the fitted lavaan models by the small ggplot2 engine in sem_plot_helpers.R | figures/, sem_plot_helpers.R |

The pipeline runs 01, 02, 04-09, 13, 14, 26, 27, 28, 25, 24, 23, 29, 30, 31, 32, 22, 21, 20, 19, 18, 17, 16, 15, 12, 11 and 10 in that order. The full run now takes roughly ten minutes, most of it script 21. `03_typology_dif.R` is superseded (it treated Q4 as personal endorsement) and is not run.

## Notes
- Items are identified by column position, verified to be identical across the six files; demographics are the last 4 columns (5 in Canada and Panama, which add an Indigenous-identity item).
- Education answer options differ by questionnaire (9, 8 and 7 codes). `04_mimic_model.R` harmonises them to three levels (primary, high school, higher) plus a not-stated flag, and every model of the role uses them as covariates (`29_education_check.R` shows what they change).
- Every number in the working outline comes from `pipeline_log.txt`; the outline itself is kept outside the repository.
