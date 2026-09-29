# Paper II — replication scripts

Analysis pipeline for *It's Complicated: Human–Nature Relationships Across Six Countries* (outline: `../PaperII_Outline_v4.docx`).

## Requirements
- R 4.4.1 (tested). Packages: readxl, lavaan, nnet, sirt — install with `source("00_setup.R")`.
- Raw survey exports (SurveyMonkey xlsx, one per country) in `../data/`:
  Canada, Panama, Poland, Netherlands, Spain, Sweden (file names are set in `01_load_and_prepare.R`).

## How to run
From this folder:
```
Rscript 00_setup.R
Rscript 00_run_pipeline.R
```
or open `00_run_pipeline.R` in RStudio and Source it. It deletes old `hnr_*.rds` outputs, runs every step, and writes `pipeline_log.txt` (all printed results) and `sessionInfo.txt`. Runtime: about 1–2 minutes.

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
| 11_square_table_models.R | Loglinear models for the 6x6 perceived-vs-ideal table: independence, quasi-independence, symmetry, quasi-symmetry; marginal homogeneity as the QS-vs-S contrast; fitted per country | hnr_square_table_models.rds |
| 10_figures.R | Figures 1-6 for the manuscript, PNG and PDF | figures/ |

The pipeline runs 01, 02, 04-09, 13, 14, 18, 17, 16, 15, 12, 11 and 10 in that order. `03_typology_dif.R` is superseded (it treated Q4 as personal endorsement) and is not run.

## Notes
- Items are identified by column position, verified to be identical across the six files; demographics are the last 4 columns (5 in Canada and Panama, which add an Indigenous-identity item).
- Education is not comparable across countries (different scales) and is not used in the models.
- Every number in the outline comes from `pipeline_log.txt`.
