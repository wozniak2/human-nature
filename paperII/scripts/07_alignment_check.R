# ============================================================
# 07_alignment_check.R
# Sensitivity check for the multi-group CFA / partial-scalar
# invariance approach in 02_cfa_invariance.R: the alignment
# method (Asparouhov & Muthen, 2014) estimates approximate
# invariance across many groups WITHOUT needing to find a common
# invariant item subset -- unlike the classical configural ->
# metric -> partial-scalar route, which required an ad hoc choice
# (free person_changes_place and place_influences_person based on
# modification indices). This checks whether the country ranking
# on the relational-entanglement factor is an artefact of that
# specific choice.
#
# Requires: 02_cfa_invariance.R already run (hnr_cfa_fits.rds has
# the configural fit)
# ============================================================

suppressMessages(library(lavaan))
if (!requireNamespace("sirt", quietly = TRUE)) install.packages("sirt", repos = "https://cloud.r-project.org")
suppressMessages(library(sirt))

fits <- readRDS("hnr_cfa_fits.rds")
fit_config <- fits$configural
dat <- readRDS("hnr_data.rds")
countries <- levels(dat$country)

q1_labels <- c(
  "presence_influences_place", "place_influences_person", "person_shares_stories_w_place",
  "place_tells_story_to_person", "person_changes_place", "place_leaves_traces_in_person"
)

# --- extract per-country loadings (lambda) and intercepts (nu) from the
#     configural (unconstrained) multi-group CFA already fit in 02 ---
pe <- parameterEstimates(fit_config)
lambda <- matrix(NA_real_, nrow = length(countries), ncol = length(q1_labels),
                  dimnames = list(countries, q1_labels))
nu <- matrix(NA_real_, nrow = length(countries), ncol = length(q1_labels),
             dimnames = list(countries, q1_labels))
for (g in seq_along(countries)) {
  sub <- pe[pe$group == g, ]
  load_rows <- sub[sub$op == "=~" & sub$lhs == "relational_f", ]
  int_rows  <- sub[sub$op == "~1" & sub$lhs %in% q1_labels, ]
  lambda[g, load_rows$rhs] <- load_rows$est
  nu[g, int_rows$lhs] <- int_rows$est
}

cat("Loadings by country (configural model):\n"); print(round(lambda, 3))
cat("\nIntercepts by country (configural model):\n"); print(round(nu, 3))

# --- run the alignment optimization ---
align <- sirt::invariance.alignment(lambda = lambda, nu = nu)

cat("\n========== Alignment: approximate-invariance summary ==========\n")
print(summary(align))

cat("\n========== Aligned factor means by country (approximate invariance) ==========\n")
aligned_means <- align$pars$alpha0
names(aligned_means) <- countries
print(round(aligned_means, 4))

cat("\n--- Re-centred on Canada (to match the partial-scalar model's reference) ---\n")
aligned_vs_canada <- aligned_means - aligned_means["Canada"]
print(round(aligned_vs_canada, 4))
pp <- parameterEstimates(fits$partial)
pl <- pp[pp$op == "~1" & pp$lhs == "relational_f", ]
cat("\nAligned vs partial-scalar multi-group latent means (Canada = 0):\n")
print(data.frame(aligned = round(aligned_vs_canada, 3),
                 partial_scalar = round(pl$est[match(seq_along(countries), pl$group)], 3)))

cat("\n========== Item-level residuals: which items are least invariant ==========\n")
cat("Loading residuals (abs, by item, averaged across countries):\n")
print(round(colMeans(abs(align$lambda.resid)), 4))
cat("\nIntercept residuals (abs, by item, averaged across countries):\n")
print(round(colMeans(abs(align$nu.resid)), 4))
cat("(compare against the 2 items freed in the classical partial-scalar model:\n")
cat(" person_changes_place, place_influences_person -- do they also show the largest\n")
cat(" residuals here? If so, both methods agree on which items are least invariant.)\n")

saveRDS(align, "hnr_alignment_fit.rds")
cat("\nSaved: hnr_alignment_fit.rds\n")
