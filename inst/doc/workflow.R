## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(mispitools)
library(forrel)     # For genetic simulations
library(pedtools)   # For pedigree operations

## -----------------------------------------------------------------------------
# Probability of misrecording biological sex
eps_sex <- 0.02

# Probability of age estimate being outside true range
eps_age <- 0.05

# Hair color observation error matrix
eps_color <- error_matrix_hair(ep = 0.05)  # 5% error rate

## -----------------------------------------------------------------------------
# MP is female (H=1), POI observed as female
lr_sex_result <- lr_sex(
  LR = TRUE,
  H = 1,          # True hypothesis (MP is female)
  nsims = 1,
  eps = eps_sex,
  erRs = 0.01     # Database recording error
)

cat("LR for sex evidence:", lr_sex_result$LRs, "\n")

## -----------------------------------------------------------------------------
# MP age = 25, tolerance = 5 years (so 20-30 is acceptable)
# POI age = 27 (falls within range)
lr_age_result <- lr_age(
  LR = TRUE,
  H = 1,          # True hypothesis
  nsims = 1,
  epa = eps_age,
  erRa = 0.01,
  MPa = 25,       # MP age
  MPr = 5         # Range tolerance
)

cat("LR for age evidence:", lr_age_result$LRa, "\n")

## -----------------------------------------------------------------------------
# MP has color 2 (dark brown), POI observed as color 2
lr_color_result <- lr_hair_color(
  LR = TRUE,
  H = 1,          # True hypothesis
  nsims = 1,
  MPc = 2,        # MP hair color
  epc = eps_color,
  erRc = eps_color
)

cat("LR for hair color:", lr_color_result$LRc, "\n")

## -----------------------------------------------------------------------------
# Combine all non-genetic evidence
lr_nongenetic <- lr_sex_result$LRs * lr_age_result$LRa * lr_color_result$LRc
cat("Combined non-genetic LR:", round(lr_nongenetic, 2), "\n")
cat("Log10(LR):", round(log10(lr_nongenetic), 2), "\n")

## ----fig.height=4-------------------------------------------------------------
# Population CPT (H2)
cpt_h2 <- cpt_population(
  propS = c(0.5, 0.5),
  MPa = 25,
  MPr = 5,
  propC = c(0.15, 0.35, 0.25, 0.15, 0.10)  # Realistic color distribution
)

# MP CPT (H1)
cpt_h1 <- cpt_missing_person(
  MPs = 1,        # Female
  MPc = 2,        # Dark brown
  eps = eps_sex,
  epa = eps_age,
  epc = eps_color
)

# Visualize both CPTs and LR heatmap
plot_cpt(cpt_h2, cpt_h1)

## ----eval=FALSE---------------------------------------------------------------
# # Create a simple pedigree: parent-child relationship
# # The missing person (ID 5) is child of individual 2
# 
# # Using linearPed to create grandparent-parent-child
# ped <- linearPed(2)  # 5 individuals
# 
# # Add genetic markers from Norwegian population
# ped <- setMarkers(ped, locusAttributes = NorwegianFrequencies[1:10])
# 
# # Simulate a profile for the reference person
# set.seed(123)
# ped <- profileSim(ped, N = 1, ids = 2)[[1]]

## ----eval=FALSE---------------------------------------------------------------
# # Simulate genetic LRs
# genetic_sims <- sim_lr_genetic(
#   reference = ped,
#   missing = 5,
#   numsims = 100,
#   seed = 456
# )
# 
# # Convert to dataframe
# genetic_df <- lr_to_dataframe(genetic_sims)
# 
# # Visualize
# plot_lr_distribution(genetic_df)

## -----------------------------------------------------------------------------
# Example genetic LR values (pre-computed)
# These represent typical values from parent-child testing
set.seed(42)
genetic_df <- data.frame(
  Related = 10^rnorm(100, mean = 3, sd = 1.5),
  Unrelated = 10^rnorm(100, mean = -0.5, sd = 1)
)

cat("Summary of log10(LR) under H1 (Related):\n")
summary(log10(genetic_df$Related))

cat("\nSummary of log10(LR) under H2 (Unrelated):\n")
summary(log10(genetic_df$Unrelated))

## ----fig.height=4-------------------------------------------------------------
# Plot the LR distributions
plot_lr_distribution(genetic_df)

## -----------------------------------------------------------------------------
# If there are ~10,000 potential candidates
prior_prob <- 1/10000

# Convert to prior odds
prior_odds <- prior_prob / (1 - prior_prob)
cat("Prior probability:", prior_prob, "\n")
cat("Prior odds:", prior_odds, "\n")

## -----------------------------------------------------------------------------
# For the simulations under H1 (true match scenario)
posterior_h1 <- prior_odds * genetic_df$Related * lr_nongenetic

# For simulations under H2 (no match scenario)
posterior_h2 <- prior_odds * genetic_df$Unrelated * lr_nongenetic

# Summary
cat("Posterior odds under H1 (median):", round(median(posterior_h1), 4), "\n")
cat("Posterior odds under H2 (median):", round(median(posterior_h2), 6), "\n")

## ----fig.height=4-------------------------------------------------------------
# Find optimal threshold with weight 10 (FP 10x worse than FN)
threshold_result <- decision_threshold(
  datasim = genetic_df,
  weight = 10
)

# Calculate error rates at the optimal threshold
rates <- threshold_rates(
  datasim = genetic_df,
  threshold = threshold_result
)

# Check rates at different thresholds
cat("\nError rates at different thresholds:\n")
for (t in c(1, 10, 100, 1000)) {
  r <- threshold_rates(genetic_df, threshold = t)
  cat(sprintf("LR > %5d: FPR=%.3f, FNR=%.3f, MCC=%.3f\n",
              t, r$FPR, r$FNR, r$MCC))
}

# Plot decision curve
plot_decision_curve(
  datasim = genetic_df,
  LRmax = 10000
)

## -----------------------------------------------------------------------------
# Median genetic LR under H1
median_genetic_lr <- median(genetic_df$Related)

# Total LR
total_lr <- median_genetic_lr * lr_nongenetic
log10_total <- log10(total_lr)

cat("Genetic LR (median under H1):", round(median_genetic_lr, 0), "\n")
cat("Non-genetic LR:", round(lr_nongenetic, 2), "\n")
cat("Total combined LR:", round(total_lr, 0), "\n")
cat("Log10(Total LR):", round(log10_total, 2), "\n")

## ----eval=FALSE---------------------------------------------------------------
# # Basic CPT and LR visualization
# app_mispitools()
# 
# # Advanced analysis with ROC curves
# app_lr_comparison()

## -----------------------------------------------------------------------------
sessionInfo()

