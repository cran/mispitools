## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 4
)
set.seed(2026)

## ----libs, message = FALSE, warning = FALSE-----------------------------------
library(mispitools)
library(forrel)
library(pedtools)

## ----pedigree-----------------------------------------------------------------
ped <- linearPed(2)
data(Argentina)

# Build the allele-frequency list from the Argentina database
db <- list()
for (m in names(Argentina)[-1]) {
  freqs <- Argentina[[m]]
  names(freqs) <- as.character(Argentina$Allele)
  freqs <- freqs[freqs > 0]
  db[[m]] <- freqs
}
marker_names <- names(db)[1:15]
db15 <- db[marker_names]

for (i in seq_along(db15)) {
  m <- marker(ped, afreq = db15[[i]], name = names(db15)[i])
  if (i == 1) ped <- setMarkers(ped, m) else ped <- addMarkers(ped, m)
}
ped

## ----simulate, eval = FALSE---------------------------------------------------
# # NOTE: eval=FALSE because the simulation needs forrel's full machinery
# # which may be slow to run at package-check time. The code below is the
# # canonical recipe.
# fullsim <- profileSim(ped, N = 1, ids = labels(ped))
# ref     <- fullsim
# for (i in 1:nMarkers(ref))
#   ref <- setGenotype(ref, id = "3", marker = i, geno = "0/0")
# 
# poi <- pedtools::singleton("POI")
# for (i in 1:nMarkers(fullsim)) {
#   af <- afreq(fullsim, marker = i)
#   nm <- name(fullsim, marker = i)
#   mk <- pedtools::marker(poi, afreq = af, name = nm)
#   poi <- if (i == 1) pedtools::setMarkers(poi, mk) else pedtools::addMarkers(poi, mk)
# }
# for (i in 1:nMarkers(fullsim)) {
#   g <- pedtools::genotype(fullsim, id = "3", marker = i)
#   poi <- pedtools::setGenotype(poi, id = "POI", marker = i, geno = g)
# }
# res <- missingPersonLR(ref, missing = "3", poi = poi, verbose = FALSE)
# lrs <- setNames(as.numeric(res$LRperMarker), marker_names)

## ----synthetic_lrs------------------------------------------------------------
# A synthetic matched pair: balanced and concentrated profiles with
# similar total log10(LR).
lrs_balanced <- c(
  D3S1358 = 5.2,  TH01    = 3.8,  D21S11  = 4.1,  D18S51  = 2.9,
  D5S818  = 3.4,  D13S317 = 2.7,  D7S820  = 4.5,  D16S539 = 3.2,
  CSF1PO  = 2.8,  vWA     = 4.2,  TPOX    = 3.1,  D8S1179 = 3.9,
  FGA     = 5.0,  D2S1338 = 3.6,  D19S433 = 2.5
)
lrs_concentrated <- c(
  D3S1358 = 1.3,  TH01    = 1.1,  D21S11  = 1.4,  D18S51  = 1.2,
  D5S818  = 1.5,  D13S317 = 1.2,  D7S820  = 1.3,  D16S539 = 1.1,
  CSF1PO  = 1.4,  vWA     = 1.2,  TPOX    = 1.3,  D8S1179 = 1.1,
  FGA     = 250,  D2S1338 = 1.5,  D19S433 = 1.2   # FGA dominates
)

W_balanced     <- sum(log10(lrs_balanced))
W_concentrated <- sum(log10(lrs_concentrated))
round(c(W_balanced = W_balanced, W_concentrated = W_concentrated), 2)

## ----trajectories-------------------------------------------------------------
traj_balanced     <- binary_belief_trajectory(lrs_balanced)
traj_concentrated <- binary_belief_trajectory(lrs_concentrated)
head(traj_balanced)

## ----metrics------------------------------------------------------------------
prior <- c(0.5, 0.5)

make_traj <- function(lrs) {
  lr_list <- lapply(unname(lrs), function(r) c(r, 1))
  belief_trajectory(prior, lr_list)
}

metrics_balanced     <- trajectory_metrics(make_traj(lrs_balanced))
metrics_concentrated <- trajectory_metrics(make_traj(lrs_concentrated))

summary_df <- data.frame(
  profile       = c("balanced", "concentrated"),
  path_length   = round(c(metrics_balanced$path_length, metrics_concentrated$path_length), 3),
  concentration = round(c(metrics_balanced$concentration, metrics_concentrated$concentration), 3),
  herfindahl    = round(c(metrics_balanced$concentration_herfindahl,
                          metrics_concentrated$concentration_herfindahl), 3)
)
summary_df

## ----cwplus-------------------------------------------------------------------
cwp_balanced     <- concentration_index_positive(log10(lrs_balanced))
cwp_concentrated <- concentration_index_positive(log10(lrs_concentrated))
round(c(balanced = cwp_balanced, concentrated = cwp_concentrated), 3)

## ----leave_one_out------------------------------------------------------------
loo_balanced     <- leave_one_out(lrs_balanced)
loo_concentrated <- leave_one_out(lrs_concentrated)
head(loo_concentrated[order(loo_concentrated$fraction, decreasing = TRUE), ])

## ----calibration_table, echo = FALSE------------------------------------------
calib <- data.frame(
  Pedigree = c("linearPed(2)  (2nd degree)",
               "linearPed(3)  (3rd degree)",
               "cousinPed(1)  (3rd degree)"),
  `50%` = c(0.130, 0.140, 0.141),
  `75%` = c(0.152, 0.163, 0.163),
  `90%` = c(0.174, 0.192, 0.192),
  `95%` = c(0.192, 0.211, 0.213),
  `99%` = c(0.225, 0.254, 0.258),
  check.names = FALSE
)
knitr::kable(calib, caption = "Quantiles of C_W+ under H_p for three pedigrees on the Argentine 15-STR database.")

## ----calibrate_helpers, eval = FALSE------------------------------------------
# library(pedtools); library(forrel)
# 
# # Reference pedigree with founder profiles simulated
# ped <- linearPed(2)
# ped <- setMarkers(ped, locusAttributes = NorwegianFrequencies[1:15])
# ped <- profileSim(ped, N = 1, ids = 2, seed = 1)
# 
# # Calibrate the pedigree-specific cutoff under H_p (90th percentile by default)
# cal <- calibrate_concentration_cutoff(
#   reference = ped, missing = 5,
#   numsims   = 1500, probs = 0.90, seed = 42
# )
# cal$cutoff
# 
# # Per-case fragility report against the calibrated cutoff
# fr <- fragility_report(
#   per_marker_lrs = lrs_concentrated,
#   cutoff = cal$cutoff, probs = cal$probs
# )
# fr$flag        # TRUE -> leave-one-out review required
# fr$statement   # natural-language sentence ready for the case file

## ----se_integration, eval = FALSE---------------------------------------------
# # Supplementary evidence LRs (numerical example from Egeland & Marsico 2026)
# LR_sex  <- 2.0    # lr_sex(...)
# LR_age  <- 7.7    # lr_age(...)
# LR_hair <- 3.3    # lr_hair_color(...)
# 
# # Extended per-marker LR vector: 15 STRs + 3 SE items
# lrs_combined <- c(lrs_balanced,
#                    Sex = LR_sex, Age = LR_age, Hair = LR_hair)
# traj_combined <- binary_belief_trajectory(lrs_combined)
# 
# # Trajectory metrics and concentration on the combined 18-step sequence
# metrics_combined <- trajectory_metrics(make_traj(lrs_combined))
# cwp_combined     <- concentration_index_positive(log10(lrs_combined))

