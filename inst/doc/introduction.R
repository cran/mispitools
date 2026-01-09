## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from CRAN (when available)
# install.packages("mispitools")
# 
# # Or install the development version from GitHub
# # devtools::install_github("MarsicoFL/mispitools")

## -----------------------------------------------------------------------------
library(mispitools)

# LR for sex evidence
# H1: MP is female, POI observed as female
# eps = probability of sex observation error
lr_sex(LR = TRUE, H = 1, eps = 0.05)

## -----------------------------------------------------------------------------
# LR for age evidence
# MP age = 25, tolerance range = 5 years
# POI observed age falls within range
lr_age(LR = TRUE, H = 1, MPa = 25, MPr = 5, epa = 0.05)

## -----------------------------------------------------------------------------
# CPT under H2 (population hypothesis)
cpt_h2 <- cpt_population(
  propS = c(0.5, 0.5),  # 50% female, 50% male
  MPa = 30,             # MP age
  MPr = 5,              # Age range
  propC = c(0.3, 0.25, 0.2, 0.15, 0.1)  # Hair color proportions
)

# CPT under H1 (MP hypothesis)
cpt_h1 <- cpt_missing_person(
  MPs = 1,    # Female
  MPc = 2,    # Hair color 2
  eps = 0.05, # Sex error
  epa = 0.05, # Age error
  epc = error_matrix_hair()  # Hair color error matrix
)

# View dimensions
dim(cpt_h1)

## ----fig.height=4-------------------------------------------------------------
# Visualize both CPTs and LR heatmap
plot_cpt(cpt_h2, cpt_h1)

## ----eval=FALSE---------------------------------------------------------------
# # Basic CPT explorer
# app_mispitools()
# 
# # Advanced LR comparison with ROC analysis
# app_lr_comparison()

## -----------------------------------------------------------------------------
# Available databases
data(Argentina)
data(Europe)
data(USA)
data(Asia)
data(Austria)
data(BosniaHerz)
data(China)
data(Japan)

# View structure
dim(Argentina)
names(Argentina)[1:10]

