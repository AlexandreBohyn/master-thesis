# SPATS: INTRODUCTION ----
#' Load the SpATS package and state the base parameters.
#' Use the 5 variables to run the model.
#' Extract the comparison information from the model's output
#' 
#' Inputs:
#' - Data table (.RData)
#' - Outliers table (outliers.xlsx)
#' - Genotype information table (GENO_INFO.xlsx)
#'
# USER DESIGNED FUNCTIONS -----------------------------------------------------

library(SpATS)

# SPATS PARAMETERS ------------------------------------------------------------

vars <- c('FRESH_LS','FRESH_RS','DRY_LS','DRY_RS')
genotype <- "GENOTYPE"
geno.decomp <- NULL
spatial <- ~formula()
fixed <- ~formula()
random <- ~formula()
data <- data_table
family <- gaussian()
offset <- 0
weights <- data_table$w_WEIGHTS

# SPATS MODEL -----------------------------------------------------------------

SpATS(response, genotype, geno.decomp = NULL,
      genotype.as.random = FALSE, spatial, 
      fixed = NULL, random = NULL, data,
      family = gaussian(), offset = 0, weights = NULL, 
      control = controlSpATS())
