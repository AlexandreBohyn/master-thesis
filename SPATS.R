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
# SPATS PARAMETERS ------------------------------------------------------------

library(SpATS)

<<<<<<< Updated upstream
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
=======
# Load the data
load("data.RData")

# Reformat the data table
data <- filtered_data%>%
  select(-QR)
data$POSITION <- ifelse(data$TANK == "A",
                        as.numeric(data$POSITION),
                        as.numeric(data$POSITION)+5)

# Factor the variables
data$S <- as.factor(filtered_data$STRIP)
data$P <- as.factor(filtered_data$POSITION)
data$T <- as.factor(filtered_data$TANK)
data$geno <- as.factor(filtered_data$REAL_GENOTYPE)

# Specify plot layout
nseg1 <- 5
nseg2 <- 99

# Additional SpATS parameters
vars <- c('FRESH_LS','FRESH_RS','DRY_LS','DRY_RS')
weights <- filtered_data$w_WEIGHT

# SPATS MODEL -----------------------------------------------------------------

# Fit the tensor product of P-splines
spatial.SpATS <- as.formula(
  ~PSANOVA(POSITION, STRIP, 
           nseg = c(nseg1, nseg2), 
           degree = 3, pord = 2, 
           nest.div = 2)
)

# Create the SpATS model as a function 
fit.spats <- function(x){
  return(
    SpATS(response = x, 
          genotype = "geno",
          geno.decomp = NULL,
          genotype.as.random = TRUE, 
          spatial = spatial.SpATS, 
          fixed = ~ 1 + T, 
          random = ~ P + S,
          data = data,
          family = gaussian(),
          offset = 0,
          weights = weights,
          control =  list(tolerance = 1e-03))
  )
}

# Map it to all the variables
fits <- map(vars, ~fit.spats(.x))

# SUMMARY ---------------------------------------------------------------------------

# Summary measures
summary(fit.spats)
summary(fit.spats, which = "variances")


# PLOTS -----------------------------------------------------------------------------

# Plots
plot(fit.spats, depict.missing = TRUE)
>>>>>>> Stashed changes
