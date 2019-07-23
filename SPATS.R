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

# SPATS MODEL -----------------------------------------------------------------

# Fit the tensor product of P-splines
spatial.SpATS <- as.formula(
  ~PSANOVA(POSITION, STRIP, 
           nseg = c(nseg1, nseg2), 
           degree = 3, pord = 2, 
           nest.div = 2)
)

# Fit the SpATS model
fit.spats <- SpATS(response = "FRESH_RS", 
                   genotype = "geno",
                   geno.decomp = NULL,
                   genotype.as.random = TRUE, 
                   spatial = spatial.SpATS, 
                   fixed = ~ 1 + T, 
                   random = ~ P + S,
                   data = data,
                   family = gaussian(),
                   offset = 0,
                   #weights = "w_WEIGHT",
                   control =  list(tolerance = 1e-03))

fit.spats$deviance

fir.spats <- SpATS(response = "FRESH_RS", 
                   genotype = "geno",
                   geno.decomp = NULL,
                   genotype.as.random = TRUE, 
                   spatial = spatial.SpATS, 
                   fixed = ~ 1 , 
                   random = ~ P + S + T,
                   data = data,
                   family = gaussian(),
                   offset = 0,
                   #weights = "w_WEIGHT",
                   control =  list(tolerance = 1e-03))

fir.spats$deviance

#summary(fit.spats)
#plot(fit.spats, depict.missing = TRUE)
