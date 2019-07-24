# SPATS: INTRODUCTION ----
#' Load the SpATS package and state the base parameters.
#' Use the 5 variables to run the model.
#' Extract the comparison information from the model's output
#' 
#' Inputs:
#' - Data table (.RData)
#' - Outliers table (outliers.xlsx)
#' - Genotype information table (GENO_INFO.xlsx)

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

# Dimensions table with percentage of total
dim <- matrix(nrow = 13, ncol = 0)
for(i in c(1:4)){
  dim <- cbind(dim, fits[[i]]$eff.dim)
}
colnames(dim) <- c('Effective_FRESH_LS','Effective_FRESH_RS',
                     'Effective_DRY_LS','Effective_DRY_RS')
perc <- function(x)(x/sum(x)*100)
dim <- dim%>%
  as_tibble()%>%
  mutate_all(list(percentage = ~perc))

data.frame(Model = c(1,1,1,1,1,30,5,99,6,100,6,100,150))

write.xlsx(t(dim), file = "Tables/dim.xlsx")

# Summary measures
summary(fit.spats)
summary(fit.spats, which = "variances")


# PLOTS -----------------------------------------------------------------------------

# Plots
plot(fit.spats, depict.missing = TRUE)
