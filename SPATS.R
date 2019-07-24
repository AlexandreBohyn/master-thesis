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
library(gridExtra)

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
dim <- data.frame(Model = c(1,1,1,1,1,30,5,99,6,100,6,100,150))
for(i in c(1:4)){
  dim <- cbind(dim, fits[[i]]$eff.dim)
}
colnames(dim) <- c('Model','Effective_FRESH_LS','Effective_FRESH_RS',
                     'Effective_DRY_LS','Effective_DRY_RS')

#write.xlsx(t(dim), file = "Tables/dim.xlsx")

# Summary measures
summary(fit.spats)
summary(fit.spats, which = "variances")

# Get the heritability
herit <- c()
for(i in 1:4){
  herit <- append(herit,getHeritability(fits[[i]]))
}
names(herit) <- vars

# Extract the residuals
res <- matrix(nrow = 503, ncol = 0)
for(i in 1:4){
  res <- cbind(res,fits[[i]]$residuals)
}
colnames(res) <- vars
res <- as.data.frame(res)

# Get the variance of all the components
var <- matrix(nrow = 8, ncol = 0)
for(i in 1:4){
  var <- cbind(var,fits[[i]]$var.comp)
}
colnames(var) <- vars

print(xtable(t(var), digits = 4,
             caption = "Variances of all the components of the SpATS model"),
      file = "Tables/latex_SpATS_variance.txt",
      append = FALSE,
      include.rownames = TRUE,
      caption.placement = "top",
      booktabs = TRUE,
      timestamp = NULL,
      comment = FALSE)

# PLOTS ---------------------------------------------------------------------------


# RESIDUALS ------------------------------------------------------------------------

# Residuals analysis plots
lagplot <- function(colname){
  forecast::gglagchull(res[[colname]], lags = 10, diag = TRUE, do.lines = FALSE,
                       diag.col = "black") +
    coord_fixed()+
    theme_bw()+
    labs(x = "Original residuals", y = "Lagged residuals",
         title = colname)+
    theme(legend.position = "none")
}
lag_plots <- map(vars, ~lagplot(.x))

# Residuals normal distribution plot
n = 503
mean = 0

normal_res_plot <- function(colname){
  sd = sd(res[[colname]])
  binwidth = (max(res[[colname]])-min(res[[colname]]))/50
  set.seed(1)
  df <- data.frame(x = rnorm(n, mean, sd))
  var <- enquo(colname)
  ggplot(res, aes(x = eval(parse_expr(!!var)), mean = mean, sd = sd,
                  binwidth = binwidth, n = n))+
    theme_bw() +
    geom_histogram(binwidth = binwidth, 
                   colour = "white", fill = "cornflowerblue", size = 0.1) +
    stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth,
                  color = "darkred", size = 1)+
    labs(title = colname,
         x = "Residuals",
         y = "Count")
}

norm_plots <- map(vars, ~normal_res_plot(.x))
plots <- c(lag_plots,norm_plots)
names(plots) <- c('lag1','lag2','lag3','lag4','norm1','norm2','norm3','norm4')
plots <- plots[c('lag1','norm1','lag2','norm2','lag3','norm3','lag4','norm4')]
residuals_plot <- marrangeGrob(plots, ncol = 4, nrow=2, top=NULL)

ggsave(filename = "Figures/residuals_plot.pdf", plot = residuals_plot,
       height = 3.6, width = 7.5, scale = 1.5)

