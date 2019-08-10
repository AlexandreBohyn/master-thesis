# BSS: INTRODUCTION ----
#' Creates the visualization plots for the BSS models data from the SAS
#' proc mixed
#' Inputs:
#' - SAS datasets from prox mixed for all 4 variables
#'
#' Outputs:
#' - Variograms
#' - Residuals exploration plots

# USER DESIGNED FUNCTIONS -----------------------------------------------------
library(SpATS)
library(plot3D)
library(gridExtra)
vars <- c('FRESH_LS','FRESH_RS','DRY_LS','DRY_RS')
load(file="SpATSFits.RData")

# SPATS VARIOGRAMS ------------------------------------------------------------------

# Get 3D vario data and plot them
GetSpATS3DVario <- function(var,min.length=30, theta= -45, phi=15){
  x <- SpATS::variogram(fits[[var]])
  values <- matrix(replace(x$data$value, x$data$length < min.length, 
                           NA), ncol = length(x$col.displacement), 
                   nrow = length(x$row.displacement), 
                   byrow = TRUE)
  p <- persp3D(x$row.displacement, x$col.displacement, 
          values, xlab = "Row displacement", ylab = "Col displacement", 
          zlab = "", ticktype = "detailed", col = topo.colors(100), facets = FALSE,
          main = var , expand = 0.5, theta = theta, phi = phi)
  return(p)
}

# Aply to all vars
for (i in vars){
  plotname <- paste0("Figures/",i,"_3DVario.pdf")
  pdf(plotname, width = 5.5, height = 5)
  GetSpATS3DVario(i)
  dev.off()
}

#Get 2D vario data and plot them
GetSpats2DIndVario <- function(var,min.length=30){
  x <- variogram(fits[[var]])
  values <- matrix(replace(x$data$value, x$data$length < min.length, 
                           NA), ncol = length(x$col.displacement), 
                   nrow = length(x$row.displacement), 
                   byrow = TRUE)
  ColData <- as_tibble(cbind(Disp = x$col.displacement,val = values[1,]))
  RowData <- as_tibble(cbind(Disp = x$row.displacement,val = values[,1]))
  p1 <- ggplot(ColData,aes(x=Disp, y = val))+
    geom_point()+
    geom_line()+
    theme_bw()+
    labs(x = "Distance in positions (columns)",
         y = "Semivariance",
         title = var)
  p2 <- ggplot(RowData,aes(x=Disp, y = val))+
    geom_point()+
    geom_line()+
    theme_bw()+
    labs(x = "Distance in strips (rows)",
         y = "Semivariance",
         title = var)
  gridExtra::marrangeGrob(list(p1,p2), ncol = 1, nrow=2, top=NULL)
}

#Apply to all vars
for (i in vars){
  plotname <- paste0("Figures/",i,"_2DIndVario.pdf")
  pdf(plotname, width = 5.5, height = 5)
  GetSpats2DIndVario(i)
  dev.off()
}

# BSS VARIOGRAM -------------------------------------------------------------------

# Get BSS residuals
GetBSSResid <- function(var){
  path <- paste0('C:/Users/alexa/Documents/SASUniversityEdition/myfolders/Alex/',
                 var,'_PRED.xlsx')
  temp <- read_xlsx(path)%>%
    select(STRIP,POSITION,Resid)
  V <- data.frame(col.disp = double(),
              row.disp = double(),
              value = double())
  init <- 1
  for (i in 503:1){
    print("Iteration #:")
    print(i)
    for (j in 1:i){
      col.disp  <- abs(temp[i,]$POSITION - temp[j,]$POSITION)
      row.disp <- abs(temp[i,]$STRIP - temp[j,]$STRIP)
      value <- 0.5*(temp[i,]$Resid - temp[j,]$Resid)^2
      V[init,] <- c(col.disp,row.disp,value)
      init = init+1
      
    }
  }
  V%>%
    group_by(
      col.disp,row.disp
    )%>%
    summarise(avg = mean(value, na.rm = T),
              length = n()) -> vario
  
  values <- matrix(replace(vario$avg, vario$avg < min.length, 
                           NA), ncol = 20, 
                   nrow = 50, 
                   byrow = TRUE)
}



# RESIDUAL ANALYSIS PLOTS -----------------------------------------------------

# GENOTYPE COMPARISONS -------------------------------------------------------

# Geno estimates from BSS

GetGenoEst <- function(var){
  path <- paste0('C:/Users/alexa/Documents/SASUniversityEdition/myfolders/Alex/',
                 var,'_SOLUTIONR.xlsx')
temp <- read_xlsx(path)%>%
           dplyr::filter(Effect == "geno")%>%
  select(geno,GenoBSS = Estimate)
return(temp)
}

# Geno PEV from BSS

GetGenoPEV <- function(var){
  path <- paste0('C:/Users/alexa/Documents/SASUniversityEdition/myfolders/Alex/',
                 var,'_SOLUTIONR.xlsx')
  temp <- read_xlsx(path)%>%
    dplyr::filter(Effect == "geno")%>%
    summarise(PEV = mean(StdErrPred))
  return(temp)
}

# Geno Variance from BSS

GetGenoVar <- function(var){
  path <- paste0('C:/Users/alexa/Documents/SASUniversityEdition/myfolders/Alex/',
                 var,'_COVPARMS.xlsx')
  temp <- read_xlsx(path)%>%
    dplyr::filter(CovParm == "geno")%>%
    select(Var = Estimate)
  return(temp)
}

# Plots of the genotype BLUPs

GenoComparativePlot <- function(var){
  GenoSpATS <- fits[[var]]$coef[1:30]
  GenoBSS <- GetGenoEst(var)
  GenoEst <- cbind(GenoBSS,GenoSpATS)
  GenoCorr <- cor(GenoEst$GenoBSS,GenoEst$GenoSpATS)
  ggplot(GenoEst, aes(x=GenoSpATS,y=GenoBSS))+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
    geom_point()+
    coord_fixed(ratio=1)+
    labs(x = "BLUPs from SpATS",
         y = "BLUPs from BSS",
         title = var)+
    ggalt::annotate_textp(x = 0.9, y = 0.1, 
             label = round(GenoCorr,3))+
    theme_bw()
}

# Apply to all vars
GenoMappedPlots <- map(vars, ~GenoComparativePlot(.x))

# Plot in a 2 by 2 matrix
GenoArrangedPlots <-  marrangeGrob(GenoMappedPlots, ncol = 2, nrow=2, top=NULL)
ggsave(filename = "Figures/Genotype_Comparative_plots.pdf", 
       plot = GenoArrangedPlots, width = 5.5, height = 7)









