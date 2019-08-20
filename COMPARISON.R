#load the Dataset
load("data.RData")
load("SpATSFits.RData")

# TANK difference ----
vars <- c("FRESH_LS","FRESH_RS","DRY_LS","DRY_RS")
tResults <- matrix(nrow = 3,ncol = 4)
for(i in 1:4){
  fml <- as.formula(paste0(vars[i],"~TANK"))
  t <- t.test(fml,filtered_data)
  tResults[1,i] <- round(t$estimate[1],4)
  tResults[2,i] <- round(t$estimate[2],4)
  tResults[3,i] <- t$p.value
}


pairedResults <- matrix(ncol = 30, nrow = 4)
for(j in 1:30){
  for (i in 1:4){
    a <- filtered_data%>%
      dplyr::filter(REAL_GENOTYPE == j)
    fml <- as.formula(paste0(vars[i],"~TANK"))
    t <- t.test(fml,data=a)
    pairedResults[i,j] <- t$p.value
  }
}


# GENOTYPE diff -----
GetGenoEst <- function(var){
  path <- paste0('C:/Users/alexa/Documents/SASUniversityEdition/myfolders/Alex/',
                 var,'_SOLUTIONR.xlsx')
  temp <- read_xlsx(path)%>%
    dplyr::filter(Effect == "geno")%>%
    select(geno,GenoBSS = Estimate)
  return(temp)
}
GetGenoPEV <- function(var){
  path <- paste0('C:/Users/alexa/Documents/SASUniversityEdition/myfolders/Alex/',
                 var,'_SOLUTIONR.xlsx')
  temp <- read_xlsx(path)%>%
    dplyr::filter(Effect == "geno")%>%
    select(geno, PEVBSS = StdErrPred)
  return(temp)
}
GenoEst <- list()
for (i in vars){
 temp <- full_join(GetGenoEst(i),
               as_tibble(cbind(GenoSpATS = fits[[i]]$coeff[1:30],
                               geno = c(1:30))),
               by = "geno")
temp <- full_join(temp,
                  GetGenoPEV(i),
                  by = "geno")
GenoEst[[i]] <- full_join(temp,
                          as_tibble(cbind(PEVSpATS = sqrt(diag(fits[[i]]$vcov$C11_inv)),
                                          geno = c(1:30))),
                          by = "geno")

}

GenoEst[['DRY_LS']]%>%
  ggplot(aes(x = fct_reorder(as.factor(geno),GenoBSS, .desc = T), y = GenoBSS))+
  geom_point()
