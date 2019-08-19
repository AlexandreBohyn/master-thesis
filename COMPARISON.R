#load the Dataset
load("data.RData")
load("SpATSFits.RData")

# TANK difference ----

# Summarise data
TANKA <- filtered_data%>%
  dplyr::filter(TANK == "A")%>%
  select(FRESH_LS,FRESH_RS,DRY_LS,DRY_RS)%>%
  as.matrix()

TANKB <- filtered_data%>%
  dplyr::filter(TANK == "B")%>%
  select(FRESH_LS,FRESH_RS,DRY_LS,DRY_RS)%>%
  as.matrix()

# Hotteling Tsquared test
Compositional::hotel2T2(TANKA,TANKB)

# Individuals t-tests
vars <- c('FRESH_LS','FRESH_RS','DRY_RS','DRY_LS')
TANK_tTest <- c()
for(i in vars){
  fmla <- as.formula(paste0(i,'~TANK'))
  TANK_tTest <- append(TANK_tTest,
                       t.test(fmla, filtered_data)$p.value)
}

# 2 groups MANOVA
manoTank <- manova(cbind(FRESH_RS,FRESH_LS,DRY_LS,DRY_RS) ~ TANK, filtered_data)
summary(manoTank)

# t-test by Genotype
TANK_GENO_tTest <- c()
for(g in 1:30){
  for(i in vars){
    temp <- filtered_data%>%
      dplyr::filter(REAL_GENOTYPE == g)
    fmla <- as.formula(paste0(i,'~TANK'))
    t <- round(t.test(fmla,temp)$p.value,4)
    TANK_GENO_tTest <- rbind(TANK_GENO_tTest,
                             c(g,i,t))
  }
}
colnames(TANK_GENO_tTest) <- c('Geno','Variable','P.Value')
TANK_GENO_tTest <- TANK_GENO_tTest%>%
  as_tibble()%>%
  spread(Variable,P.Value)

num_obs <- filtered_data%>%
  group_by(REAL_GENOTYPE)%>%
  summarise(n())


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
