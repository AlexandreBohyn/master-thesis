# METRICS: INTRODUCTION ----
#' Add the outliers file and compute the weight matrix.
#' Filter the data and compute weighted mean, var and quantiles
#' Creates custom boxplots for the diff. variables
#' Creates tables summarizing the metrics 
#' for each genotype, tank, etc...
#' 
#' Tables: mean and standard deviation for weight (D and F), AUPC, area
#' Plots: Boxplots of the repartition between genotypes 
#' and difference between tanks
#' 
#' Inputs:
#' - Data table (.RData)
#' - Outliers table (outliers.xlsx)
#' - Genotype information table (GENO_INFO.xlsx)
#'
# USER DESIGNED FUNCTIONS -----------------------------------------------------
QRreformat <- function(vec){
  #' Reformats the QRcode from the weighting data into standard QR format
  return(paste(toupper(str_sub(vec,1,1)),
               str_pad(str_sub(vec,3,-3), 2, pad = "0"),
               str_sub(vec,-1),
               sep = "_")
  )
}

# Load data table and design table
load("data.RData")

# OUTLIERS --------------------------------------------------------------------

# Load the outliers file
outliers <- read_xlsx("Inputs/outliers.xlsx", sheet = 'table', na = "0")%>%
  mutate(QR = QRreformat(CODE))%>%
  select(-COMMENT, -CODE,
         NOT_FG = NOT_FULL_GROWTH)%>%
  as.data.frame()

# Replace NA's by zeroes
outliers[is.na(outliers)] <- 0


# Load the weight table
weights_table <- read_xlsx('Inputs/outliers.xlsx', sheet = 'weights')

# Load the seeds initial weights
seed_weights <- read_xlsx('Inputs/GENO_INFO.xlsx', sheet = 'WEIGHT')
seed_weights <- seed_weights%>%
  mutate(WEIGHT = rowMeans(seed_weights)/10)%>%
  select(REAL_GENOTYPE = GENOTYPE, 
         S_WEIGHT = WEIGHT)

# WEIGHTS ---------------------------------------------------------------------

# Form temp dataset with all conditions and added final AREA and 
#   WEIGHT weights
filtered_data <- data_table%>%
  left_join(seed_weights, by = "REAL_GENOTYPE")%>%
  left_join(outliers, by = "QR")%>%
  mutate(w_WEIGHT = ifelse(NO_SEED == 1, 0,
                         ifelse(NO_RS ==1, 1 ,
                                ifelse(NO_LS == 1, 2, 
                                       ifelse(BAD_LS == 1, 3,
                                              ifelse(NOT_FG == 1, 4, 5)
                                              )
                                       )
                                )
                         ),
         w_AREA = ifelse(OVERLAP ==1, 0, 1) 
         )%>%
  select(-c(NO_RS, NO_LS, BAD_LS, NOT_FG, NO_SEED, OK, OVERLAP))

# Fix the wrong NA genotypes
filtered_data <- filtered_data%>%
  mutate(REAL_GENOTYPE = ifelse(is.na(REAL_GENOTYPE) & w_WEIGHT > 0,
                                GENOTYPE, REAL_GENOTYPE)
  )%>%
  dplyr::filter(w_WEIGHT > 0)
        

# SUMMARY MEASURES ------------------------------------------------

# Weighted mean
filtered_data%>%
  group_by(REAL_GENOTYPE)%>%
  summarise_at(.vars = vars(DRY_LS, DRY_RS, FRESH_LS, FRESH_RS),
               .funs = list(mean = ~wtd.mean(., weights = w_WEIGHT),
                            var = ~wtd.var(., weights = w_WEIGHT, 
                                           normwt = TRUE)))%>%
  gather(key = "key", value = "value", -REAL_GENOTYPE) -> test
               