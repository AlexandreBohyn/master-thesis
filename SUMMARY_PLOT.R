# METRICS: INTRODUCTION ----
#' Creates all the plots and tables, summarizing the metrics 
#' for each genotype, tank, etc...
#' Tables: mean and standard deviation for weight (D and F), AUPC, area
#' Plots: Boxplots of the repartition between genotypes 
#' and difference between tanks
#' 
#' Inputs:
#' - Data table
#' - Outliers table
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
  select(-COMMENT, -CODE)%>%
  as.data.frame()

# Replace NA's by zeroes
outliers[is.na(outliers)] <- 0


# Load the weight table
weights_table <- read_xlsx('Inputs/outliers.xlsx', sheet = 'weights')

# Load the seeds initial weights
seed_weights <- read_xlsx('Inputs/GENO_INFO.xlsx', sheet = 'WEIGHT')%>%
  mutate(WEIGHT = rowMeans(seed_weights)/10)%>%
  select(REAL_GENOTYPE = GENOTYPE, 
         S_WEIGHT = WEIGHT)

# WEIGHTS ---------------------------------------------------------------------

# Original seed weight condition
temp <- data_table%>%
  left_join(seed_weights, by = "REAL_GENOTYPE")%>%
  left_join(outliers, by = "QR")%>%
  mutate(WEIGHT = 0)

# Loop through the data table to set the weight value
for (i in 1:990){
  if (OK ==1){
    temp$WEIGHT = 1
  }
  
}

  