# METRICS: INTRODUCTION ----
#' Adds the dry and fresh weight to the design table.
#' Compute the root system area for each final root scan using:
#' image_analysis.py script.
#' Outputs the table with weight and area summary for genotype (by tank).
#' 
#' Inputs:
#' - Design_table
#' - Fresh weights
#' - Dry weights
#' - Image analysis python script (.csv output)

# USER DESIGNED FUNCTIONS -----------------------------------------------------
QRreformat <- function(vec){
  #' Reformats the QRcode from the weighting data into standard QR format
  #' Use sep = TRUE for the QR already using "_" as a separator
  return(paste(toupper(str_sub(vec,1,1)),
               str_pad(str_sub(vec,2,-2), 2, pad = "0"),
               str_sub(vec,-1),
         sep = "_")
         )
}

QRpad <- function(mat,vec){
  vec = enquo(vec)
  return(
    mat%>%
      separate(!!vec,c("a","b","c"),"_")%>%
      mutate(QR = paste(a, str_pad(b, 2, side = "left", pad = "0"),c,
             sep = "_"))%>%
      select(-a,-b,-c)
  )
}

# Load the design table
load("data.RData")

# FRESH WEIGHTS ---------------------------------------------------------------

# Load the fresh weights from xlsx file
fresh_weight <- read_xlsx("Inputs/weight.xlsx", sheet = 1)%>%
  select(CODE,
         FRESH_LS = LS_WEIGHT,
         FRESH_RS = RS_WEIGHT)%>%
  QRpad(CODE)

# Join them to design table to create data table
data_table <- design_table%>%
  full_join(fresh_weight, by = "QR")

# DRY WEIGHTS -----------------------------------------------------------------

# Load the 2nd part of dry weights from txt file
dry_weight_1 <- read_table("Inputs/data_dry_weights.txt",
                           col_types = cols(
                             Name = col_character(),
                             Shoot = col_double(),
                             Root = col_double()
                           ))%>%
  mutate(QR = QRreformat(Name))%>%
  select(QR,
         DRY_LS = Shoot,
         DRY_RS = Root)

# Load the 1st part of dry weights from xlsx file
dry_weight <- read_xlsx("Inputs/weight.xlsx", sheet = 2)%>%
  select(CODE,
         DRY_LS = LS,
         DRY_RS = RS)%>%
  QRpad(CODE)%>%
  anti_join(dry_weight_1, by = "QR")%>%
  rbind(dry_weight_1)

# Join them to the design table
data_table <- data_table%>%
  full_join(dry_weight, by = "QR")
  

# ROOT SYSTEM AREA ------------------------------------------------------------

# Load the results from the image analysis python script (final.csv)
area <- read_delim("Image analysis/final.csv", delim = "\t",
                   col_types = cols(
                     X1 = col_double(),
                     percentage = col_double(),
                     filename = col_character(),
                     camera = col_character(),
                     QRcode = col_character()
                   ))


# Merge them to data table
data_table <- data_table%>%
  full_join(select(area,
                   AREA = percentage,
                   QR = QRcode), by = "QR")

# DATA OUTPUT -----------------------------------------------------------------

# Save the design table and data table
save(design_table,data_table, file = "data.RData")


