# SUMMARY_PLOT: INTRODUCTION ----
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
QRreformat <- function(vec) {
  #' Reformats the QRcode from the weighting data into standard QR format
  return(paste(toupper(str_sub(vec, 1, 1)),
               str_pad(str_sub(vec, 3, -3), 2, pad = "0"),
               str_sub(vec, -1),
               sep = "_")
  )
}

summ_plot <- function(var) {
  summ_plot <- function(varname){
    var <- enquo(varname)
    filtered_data%>%
      dplyr::filter(TANK == "A")%>%
      mutate(GENO = as.factor(REAL_GENOTYPE))%>%
      group_by(GENO)%>%
      summarise(meanA  = mean(eval(parse_expr(!!var))))%>%
      select(GENO,meanA) -> RM
    RM <- rbind(RM,RM)
    pd = position_dodge(width = 0.7)
    
    filtered_data%>%
      mutate(GENO = as.factor(REAL_GENOTYPE))%>%
      group_by(TANK,GENO)%>%
      summarise(mean = mean(eval(parse_expr(!!var)), na.rm = T),
                sd = sd(eval(parse_expr(!!var)), na.rm = T))%>%
      ggplot(aes(x = fct_reorder(RM$GENO,RM$meanA, .desc = T), 
                 y = mean, color = TANK))+
      geom_point(aes(fill = TANK),
                 color = "black",
                 shape = 22, size = 1.75,
                 position = pd)+
      geom_errorbar(aes(x = fct_reorder(RM$GENO,RM$meanA, .desc = T),
                        group = TANK,
                        ymin = mean -0.5*sd,
                        ymax = mean + 0.5*sd),
                    colour = "black", width = 0.1, position = pd,
                    inherit.aes = FALSE)+ 
      theme_bw()+
      theme(legend.position = "bottom") +
      labs(x = "GENOTYPE",
           y = labels[[varname]])
}

# Load data table and design table
load("data.RData")

# OUTLIERS --------------------------------------------------------------------

# Load the outliers file
outliers <- read_xlsx("Inputs/outliers.xlsx", sheet = 'table', na = "0") %>%
  mutate(QR = QRreformat(CODE)) %>%
  select(-COMMENT, - CODE,
         NOT_FG = NOT_FULL_GROWTH) %>%
  as.data.frame()

# Replace NA's by zeroes
outliers[is.na(outliers)] <- 0


# Load the weight table
weights_table <- read_xlsx('Inputs/outliers.xlsx', sheet = 'weights')

# Load the seeds initial weights
seed_weights <- read_xlsx('Inputs/GENO_INFO.xlsx', sheet = 'WEIGHT')
seed_weights <- seed_weights %>%
  mutate(WEIGHT = rowMeans(seed_weights) / 10) %>%
  select(REAL_GENOTYPE = GENOTYPE,
         S_WEIGHT = WEIGHT)

# WEIGHTS ---------------------------------------------------------------------

# Form temp dataset with all conditions and added final AREA and 
#   WEIGHT weights
filtered_data <- data_table %>%
  left_join(seed_weights, by = "REAL_GENOTYPE") %>%
  left_join(outliers, by = "QR") %>%
  mutate(w_WEIGHT = ifelse(NO_SEED == 1, 0,
                         ifelse(NO_RS == 1, 1,
                                ifelse(NO_LS == 1, 2,
                                       ifelse(BAD_LS == 1, 3,
                                              ifelse(NOT_FG == 1, 4, 5)
                                              )
                                       )
                                )
                         ),
         w_AREA = ifelse(OVERLAP == 1, 0, 1)
         ) %>%
  select(-c(NO_RS, NO_LS, BAD_LS, NOT_FG, NO_SEED, OK, OVERLAP))

# DATA FILTERING -------------------------------------------------

# Fix the wrong NA genotypes and filter:
# - outliers (null weights)
# - null fresh/dry RS weights

filtered_data <- filtered_data %>%
  mutate(REAL_GENOTYPE = ifelse(is.na(REAL_GENOTYPE) & w_WEIGHT > 0,
                                GENOTYPE, REAL_GENOTYPE)
  )

# Save a side table for updated germination rates
UPD_germ_rate <- filtered_data%>%
  mutate(KEPT = ifelse(w_WEIGHT > 0 &
                       !is.na(DRY_RS) &
                       DRY_RS > 0 & FRESH_LS > 0 &
                       REAL_GENOTYPE != 31, 1, 0))%>%
  group_by(REAL_GENOTYPE)%>%
  summarise(N_seeds_placed = n(),
    N_seeds_used = sum(KEPT),
    UPD_rate = round(sum(KEPT)/n()*100,1))%>%
  arrange(desc(UPD_rate))

# Output it to xtable and save it as txt file

print(xtable(UPD_germ_rate, digits =c(0,0,0,0,1),
             caption = "Effective on-platform germination rates (GR) and effective number of
             seeds kept for data analysis (NS) for each genotype"),
      file = "Tables/updated_germ_table.txt",
      append = FALSE,
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity,
      booktabs = TRUE,
      timestamp = NULL,
      comment = FALSE)

filtered_data <- filtered_data%>%
  dplyr::filter(w_WEIGHT > 0,
                !is.na(DRY_RS),
                DRY_RS > 0 & FRESH_LS > 0,
                REAL_GENOTYPE != 31)


# Save the filtered data
save(filtered_data,design_table,data_table,
     file = "data.RData")

# SUMMARY TABLES ------------------------------------------------

# Print the latex table for the seed weights
print(xtable(seed_weights, digits = c(0,0,2),
             caption = "Mean individual seed weight for each genotype"),
      file = "Tables/latex_seed_Weight_table.txt",
      append = FALSE,
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity,
      booktabs = TRUE,
      timestamp = NULL,
      comment = FALSE)


# Weighted mean and SD
summary_table <- filtered_data %>%
  group_by(REAL_GENOTYPE, TANK) %>%
  summarise_at(.vars = vars(DRY_LS, DRY_RS, FRESH_LS, FRESH_RS),
               .funs = list(mean = ~wtd.mean(., weights = w_WEIGHT),
                            SD = ~sqrt(wtd.var(., weights = w_WEIGHT,
                                           normwt = TRUE, na.rm = TRUE))))

# Create the latex table with xtable()
summary_latex_table <- filtered_data %>%
  group_by(REAL_GENOTYPE) %>%
  summarise_at(.vars = vars(DRY_LS, DRY_RS, FRESH_LS, FRESH_RS),
               .funs = list(mean = ~wtd.mean(., weights = w_WEIGHT),
                            SD = ~sqrt(wtd.var(., weights = w_WEIGHT,
                                               normwt = TRUE, na.rm = TRUE)))) %>%
  mutate_if(is.numeric, round, 4) %>%
  unite(col = "FRESH_LS", starts_with("FRESH_LS"), sep = " $\\pm$ ") %>%
  unite(col = "FRESH_RS", starts_with("FRESH_RS"), sep = " $\\pm$ ") %>%
  unite(col = "DRY_LS", starts_with("DRY_LS"), sep = " $\\pm$ ") %>%
  unite(col = "DRY_RS", starts_with("DRY_RS"), sep = " $\\pm$ ")

print(xtable(summary_latex_table, digits = rep(c(0, 4), times = c(2, 4)),
             caption = "Weighted mean and standard deviation for the four
             main variables"),
      file = "Tables/latex _summary_table.txt",
      append = FALSE,
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity,
      booktabs = TRUE,
      timestamp = NULL,
      comment = FALSE)

#Save the filtered data set to .RData
save(design_table,data_table,filtered_data,
     file = "data.RData")

# SUMMARY PLOTS --------------------------------------------------

# Extract the names of the vars I want to plot, the labels of the axis
vars <- names(filtered_data[7:10])
labels <- list(FRESH_LS = "FRESH LEAF SYSTEM WEIGHT (g)",
             FRESH_RS = "FRESH ROOT SYSTEM WEIGHT (g)",
             DRY_LS = "DRY LEAF SYSTEM WEIGHT (g)",
             DRY_RS = "DRY ROOT SYSTEM WEIGHT (g)")

# Stock the plots in a object
summary_plots <- map(vars, ~ summ_plot(.x))

# Print them all to pdf 
plotnames <- map(vars, ~ paste0("Figures/", .x, "_summary_plot.pdf"))
walk2(plotnames, summary_plots,
      ~ ggsave(filename = .x, plot = .y, height = 3.78, width = 7.5))


