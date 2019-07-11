# DESIGN: INTRODUCTION ----
#' Creates the design table from the JMP file.
#' Adds the QR code to the design.
#' Outputs the design layout plot and repartition plots.
#' Updates the design table with the germination and border runs.
#' Inputs:
#' - JMP design table (.xlsx)
#' - Border runs and NA runs (xlsx)

# USER DESIGNED FUNCTIONS -----------------------------------------------------
repeat_design <- function(mat){
  #' Repeats the design table 3 times and increases the number of strips by 
  #' 33 for each repetition to have STRIP:1->99
  mat2 <- mat%>%
    mutate(STRIP = STRIP+33)
  mat3 <- mat%>%
    mutate(STRIP = STRIP+66)
  mat <- rbind(mat,mat2,mat3)
  return(mat)
}

QR_CODE <- function(mat){
  #' Creates the QR code for a table with TANK,STRIP and POSITION
  mat <- mat%>%mutate(QR = paste(TANK,STRIP,POSITION,sep = "_"))
  return(mat)
}

# JMP DESIGN -----------------------------------------------------------------

# Load the JMP design and reformat design labels (tanks, strips, position)
JMP_design <- read_excel("Inputs/30G_33S_10ST.xlsx")%>%
  select(-Y, -Tank, -Strip,
         TANK = `Whole plots`,
         STRIP = Subplots,
         POSITION = Position,
         GENOTYPE = Genotype)%>%
  mutate_if(is.character,list( ~ str_replace(.,"L","")))%>%
  mutate(TANK = ifelse(TANK == 1, "A", "B"), 
         #Moving: A, Still: B
         STRIP = ifelse(STRIP > 33, STRIP-33, STRIP),
         GENOTYPE = as.numeric(GENOTYPE))

# Repeat the design 3 times
design_table <- repeat_design(JMP_design)

# Add the QR code
design_table <- design_table%>%QR_CODE()

# GERMINATION UPDATE ---------------------------------------------------------

# Load the border/NA runs files, format and apply QR_CODE()
na_runs <- read_excel("Inputs/BORDER_NA.xlsx", sheet = 1)%>%
  select(-CODE)%>%
  QR_CODE()
border_runs <- read_excel("Inputs/BORDER_NA.xlsx", sheet = 2)%>%
  select(-CODE)%>%
  mutate(TANK = ifelse(TANK == "Moving", "A", "B"))%>%
  QR_CODE()

# Add another column for 'real runs' in the design table
design_table%>%
  anti_join(na_runs, by = "QR")%>%
  anti_join(border_runs, by = "QR")%>%
  rbind(na_runs,border_runs)%>%
  mutate(REAL_GENOTYPE = as.numeric(GENOTYPE))%>%
  select(REAL_GENOTYPE, QR)%>%
  full_join(design_table, by = 'QR') -> design_table

# Save it into .RData for other scripts
save(design_table, file = "data.RData")

# REPARTITION TABLE ----------------------------------------------------------
design_table%>%
  dplyr::filter(REAL_GENOTYPE != 31)%>%
  group_by(TANK,POSITION,REAL_GENOTYPE)%>%
  summarise(COUNT = n())%>%
  unite(temp,TANK,POSITION)%>%
  spread(temp,COUNT)%>%
  xtable(digits = 0)%>%
  print(include.rownames=FALSE, NA.string = "/")
  
# LAYOUT PLOT  -----------------------------------------------------------------------

# MOVING
# Plot the design as 33 tiles in tank
moving_plot <- JMP_design%>%
  dplyr::filter(TANK == 'A')%>%
  ggplot(aes(x = STRIP, y = POSITION, fill = factor(GENOTYPE)))+
  geom_tile(colour = "white")+
  scale_x_continuous(breaks = seq(1,33))+
  scale_fill_discrete(name = "GENOTYPE")+
  labs(title = 'MOVING TANK',
       subtitle = "2D schematic genotype repartition map")+
  geom_text(aes(label = GENOTYPE), size =2.5, angle = 90)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

# Save it as a pdf file
ggsave("Figures/design_layout_moving.pdf",
       plot = moving_plot, device = "pdf",
       width = 7.5, height = 3.78)

# STILL
# Plot the design as 33 tiles in tank
still_plot <- JMP_design%>%
  dplyr::filter(TANK == 'B')%>%
  ggplot(aes(x = STRIP, y = POSITION, fill = factor(GENOTYPE)))+
  geom_tile(colour = "white")+
  scale_x_continuous(breaks =  seq(1,33))+
  scale_fill_discrete(name = "GENOTYPE")+
  labs(title = 'STILL TANK',
       subtitle = "2D schematic genotype repartition map")+
  geom_text(aes(label = GENOTYPE), size =2.5, angle = 90)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

# Save it as a pdf file
ggsave("Figures/design_layout_still.pdf",
       plot = still_plot, device = "pdf",
       width = 7.5, height = 3.78)

# REPARTITION PLOT -----------------------------------------------------------

# Histogram for genotypes per TANK and POSITION
# Planned repartition and real repartition
tank_labels <- c(A = "MOVING", B = "STILL")

repartition_plot <- design_table%>%
  gather(GENO_ID, GENO_VALUE, - TANK, -STRIP, -POSITION, -QR)%>%
  ggplot(aes(GENO_VALUE, fill = GENO_ID))+
  geom_histogram(alpha = 0.7, color = "black", binwidth = 1, boundary = -0.5)+
  scale_x_continuous(breaks = c(1:31))+
  scale_fill_discrete(name = "",
                      labels = c('Planned repartition','Actual repartition'))+
  labs(y = "COUNT",
       x = "GENOTYPE")+
  coord_flip()+
  facet_grid(TANK~POSITION, labeller = labeller(TANK = tank_labels))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom")

ggsave("Figures/repartition_plot.pdf",
       plot = repartition_plot, device = "pdf",
       width = 11, height = 8.5)
