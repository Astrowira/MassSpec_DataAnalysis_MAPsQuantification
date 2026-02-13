# MAPs Analysis (Shared peptides between treatments)
# Katherinne Herrera-Jordan
# February 12 2026

#If you don't have the following Packages, download them takin the "#" out.
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("VennDiagram")

library(readxl)
library(dplyr)
library(ggplot2)
library(VennDiagram)
library(grid)

#Stablish the path you will be working on
path <- "C:/Users/Your/Path/"


# Open your data
# The data you need for this is only the "Peptide sequence" column, but use the filtered information
# By filtered information, I mean the information that contains the peptides that appear in at least 2 of the 3 replicates.
# To get this data, you can either use your own excels or download them from the python program

#For this example, I'm comparing 3 different techniques (KF, MM, SEP) worked in triplicates
#I also repeated this experiment twice. So I have 2 different datasets with the same treatments. 
#This helps me have reproducibility and robustness for my data. 
#But you can also do this analysis/ diagram if you only did it once. 

KF_1  <- read_excel(paste0(path, "MHCI_KF_1.xlsx"))
KF_2  <- read_excel(paste0(path, "MHCI_KF_2.xlsx"))

MM_1  <- read_excel(paste0(path, "MHCI_MagneticManual_1.xlsx"))
MM_2  <- read_excel(paste0(path, "MHCI_MagneticManual_2.xlsx"))

SEP_1 <- read_excel(paste0(path, "MHCI_SepharoseManuall_1.xlsx"))
SEP_2 <- read_excel(paste0(path, "MHCI_SepharoseManuall_2.xlsx"))

#unir
KF_peptides <- union(
  unique(KF_1$Peptide_sequence),
  unique(KF_2$Peptide_sequence)
)

MM_peptides <- union(
  unique(MM_1$Peptide_sequence),
  unique(MM_2$Peptide_sequence)
)

SEP_peptides <- union(
  unique(SEP_1$Peptide_sequence),
  unique(SEP_2$Peptide_sequence)
)


#contar
counts <- data.frame(
  Method = c(
    "Magnetic beads KingFisher",
    "Magnetic beads Manual",
    "Sepharose beads manual"
  ),
  MAPs = c(
    length(KF_peptides),
    length(MM_peptides),
    length(SEP_peptides)
  )
)

print(counts)

#Histograma
ggplot(counts, aes(x = Method, y = MAPs, fill = Method)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c(
    "Magnetic beads KingFisher" = "#6331ff",
    "Magnetic beads Manual"    = "#9e27ff",
    "Sepharose beads manual"   = "#b59bf3"
  )) +
  labs(
    x = "Method used",
    y = "MAPs #",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

#Venn
venn.plot <- venn.diagram(
  x = list(
    "Magnetic beads KingFisher" = KF_peptides,
    "Magnetic beads Manual"    = MM_peptides,
    "Sepharose beads manual"   = SEP_peptides
  ),
  filename = NULL,
  fill = c("#6331ff", "#9e27ff", "#b59bf3"),
  alpha = 0.6,
  cex = 1.2,
  cat.cex = 1.1,
  cat.fontface = "bold",
  margin = 0.1
)

grid.newpage()
grid.draw(venn.plot)



###Reproducibility
#This is if you want to do a jaccard analysis

# KingFisher
KF_d1 <- unique(KF_1$Peptide_sequence)
KF_d2 <- unique(KF_2$Peptide_sequence)

# Magnetic Manual
MM_d1 <- unique(MM_1$Peptide_sequence)
MM_d2 <- unique(MM_2$Peptide_sequence)

# Sepharose Manual
SEP_d1 <- unique(SEP_1$Peptide_sequence)
SEP_d2 <- unique(SEP_2$Peptide_sequence)

compare_days <- function(day1, day2, method_name) {
  
  shared <- intersect(day1, day2)
  union_set <- union(day1, day2)
  
  data.frame(
    Method = method_name,
    Day1 = length(day1),
    Day2 = length(day2),
    Shared = length(shared),
    Unique_Day1 = length(setdiff(day1, day2)),
    Unique_Day2 = length(setdiff(day2, day1)),
    Jaccard = length(shared) / length(union_set)
  )
}

repro_summary <- bind_rows(
  compare_days(KF_d1, KF_d2, "Magnetic beads KingFisher"),
  compare_days(MM_d1, MM_d2, "Magnetic beads Manual"),
  compare_days(SEP_d1, SEP_d2, "Sepharose beads manual")
)

print(repro_summary)

