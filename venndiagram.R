# Install and load the VennDiagram package
library(VennDiagram)
library(readxl)
library(stringr)
library(ggVennDiagram)
library(plotly)

# Import data
combined <- read_excel("E:\\DI_bioinformatics_data\\Results\\BMV\\Combined_for_Venn_diagram\\combined.xlsx", 
                       col_types = c("text", "text", "text"))
combined$ID <- str_c(combined$BP_Pos, combined$RI_Pos, sep = "_")


# Example data sets
ditector <- subset(combined, program == "ditector")
virema <- subset(combined, program == "virema")
dvgfinder <- subset(combined, program == "dvgfinder")
vodka2 <- subset(combined, program == "vodka2")

sets <- list(
  virema = virema$ID,
  vodka2 = vodka2$ID,
  dvgfinder = dvgfinder$ID,
  ditector = ditector$ID)

# Create the Venn diagram
ggVennDiagram(sets, label_alpha = 0,
  label_color = "white")

# Find common junctions across all sets
common_junctions <- Reduce(intersect, sets)

# Print the result
print(common_junctions)
