# Load necessary libraries
library(readr)
library(dplyr)
library(writexl)
library(tidyr)
library(tidyverse)

# Import combined file, transformed with the simicolon_to_linebreak.R file

# Create new df using the two relevant columns from the combined file
junctions <- combined_deletions3 %>% select(BP_Pos,RI_Pos)
junctions$ID <- paste(junctions$BP_Pos, junctions$RI_Pos, sep = "_")

# Count occurrences of each Coordinate ID and append as a new column, arrange in descending order
junctions <- junctions %>%
  group_by(ID) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count))

# Keep only one instance of each coordinate ID
junctions_filtered <- junctions %>%
  distinct(ID, .keep_all = TRUE)  # Keep one instance of each Coordinate_ID

# Click on df and remove all counts smaller than the largest value
junctions_filtered <- junctions_filtered %>%
  filter(Count >= 3) %>%  # Click on the df to see what the max number is, and set that
  distinct(ID, .keep_all = TRUE) 

# Histogram + dplyr filters for help manually binning. The goal is to whittle down to the biggest bin.
# If all the BP is the vame value but the RI is different, this is useful. Same if it's vice-versa.
# Otherwise you have to fiddle with it.
ggplot(junctions_filtered, aes(x = BP_Pos)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  stat_bin(
    binwidth = 10, 
    aes(label = after_stat(count)), 
    geom = "text", 
    vjust = -0.5, 
    color = "red"
  ) +
  labs(title = "Histogram with Bin Labels", x = "RI", y = "Frequency")

junctions_filtered <- junctions_filtered %>%
  filter(RI_Pos > 2985) # Example RI is 2,985
