# Load necessary libraries
library(readr)
library(dplyr)
library(writexl)
library(tidyr)  # For using separate()

#Import the desired file directly to R
junctions <- combined_deletions %>% select(A_C.A_C.)
junctions$ID <- gsub("\\(\\d+_\\d+\\)", "", junctions$A_C.A_C.)

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
  filter(Count >= 1000) %>%  # Click on the df to see what the max number is, and set that
  distinct(ID, .keep_all = TRUE) 

# Histogram + dplyr filters for help manually binning. The goal is to whittle down to the biggest bin.
# If all the BP is the vame value but the RI is different, this is useful. Same if it's vice-versa.
# Otherwise you have to fiddle with it.
ggplot(junctions_filtered2, aes(x = RI)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  stat_bin(
    binwidth = 10, 
    aes(label = after_stat(count)), 
    geom = "text", 
    vjust = -0.5, 
    color = "red"
  ) +
  labs(title = "Histogram with Bin Labels", x = "RI", y = "Frequency")

junctions_filtered2 <- junctions_filtered2 %>%
  filter(RI < 3600)

# A save spot
junctions_filtered2 <- junctions_filtered
