# Load necessary libraries
library(readr)
library(dplyr)
library(writexl)
library(tidyr)  # For using separate()

# Define the wiggle room
wiggle_room <- 5

# Function to bin a value within the wiggle room
bin_position <- function(value, wiggle_room) {
  return(round(value / wiggle_room))  # Bins the position based on the wiggle room
}

# Will need to MANUALLY import data
file <- read.delim2("E:\\DI_bioinformatics_data\\Results\\CMV\\vodka2\\CMV_VODKA2_DEL.txt")

#deal with funky (annoying) columns for EACH dataset
file$`A_C.A_C.` <- gsub("\\(.*\\)", "", file$`A_C.A_C.`)
file <- file %>% separate(`A_C.A_C.`, into = c("A", "C"), sep = "_", convert = TRUE, remove = FALSE)
file <- file %>%  mutate(
      A = as.numeric(A),
      C = as.numeric(C)
    ) %>%
    # Apply wiggle room to both 'A' and 'C'
    mutate(
      A_bin = bin_position(A, wiggle_room),
      C_bin = bin_position(C, wiggle_room)
    )
  
  # Group by the binned A and C values and count occurrences
  ac_count <- file %>%
    group_by(A_bin, C_bin) %>%
    summarize(count = n(), .groups = 'drop')  # Count occurrences of each unique A_C
  
# Optionally, write the results to an Excel file
output_file <- "E:\\DI_bioinformatics_data\\Results\\CMV\\vodka2\\most_common_ac_results.xlsx"
write_xlsx(ac_count, output_file)
