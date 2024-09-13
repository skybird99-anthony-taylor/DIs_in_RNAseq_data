# Load necessary libraries
library(readr)
library(dplyr)
library(writexl)

# Define the wiggle room
wiggle_room <- 5

# Function to bin a value within the wiggle room
bin_position <- function(value, wiggle_room) {
  return(round(value / wiggle_room))  # Bins the position based on the wiggle room
}

# Function to bin the data and count unique species with frequencies
bin_and_count_unique_species <- function(df, wiggle_room) {
  df <- df %>%
    mutate(
      BP_Pos_bin = bin_position(BP_Pos, wiggle_room),
      RI_Pos_bin = bin_position(RI_Pos, wiggle_room)
    )
  
  species_counts <- df %>%
    group_by(BP_Pos_bin, RI_Pos_bin) %>%
    tally()  # Count occurrences of each unique species
  
  return(species_counts)
}

# Specify the directory containing the text files
directory <- "E:\\DI_bioinformatics_data\\Results\\BMV\\DItector_outputs\\txt_files"

# Get a list of all text files in the directory
text_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

# Print the list of files for debugging
cat("Text Files Found:\n")
print(text_files)

# Search each file for the unique species and collect the results with counts
all_species_counts <- data.frame(File = character(), BP_Pos_bin = integer(), RI_Pos_bin = integer(), n = integer(), stringsAsFactors = FALSE)
for (file in text_files) {
  cat("Processing file:", file, "\n")  # Debugging statement
  df <- read_delim(file, delim = "\t", col_types = cols(BP_Pos = col_double(), RI_Pos = col_double()))  # Ensure columns are numeric
  
  # Check if the dataframe has the necessary columns
  if (!all(c("BP_Pos", "RI_Pos") %in% colnames(df))) {
    cat("File", file, "does not contain required columns 'BP_Pos' and 'RI_Pos'. Skipping...\n")
    next
  }
  
  species_counts <- bin_and_count_unique_species(df, wiggle_room)
  
  if (nrow(species_counts) > 0) {
    species_counts <- mutate(species_counts, File = file)
    all_species_counts <- rbind(all_species_counts, species_counts)
  } else {
    cat("No unique species found in file:", file, "\n")
  }
}

# Optionally, write the sorted results to an Excel file
output_file <- "E:\\DI_bioinformatics_data\\Results\\BMV\\DItector_outputs\\species_counts_results_ditector.xlsx"
write_xlsx(species_counts, output_file)

# Print the working directory and output file path
cat("Results written to", file.path(getwd(), output_file), "\n")
