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

# Function to bin the data and count occurrences of each species
bin_and_count_species <- function(df, wiggle_room) {
  df <- df %>%
    mutate(
      BP_bin = bin_position(BP, wiggle_room),
      RI_bin = bin_position(RI, wiggle_room)
    ) %>%
    group_by(BP_bin, RI_bin) %>%
    summarize(count = n(), .groups = 'drop')  # Count occurrences of each unique species
  
  return(df)
}

# Specify the directory containing the tab-separated text files
directory <- "E:\\DI_bioinformatics_data\\Results\\CMV\\CMV_DVGfinder_results"

# Get a list of all text files in the directory
text_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

# Print the list of files for debugging
cat("Text Files Found:\n")
print(text_files)

# Initialize an empty dataframe with explicit column types
all_species_counts <- data.frame(
  File = character(),
  BP_bin = integer(),
  RI_bin = integer(),
  count = integer(),
  stringsAsFactors = FALSE
)

# Search each file for the species and collect the results
for (file in text_files) {
  cat("Processing file:", file, "\n")  # Debugging statement
  
  # Use read_delim to read tab-separated text files
  df <- read_delim(file, delim = "\t", col_types = cols(BP = col_double(), RI = col_double()))  
  
  # Check if the dataframe has the necessary columns
  if (!all(c("BP", "RI") %in% colnames(df))) {
    cat("File", file, "does not contain required columns 'BP' and 'RI'. Skipping...\n")
    next
  }
  
  # Bin and count species in the current file
  species_counts <- bin_and_count_species(df, wiggle_room)
  
  # If there are results, append them to all_species_counts, ensuring column names match
  if (nrow(species_counts) > 0) {
    species_counts <- mutate(species_counts, File = file)  # Add file column
    all_species_counts <- bind_rows(all_species_counts, species_counts)  # Safely combine dataframes
  } else {
    cat("No species found in file:", file, "\n")
  }
}

# Sort the species by count (most common first)
most_common_species <- all_species_counts %>%
  arrange(desc(count))

# Print the most common species
cat("Most common species:\n")
print(most_common_species)

# Optionally, write the sorted results to an Excel file
output_file <- "sorted_species_results.xlsx"
write_xlsx(most_common_species, output_file)

# Print the working directory and output file path
cat("Results written to", file.path(getwd(), output_file), "\n")
