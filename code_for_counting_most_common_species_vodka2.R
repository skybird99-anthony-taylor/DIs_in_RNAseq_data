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

# Function to process the species column, split it, and apply wiggle room
process_species_column <- function(df, wiggle_room) {
  df_processed <- df %>%
    # Split the 'species' column into 'BP' and 'RI'
    separate(SPECIES, into = c("BP", "RI"), sep = "_", convert = TRUE) %>%
    # Apply wiggle room to both 'BP' and 'RI'
    mutate(
      BP_bin = bin_position(BP, wiggle_room),
      RI_bin = bin_position(RI, wiggle_room)
    )
  
  # Group by the binned species and count occurrences
  species_count <- df_processed %>%
    group_by(BP_bin, RI_bin) %>%
    summarize(count = n(), .groups = 'drop')  # Count occurrences of each unique species
  
  return(species_count)
}

# Specify the directory containing the text files
directory <- "E:\\DI_bioinformatics_data\\Results\\TuMV\\VODKA2"

# Get a list of all text files in the directory
text_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

# Print the list of files for debugging
cat("Text Files Found:\n")
print(text_files)

# Initialize an empty dataframe to store results
all_species_counts <- data.frame(File = character(), BP_bin = integer(), RI_bin = integer(), count = integer(), stringsAsFactors = FALSE)

# Process each file and collect the species counts
for (file in text_files) {
  cat("Processing file:", file, "\n")  # Debugging statement
  df <- read_delim(file, delim = "\t", col_types = cols(SPECIES = col_character()))  # Ensure 'species' is treated as character
  
  # Check if the dataframe has the necessary column
  if (!"SPECIES" %in% colnames(df)) {
    cat("File", file, "does not contain required column 'SPECIES'. Skipping...\n")
    next
  }
  
  # Process the species column to bin and count occurrences
  species_counts <- process_species_column(df, wiggle_room)
  
  # If there are results, append them to all_species_counts
  if (nrow(species_counts) > 0) {
    species_counts <- mutate(species_counts, File = file)  # Add file column
    all_species_counts <- bind_rows(all_species_counts, species_counts)  # Safely combine dataframes
  } else {
    cat("No species found in file:", file, "\n")
  }
}

# Now group all_species_counts by BP_bin and RI_bin and sum the counts
total_species_counts <- all_species_counts %>%
  group_by(BP_bin, RI_bin) %>%
  summarize(total_count = sum(count), .groups = 'drop')

# Sort the results by the total count (most common species first)
most_common_species <- total_species_counts %>%
  arrange(desc(total_count))

# Print the most common species
cat("Most common species:\n")
print(most_common_species)

# Optionally, write the results to an Excel file
output_file <- "most_common_species_results.xlsx"
write_xlsx(most_common_species, output_file)

# Print the working directory and output file path
cat("Results written to", file.path(getwd(), output_file), "\n")
