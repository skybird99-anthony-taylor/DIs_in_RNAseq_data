# Load necessary libraries
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Function to process each file
process_file <- function(file_path) {
  # Read the file into a dataframe
  df <- read_delim(file_path, delim = "\t", col_names = FALSE)
  
  # Convert dataframe to a single character vector for pattern matching
  text_lines <- unlist(df)
  
  # Define the pattern to search for
  pattern <- "\\b(\\d+)_to_(\\d+)_#_\\d+\\b"
  
  # Extract all matches for the pattern
  matches <- str_extract_all(text_lines, pattern)
  
  # Flatten the list of matches into a single vector
  matches <- unlist(matches)
  
  # Create a dataframe with the hit and file columns
  hits_df <- data.frame(
    hit = matches,
    file = file_path,
    stringsAsFactors = FALSE
  )
  
  return(hits_df)
}

# Specify the directory containing the text files
directory <- "E:\\DI_bioinformatics_data\\Results\\BMV\\ViReMa"

# Get a list of all text files in the directory
text_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

# Process each file and combine the results
all_hits <- bind_rows(lapply(text_files, process_file))

# Remove the _#_NUMBER portion from the hits
all_hits$hit <- str_remove(all_hits$hit, "_#_\\d+$")

# Remove the _to_ portion to get just NUMBERNUMBER
all_hits$hit <- str_replace_all(all_hits$hit, "_to_", "_")

# Function to split 'hit' into 'BP' and 'RI' and convert to integers
process_hits <- function(all_hits) {
  all_hits <- all_hits %>%
    # Split 'hit' into 'BP' and 'RI' based on '_'
    separate(hit, into = c("BP", "RI"), sep = "_", convert = TRUE) %>%
    # Convert 'BP' and 'RI' from character to integer
    mutate(BP = as.integer(BP),
           RI = as.integer(RI))
  
  return(all_hits)
}

# Apply the function to the dataframe
all_hits_processed <- process_hits(all_hits)

# Function to find unique junctions with wiggle room and count occurrences
find_common_junctions <- function(df, wiggle_room = 5) {
  # Sort the dataframe by BP and RI
  df_sorted <- df %>%
    arrange(BP, RI)
  
  # Initialize a column to mark junction groups
  df_sorted <- df_sorted %>%
    mutate(junction_id = cumsum(
      (BP - lag(BP, default = first(BP)) > wiggle_room) |
        (RI - lag(RI, default = first(RI)) > wiggle_room)
    ))
  
  # Group by junction_id and count occurrences of each unique junction
  junctions_count <- df_sorted %>%
    group_by(BP, RI, junction_id) %>%
    summarize(count = n(), .groups = 'drop') %>%
    arrange(desc(count))  # Sort by the count to identify the most common species
  
  return(junctions_count)
}

# Apply the function to the dataframe
common_junctions <- find_common_junctions(all_hits_processed)

# Print the sorted junction counts (most common first)
print("Junctions sorted by occurrence:")
print(common_junctions)

# Find the most common species
most_common_species <- common_junctions %>%
  slice(1)  # Select the first row (the most common species)

# Print the most common species
cat("Most common species found:\n")
print(most_common_species)
