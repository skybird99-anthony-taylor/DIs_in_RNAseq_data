# Load necessary libraries
library(readr)
library(dplyr)
library(writexl)

# Define the junctions and the wiggle room. Example junctions are given.
junctions <- list(
  list(start=781, end=19664),
  list(start=20340, end=29902)
)
wiggle_room <- 5

# Function to check if a value is within the wiggle room
within_wiggle_room <- function(value, target, wiggle_room) {
  return(abs(value - target) <= wiggle_room)
}

# Function to search for junctions in a data frame
search_junctions_in_df <- function(df, junctions, wiggle_room) {
  results <- data.frame(BP_Pos = integer(), RI_Pos = integer(), stringsAsFactors = FALSE)
  for (junction in junctions) {
    start_matches <- df %>%
      filter(within_wiggle_room(BP_Pos, junction$start, wiggle_room))
    end_matches <- df %>%
      filter(within_wiggle_room(RI_Pos, junction$end, wiggle_room))
    
    matches <- inner_join(start_matches, end_matches, by = c("BP_Pos", "RI_Pos"))
    if (nrow(matches) > 0) {
      results <- rbind(results, matches)
    }
  }
  return(unique(results))
}

# Specify the directory containing the text files
directory <- "Path\\to\\directory\\"

# Get a list of all text files in the directory
text_files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

# Print the list of files for debugging
cat("Text Files Found:\n")
print(text_files)

# Search each file for the junctions and collect the results
all_results <- data.frame(File = character(), BP_Pos = integer(), RI_Pos = integer(), stringsAsFactors = FALSE)
for (file in text_files) {
  cat("Searching in file:", file, "\n")  # Debugging statement
  df <- read_delim(file, delim = "\t", col_types = cols(BP_Pos = col_double(), RI_Pos = col_double()))  # Ensure columns are numeric
  
  # Check if the dataframe has the necessary columns
  if (!all(c("BP_Pos", "RI_Pos") %in% colnames(df))) {
    cat("File", file, "does not contain required columns 'BP_Pos' and 'RI_Pos'. Skipping...\n")
    next
  }
  
  file_results <- search_junctions_in_df(df, junctions, wiggle_room)
  
  if (nrow(file_results) > 0) {
    file_results <- mutate(file_results, File = file)
    all_results <- rbind(all_results, file_results)
  } else {
    cat("No matches found in file:", file, "\n")
  }
}

# Write the results to an Excel file
output_file <- "junctions_results.xlsx"
write_xlsx(all_results, output_file)

# Print the working directory and output file path
cat("Results written to", file.path(getwd(), output_file), "\n")
