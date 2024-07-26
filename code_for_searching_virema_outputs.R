# Load necessary libraries
library(stringr)
library(writexl)

# Define the junctions and the wiggle room. Example junctions are given.
junctions <- list(
  list(start=781, end=19664),
  list(start=20340, end=29902)
)
wiggle_room <- 5

# Function to create a regex pattern for each junction with wiggle room
create_pattern <- function(junction, wiggle_room) {
  start_range <- sprintf("%03d", (junction$start - wiggle_room):(junction$start + wiggle_room))
  end_range <- sprintf("%03d", (junction$end - wiggle_room):(junction$end + wiggle_room))
  start_pattern <- paste(start_range, collapse = "|")
  end_pattern <- paste(end_range, collapse = "|")
  pattern <- paste0("\\b(", start_pattern, ")_to_(", end_pattern, ")_#_\\d+\\b")
  return(pattern)
}

# Generate patterns for all junctions
patterns <- sapply(junctions, create_pattern, wiggle_room = wiggle_room)

# Print the generated patterns for debugging
cat("Generated Patterns:\n")
print(patterns)

# Function to search for patterns in a text file
search_junctions_in_file <- function(file_path, patterns) {
  lines <- readLines(file_path)
  results <- list()
  for (pattern in patterns) {
    matches <- str_extract_all(lines, pattern)
    matches <- unlist(matches)
    if (length(matches) > 0) {
      results <- c(results, matches)
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
all_results <- list()
for (file in text_files) {
  cat("Searching in file:", file, "\n")  # Debugging statement
  file_results <- search_junctions_in_file(file, patterns)
  if (length(file_results) > 0) {
    all_results[[file]] <- file_results
  }
}

# Print the actual junctions found
if (length(all_results) > 0) {
  for (file in names(all_results)) {
    cat("File:", file, "\n")
    cat("Junctions Found:\n")
    print(all_results[[file]])
    cat("\n")
  }
} else {
  cat("No junctions found in any files.\n")
}

# Prepare the results for writing to Excel
output <- data.frame(File = character(), Junctions = character(), stringsAsFactors = FALSE)
for (file in names(all_results)) {
  for (junction in all_results[[file]]) {
    output <- rbind(output, data.frame(File = file, Junctions = junction, stringsAsFactors = FALSE))
  }
}

# Write the results to an Excel file
output_file <- "junctions_results.xlsx"
write_xlsx(output, output_file)

cat("Results written to", output_file, "\n")
#Output file will be in base Documents folder