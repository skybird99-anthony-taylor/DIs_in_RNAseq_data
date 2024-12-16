# Read the text file
text <- readLines("E:\\DI_bioinformatics_data\\Results\\TuMV\\TuMV_virema\\combined_deletions.txt")

# Replace semicolons with newlines
modified_text <- gsub(";", "\n", text)

# Write back to a new text file
writeLines(modified_text, "E:\\DI_bioinformatics_data\\Results\\TuMV\\TuMV_virema\\combined_deletions2.txt")

# Example input vector
input <- readLines("E:\\DI_bioinformatics_data\\Results\\TuMV\\TuMV_virema\\combined_deletions2.txt")

# Use gsub to remove the last "_NUMBER" part
output <- gsub("_\\d+$", "", input)

# View the result
print(output)

# Write output
writeLines(output, "E:\\DI_bioinformatics_data\\Results\\TuMV\\TuMV_virema\\combined_deletions3.txt")

# Then, manually change the _to_ to a comma and "Junctions" to BP_Pos and RI_Pos
