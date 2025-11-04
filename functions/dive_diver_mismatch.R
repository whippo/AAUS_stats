# Function to compare 'dives' and 'divers' columns and print the character string column value
dive_diver_mismatch <- function(df) {
  # Identify column pairs ending with " dives" and " divers"
  dives_cols <- grep(" Dives$", names(df), value = TRUE)
  divers_cols <- grep(" Divers$", names(df), value = TRUE)
  
  # Extract base names
  base_names <- gsub(" Dives$", "", dives_cols)
  
  # Column to print values from, assuming it's named 'OM'
  char_col <- "OM"  # Change this to your actual column name
  
  # Initialize a list to store pairs that meet the condition
  valid_pairs <- list()
  
  # Iterate through each base name
  for (base in base_names) {
    dives_col <- paste0(base, " Dives")
    divers_col <- paste0(base, " Divers")
    
    # Check if both columns exist in the dataframe
    if (dives_col %in% names(df) && divers_col %in% names(df)) {
      # Compare values in 'dives' and 'divers' columns
      condition_met <- df[[dives_col]] < df[[divers_col]]
      
      # Print character string column value if condition is met
      for (i in which(condition_met)) {
        print(paste("Row", i + 1, "Pair", base, ":", df[[char_col]][i]))
      }
      
      # Store pairs that meet the condition
      if (any(condition_met)) {
        valid_pairs <- c(valid_pairs, base)
      }
    }
  }
  
  # Print valid pairs
  if (length(valid_pairs) > 0) {
    cat("Pairs that meet the condition:", paste(valid_pairs, collapse = ", "), "\n")
  } else {
    cat("No pairs meet the condition.\n")
  }
}