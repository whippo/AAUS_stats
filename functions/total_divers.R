# Function to flag errors in Total Diver entries
total_divers <- function(df) {
  # Select rows where `Total Divers` equals 1
  selected_rows <- df %>% filter(`Total Divers` > 200)
  
  # Print the `OM` values and row numbers from the selected rows
  if (nrow(selected_rows) > 0) {
    for (i in 1:nrow(selected_rows)) {
      row_number <- which(df$`Total Divers` > 200)[i]
      print(paste("Row", row_number, ":", selected_rows$OM[i]))
    }
  } else {
    print("No rows with Total Divers greater than 200 found.")
  }
}