

# Function to identify N/A
find_nas <- function(data_nas, row_need = FALSE, col_need = FALSE) {
  # create empty vectors to store row and column numbers
  nas_row_number <- c()
  nas_col_number <- c()
  
  # iterate through each row and column in the data frame
  for (i in 1:nrow(data_nas)) {
    for (j in 1:ncol(data_nas)) {
      # check if the value at the current cell is NA or one of the specified string values
      if (is.na(data_nas[i, j]) == TRUE |
          data_nas[i, j] == "N/A" | data_nas[i, j] == "n/a") {
        # if yes, append the row and column number to their respective vectors
        nas_row_number <- append(nas_row_number, i)
        nas_col_number <- append(nas_col_number, j)
      }
    }
  }
  
  # if row_need is set to TRUE, return the vector of row numbers with NA values
  if (row_need == TRUE) {
    return(nas_row_number)
  }
  # if col_need is set to TRUE, return the vector of column numbers with NA values
  if (col_need == TRUE) {
    return(nas_col_number)
  }
}




remove_zeros <- function(data, start_col) {
  # Find indices of columns with all 0's
  zero_cols_indices <- which(colSums(data[, start_col:ncol(data)]) == 0)
  
  # Find indices of these columns in the original data frame
  zero_cols_names <- names(zero_cols_indices)
  zero_indices <- sapply(zero_cols_names, function(col_name)
    which(names(data) == col_name))
  
  # Create a new data frame without columns that contain only 0's
  data_without_zeros <- data[,-zero_indices]
  
  # Return the data frame without zeros and the indices of the removed columns
  return(data_without_zeros)
}


# Function to filter columns with non-zero cell count below a threshold
filter_nonzero_count_cols <- function(data, start_col, threshold) {
  
  # Initialize empty vector to store column indices that fail the filter
  filtered_cols <- c()
  
  # Loop through columns starting from a given column index
  for (i_col in c(start_col:ncol(data))) {
    
    # Extract the current column and count the number of non-zero cells
    this_col <- data[, i_col]
    nonzero_count <- length(which(this_col != 0))
    
    # If the number of non-zero cells is below the threshold, add the column index to filtered_cols
    if (nonzero_count <= threshold) {
      filtered_cols <- append(filtered_cols, i_col)
    }
  }
  
  # Remove filtered columns from the data and return the updated data
  return(data[, -filtered_cols])
}




sis_test <- function(data, start_col, response_var) {
  # Select the relevant columns of the data frame and convert to a matrix
  x <- as.matrix(data[, c(start_col:ncol(data))])
  # Calculate the sparse inverse covariance matrix (SIS)
  sis_result <- SIS(x = x, y = data[[response_var]])
  # Find the row indices of any missing values in the matrix
  row_nas <- find_nas(x, row_need = TRUE, col_need = FALSE)
  # Get the indices of the selected variables from the SIS result
  selected_indices <- c(start_col:ncol(data))[sis_result$sis.ix0]
  # Combine the selected variables with any variables that came before the start column
  candidate_indices <- append(c(1:(start_col - 1)), selected_indices)
  # Return the selected variables and their indices, as well as the row indices with missing data
  return(candidate_indices)
}
