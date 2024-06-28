calculate_C1 <- function(x) {
  n <- length(x)
  k_start <- 6
  k_end <- 14
  
  # Initialize variables to store results
  C1_values <- numeric(k_end - k_start + 1)  # To store C1 values for each k
  min_index <- NA  # Initialize variable to store index of minimum C1 value
  
  for (k in k_start:k_end) {
    sum_abs_diff <- 0
    
    for (i in 1:n) {
      # Define the indices for the neighborhood
      start_index <- max(1, i - k)
      end_index <- min(n, i + k)
      neighborhood <- x[start_index:end_index]
      
      # Exclude the current element x[i]
      neighborhood <- neighborhood[neighborhood != x[i]]
      
      # Calculate the delete-one median
      if (length(neighborhood) > 0) {
        delete_one_median <- median(neighborhood)
      } else {
        delete_one_median <- NA
      }
      
      # Compute the absolute difference
      abs_diff <- abs(x[i] - delete_one_median)
      sum_abs_diff <- sum_abs_diff + abs_diff
    }
    
    # Calculate the final value of C1(k)
    C1_k <- sum_abs_diff / n
    C1_values[k - k_start + 1] <- C1_k
  }
  
  # Find the index of the minimum non-NA C1 value
  valid_indices <- which(!is.na(C1_values))
  if (length(valid_indices) > 0) {
    min_index <- valid_indices[which.min(C1_values[valid_indices])] + k_start - 1
  }
  
  return(min_index)
}