median_absolute_distance <- function(x, window_size = 21) {
  n <- length(x)
  y <- vector("numeric", n)
  y[] <- NA  # Initialize with NAs
  
  for (i in 1:(n - window_size + 1)) {
    y[i + floor(window_size / 2)] <- 
      median(abs(median(x[i:(i + window_size - 1)])-x[i:(i + window_size - 1)]), na.rm = TRUE)
  }
  
  return(y)
}