clean_sample_size <- function(data, var, m1, m2) {
  # Cleans the variable value in one moment if the value in the other moment is
  # missing
  #
  # Args:
  #   data: a data frame in wide format, with each variable measured twice.
  #     Variables must be coded as name__XM. e.g., weight__0M and weight__12M
  #   var: a character string with the variable name, without the time
  #     idenfifier. e.g., "weight"
  #   m1, m2: a character string with the time identifier. e.g., "0M" and "12M"
  #
  # Returns:
  #   The original data frame with some of the variable values removed
  
  # Get variables names
  var_pre <- str_c(var, "_", m1)
  var_pos <- str_c(var, "_", m2)
  
  for (i in 1:nrow(data)) {
    if (is.na(data[i, var_pre])) {
      data[i, var_pos] <- NA
    }
    if (is.na(data[i, var_pos])) {
      data[i, var_pre] <- NA
    }
  }

  return(data)
}
