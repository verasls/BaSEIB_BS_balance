percent_change <- function(data, baseline, followup, var_name) {
  # Returns the row-wise percent change between two columns in a data frame.
  #
  # Args:
  #   data: The data frame or tibble with the data to be computed.
  #   baseline,followup: The name of the columns to be
  #     used to compute the percent change.
  #   var_name: The name of the output variable.
  data$percent_change <- ((data[[followup]] - data[[baseline]]) / data[[baseline]]) * 100
  names(data)[names(data) == "percent_change"] <- var_name
  data
}
