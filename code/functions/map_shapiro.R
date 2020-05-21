map_shapiro <- function(data, vars) {
  # Map the shapiro.test() function using a list of variables and a list of 
  # data frames
  #
  # Args:
  #   data: A list of data frames (or tibbles)
  #   vars: A list of character strings
  #
  # Returns:
  #   A list of data frames (or tibbles) with the shapiro.test statistic and
  #   p value

  tests <- purrr::map(vars, ~ data[[.x]]) %>% 
    purrr::map(shapiro.test)
  tibble(group = unique(data$group), time = unique(data$time), variables = vars) %>%
    mutate(
      shapiro_wilk = purrr::map_dbl(tests, "statistic"),
      p_value = purrr::map_dbl(tests, "p.value")
    )
}
