map_ancova <- function(data, vars) {
  # Map the aov() function to apply an ANCOVA using the formula method and a
  # a list of variables and a list of data frames
  #
  # Args:
  #   data: A list of data frames (or tibbles)
  #   vars: A list of character strings
  #
  # Returns:
  #   A list of data frames (or tibbles) with the tests statistics and the
  #   p value

  ancova <- purrr::map(
   vars,
   ~ paste0(.x, "_6m ~ ", .x, "_0m + group")
  ) %>% 
    purrr::map(as.formula) %>% 
    purrr::map(aov, data = data) %>% 
    purrr::map(summary)
  tibble::tibble(variables = vars) %>% 
    dplyr::mutate(
      p_cov = map_dbl(ancova, ~ .x[[1]][[5]][[1]]),
      p_group = map_dbl(ancova, ~ .x[[1]][[5]][[2]])
    )
}
