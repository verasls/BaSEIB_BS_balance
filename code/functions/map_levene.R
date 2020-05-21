map_levene <- function(data, vars, group) {
  # Map the car::leveneTest() function using a list of variables and a list of 
  # data frames
  #
  # Args:
  #   data: A list of data frames (or tibbles)
  #   vars: A list of character strings
  #   group: A character string with the name of the grouping variable
  #
  # Returns:
  #   A list of data frames (or tibbles) with the F ratio and
  #   p value

  tests <- purrr::map(vars, ~ data[[.x]]) %>% 
    purrr::map(~ car::leveneTest(.x, data[[group]]))
  tibble::tibble(variables = vars) %>% 
    dplyr::mutate(
      F_value = purrr::map(tests, "F value") %>% purrr::map_dbl(~ .x[[1]]),
      p_value = purrr::map(tests, "Pr(>F)") %>% purrr::map_dbl(~ .x[[1]])
    ) %>% 
    dplyr::mutate_if(is.numeric, round, 4)
}
