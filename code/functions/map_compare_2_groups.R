map_compare_2_groups <- function(data, vars, group, paired = FALSE) {
  # Map the t.test() and wilcox.test() functions using the formula method and a
  # a list of variables, a list of data frames and the grouping variable
  #
  # Args:
  #   data: A list of data frames (or tibbles)
  #   vars: A list of character strings
  #   group: A character string with the name of the grouping variable
  #   paired: A boolean indicating whether or not to assume paired samples.
  #
  # Returns:
  #   A list of data frames (or tibbles) with the tests statistics and the
  #   p value

  para <- purrr::map(vars, ~ paste0(.x, " ~ ", group)) %>% 
    purrr::map(as.formula) %>% 
    purrr::map(t.test, data = data, paired = paired)
  npara <- purrr::map(vars, ~ paste0(.x, " ~ ", group)) %>% 
    purrr::map(as.formula) %>% 
    purrr::map(wilcox.test, data = data, paired = paired)
  tibble::tibble(variables = vars) %>% 
    dplyr::mutate(
      t_value = purrr::map_dbl(para, "statistic"),
      p_value_t = purrr::map_dbl(para, "p.value"),
      w_value = purrr::map_dbl(npara, "statistic"),
      p_value_w = purrr::map_dbl(npara, "p.value")
    )
}
