map_levene <- function(data, vars, group) {
  tests <- purrr::map(vars, ~ data[[.x]]) %>% 
    purrr::map(~ leveneTest(.x, data[[group]]))
  tibble::tibble(variables = vars) %>% 
    dplyr::mutate(
      F_value = purrr::map(tests, "F value") %>% purrr::map_dbl(~ .x[[1]]),
      p_value = purrr::map(tests, "Pr(>F)") %>% purrr::map_dbl(~ .x[[1]])
    ) %>% 
    dplyr::mutate_if(is.numeric, round, 4)
}
