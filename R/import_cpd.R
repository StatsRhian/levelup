#' import_cpd
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @param path Path to CPD records
#' @return a [tibble][tibble::tibble-package]
#' @export
import_cpd = function(path =  "."){
  filename = list.files(path = path, pattern = "*.md", recursive = TRUE)
  df = tibble(filename = filename)

  df =
    df %>%
    mutate(data = map(.x = filename, ~yaml_to_tibble(file = glue("{path}/{.x}")))) %>%
    select(-filename) %>%
    unnest(cols = c(data))
  return(df)
}


