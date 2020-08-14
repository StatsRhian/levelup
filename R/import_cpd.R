#' import_cpd
#' @importFrom dplyr mutate select
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @param path Path to CPD records
#' @return a [tibble][tibble::tibble-package]
#' @export
import_cpd = function(path =  "cpd-records"){
  filename = list.files(path = path, pattern = "*.[yaml|yml]$", recursive = TRUE)
  df = tibble(filename = filename)

  df =
    df %>%
    mutate(data = purrr::map(.x = filename, ~yaml_to_tibble(file = glue::glue("{path}/{.x}")))) %>%
    select(-filename) %>%
    unnest(cols = c(.data$data))
  return(df)
}


