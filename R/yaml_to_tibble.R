#' yaml_to_tibble
#' @importFrom yaml read_yaml
#' @importFrom tibble as_tibble
#' @param file File to be converted
#' @return a [tibble][tibble::tibble-package]
yaml_to_tibble = function(file) {
  read_yaml(file) %>%
    as_tibble()
}
