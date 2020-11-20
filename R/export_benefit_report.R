#' export benefit report
#' @importFrom lubridate year today
#' @importFrom dplyr filter mutate select rename
#' @importFrom readr write_csv
#' @importFrom cli cli_alert_success
#' @param path Location for saving the report
#' @param records_path Path to the cpd-records
#' @param report_year The year for the benefit report
#' @export
export_benefit_report <- function(path = ".", records_path = "cpd-records",
                                  report_year = year(today())){
  df <- import_cpd(path = records_path)
  df <-
    df %>%
    mutate(activity_year = year(.data$start_date)) %>%
    filter(.data$activity_year == .data$report_year) %>%
    mutate(learning_hours = as.numeric(.data$learning_hours)) %>%
    select(.data$title, .data$learning_hours, .data$activity_description,
           .data$benefit_to_practice, .data$benefit_to_users) %>%
    rename(`Activity Title` = .data$title,
           `Learning Hours` = .data$learning_hours,
           `Activity Description` = .data$activity_description,
           `Benefit to Practice` = .data$benefit_to_practice,
           `Benefit to Users` = .data$benefit_to_users)
  new_path <-  glue("{path}/benefit-report-{report_year}.csv")
  write_csv(df, file = new_path)
  cli_alert_success(glue("Successfully saved benefit report to {new_path}"))
}
