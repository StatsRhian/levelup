#' export benefit report
#' @importFrom lubridate year today
#' @importFrom dplyr filter mutate select rename
#' @importFrom readr write_csv
#' @param path Location for saving the report
#' @param records_path Path to the cpd-records
#' @param report_year The year for the benefit report
#' @export
export_benefit_report = function(path = ".", records_path = "cpd-records", report_year = year(today())){
  df = import_cpd(path = records_path)
  df =
    df %>%
    mutate(activity_year = year(start_date)) %>%
    filter(activity_year == report_year) %>%
    mutate(learning_hours = as.numeric(learning_hours)) %>%
    select(title, learning_hours, activity_description, benefit_to_practice, benefit_to_users) %>%
    rename(`Activity Title` = title,
           `Learning Hours` = learning_hours,
           `Activity Description` = activity_description,
           `Benefit to Practice` = benefit_to_practice,
           `Benefit to Users` = benefit_to_users)
  new_path =  glue("{path}/benefit-report-{report_year}.csv")
  write_csv(df, file = new_path)
  cli::cli_alert_success(glue("Successfully saved benefit report to {new_path}"))
}
