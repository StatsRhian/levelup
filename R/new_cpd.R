#' import_cpd
#' @importFrom lubridate today ymd year
#' @importFrom tibble tibble
#' @importFrom yaml write_yaml as.yaml
#' @importFrom glue glue
#' @importFrom usethis edit_file
#' @param title Title for new CPD record
#' @param type Type of new CPD record, can be one of "work", "professional",
#' "formal", "self" or "other"
#' @param date The start date of the activity in yyyy-mm-dd format
#' @param learning_hours The number of learning hours acruded during this
#' activity
#' @param path Path to CPD records
#' @param overwrite Should the YAML be overwritten if it exists - default FALSE
#' @export
new_cpd <- function(title = "", type = "other", date = today(),
                   learning_hours = 0, path = "cpd-records",
                   overwrite = FALSE) {

   types <- tibble(
    short = c("work", "professional", "formal", "self", "other"),
    long = c("Work based learning", "Professional activity",
             "Formal / educational", "Self-directed learning", "Other")
  )

  if (!(type %in% types$short)) {
    stop("Unknown type")
  }

  start_date <- ymd(date)

  new <-
    tibble(title = title,
           activity_type = types$long[which(type == types$short)],
           start_date = as.character(start_date),
           end_date = as.character(start_date),
           activity_url = '',
           learning_hours = as.character(learning_hours),
           tags = '',
           activity_description = as.yaml("Review needed"),
           benefit_to_practice =  as.yaml("Review needed"),
           benefit_to_users =  as.yaml("Review needed")
    )
  file <- glue("{path}/{year(start_date)}/{start_date}_
               {title_to_file(title)}.yaml")

  if (file.exists(file) & overwrite == FALSE) {
  stop("File exists. Do you want overwrite = TRUE?")
  } else {
  write_yaml(new, file = file)
  edit_file(file)
  }
}


#' title to file
#' @importFrom stringr str_squish str_to_lower str_replace_all str_remove_all
#' @param title Activity title to be tidied
#' @return Tidy version of activity title

title_to_file <- function(title){
  title %>%
    str_squish() %>%
    str_to_lower() %>%
    str_replace_all("\\&", "and") %>%
    str_replace_all("\\+", "and") %>%
    str_remove_all("[^\\w\\s]") %>%
    str_replace_all("\\s", "-")
}
