#' import_cpd
#' @importFrom lubridate today ymd
#' @importFrom tibble tibble
#' @importFrom yaml write_yaml
#' @importFrom glue glue
#' @param title Title for new CPD record
#' @param type Type of new CPD record, can be one of "work", "professional", "formal", "self" or "other"
#' @param date The start date of the activity in yyyy-mm-dd format
#' @param learning_hours The number of learning hours accruded during this activity
#' @export
new_cpd = function(title = "", type = "other", date = today(), learning_hours = 0){

   types = tibble(
    short = c("work", "professional", "formal", "self", "other"),
    long = c("Work based learning", "Professional activity", "Formal / educational", "Self-directed learning", "Other")
  )

  if (!(type %in% types$short)) {
    stop("Unknown type")
  }

  start_date = ymd(date)

  blank =
    tibble(title = title,
           activity_type = types$long[which(type == types$short)],
           start_date = as.character(start_date),
           end_date = as.character(start_date),
           activity_url = '',
           learning_hours = as.character(learning_hours),
           tags = '',
           activity_description = "Review needed",
           benefit_to_practice = "Review needed",
           benefit_to_users = "Review needed"
    )
  year = year(start_date)

  write_yaml(blank, file = glue("cpd-records/{year}/{start_date}.yaml"))
  }
