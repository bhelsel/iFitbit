#' @title get_fitbit_report
#' @description Generates an HTML report, returns data, or writes to a CSV file
#' @param database_pathname Full pathname to the SQL database
#' @param reports_pathname Full pathname to the location that the HTML or CSV file should be stored, Default: NULL
#' @param returnData Returns the combined data set, Default: TRUE
#' @param toHTML Renders a Rmarkdown report to HTML, Default: FALSE
#' @param toCSV Writes the full data set to a CSV file, Default: FALSE
#' @param study_name A customized study name for the iFitbit report, Default: 'study'
#' @param report_author A customized report author for the iFitbit report, Default: 'iFitbit'
#' @param reportName PARAM_DESCRIPTION, Default: 'FitbitReport'
#' @param ... Additional arguments passed to \code{\link[data.table]{fwrite}} or \code{\link[rmarkdown]{render}}
#' @return Returns data set or generates a HTML or CSV file
#' @details Generates an HTML report, returns data, or writes to a CSV file
#' @seealso
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbReadTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#'  \code{\link[rmarkdown]{render}}
#'  \code{\link[data.table]{fwrite}}
#' @rdname get_fitbit_report
#' @export
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom rmarkdown render
#' @importFrom data.table fwrite

get_fitbit_report <- function(
    database_pathname,
    reports_pathname = NULL,
    returnData = TRUE, toHTML = FALSE, toCSV = FALSE,
    study_name = "study", report_author = "iFitbit",
    reportName = "FitbitReport", ...){

  # Create a directory for the report if a directory doesn't exist
  if(!dir.exists(reports_pathname)) dir.create(reports_pathname)

  # Identify Fitbit user name for the HTML report
  user <- strsplit(basename(database_pathname), "[.]")[[1]][1]

  # Open the SQL database connection and read in the data
  con <- DBI::dbConnect(RSQLite::SQLite(), database_pathname)
  activities <- DBI::dbReadTable(con, "activities")
  exercise_log <- DBI::dbReadTable(con, "exercise_log")
  heart <- DBI::dbReadTable(con, "heart")
  device <- DBI::dbReadTable(con, "device")
  DBI::dbDisconnect(con)

  if(toHTML){
    inputRmd <- paste0(system.file(package = "iFitbit"), "/rmd/", reportName, ".Rmd")
    outputHTML <- paste0(reports_pathname, "/", user, ".html")
    rmarkdown::render(input = inputRmd, output_file = outputHTML, quiet = TRUE, ...)
  }

  # Merge the data and add device attributes
  if(toCSV | returnData){
    names(exercise_log)[2:ncol(exercise_log)] <- paste0("exercise_", names(exercise_log)[2:ncol(exercise_log)])
    data <- merge(activities, merge(exercise_log, heart, by = "date", all = TRUE), by = "date", all = TRUE)
    attr(data, "device") <- device
  }

  if(toCSV){
    data.table::fwrite(
      data,
      paste0(reports_pathname, "/", user, ".csv"),
      dateTimeAs = "write.csv",
      ...
    )
  }

  if(returnData) return(data)
}




