# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title get_fitbit_report
#' @description Generates an HTML report, returns data, or writes to a XLSX file
#' @param database_pathname Full pathname to the SQL database
#' @param reports_pathname Full pathname to the location that the HTML or XLSX file should be stored, Default: NULL
#' @param returnData Returns the combined data set, Default: TRUE
#' @param toHTML Renders a Rmarkdown report to HTML, Default: FALSE
#' @param toXLSX Writes the full data set to a XLSX file, Default: FALSE
#' @param study_name A customized study name for the iFitbit report, Default: 'study'
#' @param report_author A customized report author for the iFitbit report, Default: 'iFitbit'
#' @param reportName PARAM_DESCRIPTION, Default: 'FitbitReport'
#' @param start.date Add a start date to allow filtering of data when returning the data or writing it to a XLSX
#' @param end.date Add an end date to allow filtering of data when returning the data or writing it to a XLSX
#' @param ... Additional arguments passed to \code{\link[openxlsx]{saveWorkbook}} or \code{\link[rmarkdown]{render}}
#' @return Returns data set or generates a HTML or XLSX file
#' @details Generates an HTML report, returns data, or writes to a XLSX file
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
#' @importFrom openxlsx saveWorkbook writeData addWorksheet createWorkbook

get_fitbit_report <- function(
    database_pathname,
    reports_pathname = NULL,
    returnData = TRUE, toHTML = FALSE, toXLSX = FALSE,
    study_name = "study", report_author = "iFitbit",
    reportName = "FitbitReport",
    start.date,
    end.date, ...){

  # Create a directory for the report if a directory doesn't exist
  if(!dir.exists(reports_pathname)) dir.create(reports_pathname)

  # Identify Fitbit user name for the HTML report
  user <- strsplit(basename(database_pathname), "[.]")[[1]][1]

  # Open the SQL database connection and read in the data
  con <- DBI::dbConnect(RSQLite::SQLite(), database_pathname)
  table_names <- DBI::dbListTables(con)
  activities <- if("activities" %in% table_names) DBI::dbReadTable(con, "activities")
  sleep <- if("sleep" %in% table_names) DBI::dbReadTable(con, "sleep")
  exercise_log <- if("exercise_log" %in% table_names) DBI::dbReadTable(con, "exercise_log")
  heart <- if("heart" %in% table_names) DBI::dbReadTable(con, "heart")
  device <- if("device" %in% table_names) DBI::dbReadTable(con, "device")
  DBI::dbDisconnect(con)

  if(toHTML){
    inputRmd <- paste0(system.file(package = "iFitbit"), "/rmd/", reportName, ".Rmd")
    outputHTML <- paste0(reports_pathname, "/", user, ".html")
    rmarkdown::render(input = inputRmd, output_file = outputHTML, quiet = TRUE, ...)
  }

  # Create a list from the data
  if(toXLSX | returnData){
    names(exercise_log)[2:ncol(exercise_log)] <- paste0("exercise_", names(exercise_log)[2:ncol(exercise_log)])
    data <- list(device = device, activities = activities, sleep = sleep, exercise_log = exercise_log, heart = heart)
  }

  if(toXLSX){
    wb <- openxlsx::createWorkbook()
    sapply(c("Device", "Activities", "Sleep", "Exercise Log", "Heart"), function(x) openxlsx::addWorksheet(wb, sheetName = x))
    openxlsx::writeData(wb, sheet = "Device", x = device)
    openxlsx::writeData(wb, sheet = "Activities", x = activities)
    openxlsx::writeData(wb, sheet = "Sleep", x = sleep)
    openxlsx::writeData(wb, sheet = "Exercise Log", x = exercise_log)
    openxlsx::writeData(wb, sheet = "Heart", x = heart)
    openxlsx::saveWorkbook(wb, paste0(reports_pathname, "/", user, ".xlsx"), overwrite = TRUE, ...)
  }

  if(returnData) return(data)
}




