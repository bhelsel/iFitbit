FitbitAPI.Render.Reports <- function(directory, id){
  reports_pathname = paste0(directory, "/reports")
  if(!dir.exists(reports_pathname)) dir.create(reports_pathname)
  database <- grep(id, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  activities <- DBI::dbReadTable(con, "activities")
  heart <- DBI::dbReadTable(con, "heart")
  exercise_log <- DBI::dbReadTable(con, "exercise_log")
  device <- DBI::dbReadTable(con, "device")
  DBI::dbDisconnect(con)

  rmarkdown::render(input = paste0(system.file(package = "iFitbit"), "/rmd/FitbitReport.Rmd"),
                    output_file = paste0(reports_pathname, "/", id, ".html"),
                    quiet = TRUE)
}




