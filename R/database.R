#' @title .removeTables
#' @description Easy removal of table(s) from the SQL databases
#' @param directory A directory where the data folder containing the SQL databases is stored.
#' @param database Path name to the SQL database containing the extracted Fitbit data.
#' @return Returns
#' @details Easy removal of table(s) from the SQL databases
#' @noRd
#' @keywords internal

.removeTables <- function(directory = NULL, database = NULL){

  if(!is.null(directory)){
    files <- list.files(file.path(directory, "data"), full.names = TRUE)
  }

  if(!is.null(database) & is.null(directory)){
    files <- database
  }

  choices <- c("activities", "device", "exercise_log", "heart", "sleep", "all")
  rlang::inform("What tables do you want to remove?")
  table_selection <- utils::menu(choices)
  if(table_selection == 6) {
    tables <- choices[1:5]
  } else {
    tables <- choices[table_selection]
  }

  if(table_selection == 6) {
    rlang::inform("Are you sure you want to delete all of the tables?")
    out <- utils::menu(c("No", "Yes"))
  }

  if(table_selection != 6){
    rlang::inform(sprintf("Are you sure you want to delete the %s table?", tables))
    out <- utils::menu(c("No", "Yes"))
  }

  for(t in tables){
    if(out == 2){
      for(file in files){
        user_id <- strsplit(basename(file), "[.]")[[1]][1]
        con <- DBI::dbConnect(RSQLite::SQLite(), file)
        if(t %in% DBI::dbListTables(con)){
          DBI::dbRemoveTable(con, t)
          DBI::dbDisconnect(con)
          print(sprintf("Removed the %s table from Fitbit user %s", t, user_id))
        }
      }
    }
  }
  return(NULL)
}

#' @title .listTables
#' @description List all of the table(s) within the SQL databases
#' @param directory A directory where the data folder containing the SQL databases is stored.
#' @param database Path name to the SQL database containing the extracted Fitbit data.
#' @return Prints a list of tables within the SQL databases
#' @details List all of the table(s) within the SQL databases
#' @noRd
#' @keywords internal

.listTables <- function(directory = NULL, database = NULL){

  if(!is.null(directory)){
    files <- list.files(file.path(directory, "data"), full.names = TRUE)
  }

  if(!is.null(database) & is.null(directory)){
    files <- database
  }

  for(file in files){
    user_id <- strsplit(basename(file), "[.]")[[1]][1]
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    tables <- DBI::dbListTables(con)
    DBI::dbDisconnect(con)
    tables <- paste0("{", tables, "}", collapse = " | ")
    if(tables == "{}"){
      print(sprintf("Fitbit user %s has no tables", user_id))
    } else{
      print(sprintf("Fitbit user %s has %s tables", user_id, tables))
    }
  }
  return(NULL)
}


#' @title .readTables
#' @description Read the table(s) within the SQL databases
#' @param directory A directory where the data folder containing the SQL databases is stored.
#' @param database Path name to the SQL database containing the extracted Fitbit data.
#' @return Reads a specific table(s) within the SQL databases and returns the table(s) as a data set.
#' @details Read the table(s) within the SQL databases
#' @noRd
#' @keywords internal

.readTables <- function(directory = NULL, database = NULL, tableName = NULL){

  if(!is.null(directory)){
    files <- list.files(file.path(directory, "data"), full.names = TRUE)
  }

  if(!is.null(database) & is.null(directory)){
    files <- database
  }

  main_data <- data.frame()

  for(file in files){
    user_id <- strsplit(basename(file), "[.]")[[1]][1]
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    if(tableName %in% DBI::dbListTables(con)){
      data <- DBI::dbReadTable(con, tableName)
      main_data <- plyr::rbind.fill(main_data, data.frame(user_id, data))
    }
    DBI::dbDisconnect(con)
  }
  return(main_data)
}


.checkDatabase <- function(directory, user){
  if(!dir.exists(file.path(directory, "data"))) dir.create(file.path(directory, "data"))
  user_database <- file.path(directory, "data", sprintf("%s.db", user))
  if(!file.exists(user_database)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), user_database)
    DBI::dbDisconnect(con)
  }
  return(user_database)
}
