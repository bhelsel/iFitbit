#' @title .max_hr_equations
#' @description Print out maximal heart rate formulas to help users choose a formula to use with the `get_fitbit_heart_intraday` function.
#' @inheritParams get_fitbit_heart_intraday
#' @return A list of formulas, populations, and references to assist the end user in choosing a maximal heart rate formula.
#' @details Print out maximal heart rate formulas to help users choose a formula to use with the `get_fitbit_heart_intraday` function.
#' @noRd
#' @keywords internal

.max_hr_equations <- function(){

  row_names <- c("formulas", "population", "reference")

  equation_names <- c("Fox", "Astrand", "Tanaka", "Gellish", "Gulati", "Fernhall")

  formulas <- c("220 - age", "216.6 - (0.84 * age)", "208 - (0.7 * age)", "207 - (0.7 * age)", "206 - (0.88 * age)", "210 - (0.56 * age) - 31")

  population <- c("Healthy men and women.",
                  "Men and women (4-34 years of age).",
                  "Healthy men and women.",
                  "Men and women in an adult fitness program with broad range of age and fitness levels.",
                  "Asymptomatic middle-aged women referred for stress testing.",
                  "Men and women with Down syndrome (9-46 years of age)")

  reference <- c("Fox et al. Physical activity and the prevention of coronary heart disease. Ann Clin Res. 1971;3(6):404-32.",
                 "Astrand PO. Experimental studies of physical working capacity in relation to sex and age. Copenhagen (Denmark): Musksgaard; 1952. 171p.",
                 "Tanaka et al. Age-predicted maximal heart rate revisted. J Am Coll Cardiol. 2001;37(1):153-6.",
                 "Gellish et al. Longitudinal modeling of the relationship between age and maximal heart rate. Med Sci Sports Exerc. 2007;39(5):822-9.",
                 "Gulati et al. Heart rate response to exercise stress testing in asymptomatic women: the St. James Women Take Heart Project. Circulation. 2010;122(2):130-7.",
                 "Fernhall et al. Prediction of maximal heart rate in individuals with mental retardation. Med Sci Sports Exerc. 2001;33(10):1655-1660.")

  equations <-
    lapply(1:6,
         function(x){
           matrix(c(formulas[x], population[x], reference[x]),
                  dimnames = list(row_names, equation_names[x]))
         }) %>%
    `names<-`(equation_names)

  return(equations)

}


#' @title .print_heart_intensity
#' @description Print out the default ACSM intensities as part of the `get_fitbit_heart_intraday` function.
#' @inheritParams get_fitbit_heart_intraday
#' @return Intensities and their labels for either the heart rate reserve or maximal heart rate methods.
#' @details Print out the default ACSM intensities as part of the `get_fitbit_heart_intraday` function.
#' @noRd
#' @keywords internal

.print_heart_intensity <- function(heart_rate_method){

  if(heart_rate_method == 1 | heart_rate_method == 2) {
    type <- "Heart Rate Reserve"
    int <- c("< 20%", "20-29%", "30-39%", "40-59%", "60-89%", "\u2265 90%")
    int_labels <- c("Sedentary", "Very Light", "Light", "Moderate", "Vigorous", "Near Maximal")
  }

  if(heart_rate_method == 3){
    type <- "Maximal Heart Rate"
    int <- c("< 47%", "47-56%", "57-63%", "64-76%", "77-95%", "\u2265 96%")
    int_labels <- c("Sedentary", "Very Light", "Light", "Moderate", "Vigorous", "Near Maximal")
  }

  defaultIntensities <- sapply(1:6, function(x) paste0(int_labels[x], ": ", int[x]))

  glue::glue("\n
             Default ACSM {type} intensities were used.\n
             {glue::glue_collapse(defaultIntensities, ' | ')}\n
             ACSM Guidelines for Exercise Testing and Prescription, 11th Edition (page 148, table 5.2)
             \n")
}


#' @title .extract_token
#' @description Extracts directory, token information, and Fitbit user id
#' @param token.pathname Full pathname to the location of the access token
#' @return A list containing the directory, token information, and Fitbit user id
#' @details Extracts directory, token information, and Fitbit user id
#' @noRd
#' @keywords internal

.extract_token <- function(token.pathname){
  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  l <- list(directory, token, user)
  l <- stats::setNames(l, c("directory", "token", "user"))
  return(l)
}


#' @title .adjustHeartDates
#' @description Adjusts the start and end date for the `get_fitbit_heart_intraday` function
#' @param token.pathname Full pathname to the location of the access token
#' @param database Name of the database to retrieve the last date value (Choices: activity, heart, and sleep)
#' @param overwrite Data extraction should be continued from the most recent date in the heart SQL database
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @return An updated start and end date for the `get_fitbit_heart_intraday` function
#' @details Adjusts the start and end date for the `get_fitbit_heart_intraday` function
#' @noRd
#' @keywords internal

.adjustDates <- function(token.pathname, database, overwrite = FALSE, start.date, end.date){
  tkn <- .extract_token(token.pathname)
  device <- get_fitbit_device(token.pathname, returnData = TRUE)
  lastSync <- as.POSIXct(device$lastSyncTime, format = "%Y-%m-%dT%H:%M:%OS")
  if(end.date > lastSync) end.date <- as.Date(lastSync) - 1
  if(!overwrite){
    con <- DBI::dbConnect(RSQLite::SQLite(), .checkDatabase(directory, tkn$user))
    lastDate <- tail(DBI::dbReadTable(con, database)$date, 1)
    DBI::dbDisconnect(con)
    if(length(lastDate) != 0) start.date <- as.Date(lastDate) - 1
  }
  return(list(start.date = start.date, end.date = end.date))
}


