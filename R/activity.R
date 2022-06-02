#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param directory PARAM_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @param resource.options PARAM_DESCRIPTION, Default: c()
#' @param start.date PARAM_DESCRIPTION, Default: ''
#' @param end.date PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get.fitbit.activities
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite

get.fitbit.activities <- function(directory, id, resource.options = c(), start.date = "", end.date = ""){
  token.pathname <- grep(id, list.files(paste0(directory, "/tokens"), full.names = TRUE), value = TRUE)
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  url_activity <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "activities/")
  date <- seq.Date(as.Date(start.date), as.Date(end.date), "day")
  data <- data.frame(date)

  if("all" %in% resource.options){
    resource.options <- c("activityCalories", "calories", "distance", "elevation", "floors", "minutesSedentary",
                         "minutesLightlyActive", "minutesFairlyActive", "minutesVeryActive", "steps")
  }

  if("activityCalories" %in% resource.options){
    activityCalories.url <- paste0(url_activity, "activityCalories", sprintf("/date/%s/%s.json", start.date, end.date))
    activityCalories <- jsonlite::fromJSON(httr::content(httr::GET(activityCalories.url, token), as = "text"))[[1]][2]
    colnames(activityCalories) <- "activityCalories"
    data <- cbind(data, activityCalories)
  }

  if("calories" %in% resource.options){
    calories.url <- paste0(url_activity, "calories", sprintf("/date/%s/%s.json", start.date, end.date))
    calories <- jsonlite::fromJSON(httr::content(httr::GET(calories.url, token), as = "text"))[[1]][2]
    colnames(calories) <- "calories"
    data <- cbind(data, calories)
  }

  if("distance" %in% resource.options){
    distance.url <- paste0(url_activity, "distance", sprintf("/date/%s/%s.json", start.date, end.date))
    distance <- jsonlite::fromJSON(httr::content(httr::GET(distance.url, token), as = "text"))[[1]][2]
    colnames(distance) <- "distance"
    data <- cbind(data, distance)
  }

  if("elevation" %in% resource.options){
    elevation.url <- paste0(url_activity, "elevation", sprintf("/date/%s/%s.json", start.date, end.date))
    elevation <- jsonlite::fromJSON(httr::content(httr::GET(elevation.url, token), as = "text"))[[1]][2]
    colnames(elevation) <- "elevation"
    data <- cbind(data, elevation)
  }

  if("floors" %in% resource.options){
    floors.url <- paste0(url_activity, "floors", sprintf("/date/%s/%s.json", start.date, end.date))
    floors <- jsonlite::fromJSON(httr::content(httr::GET(floors.url, token), as = "text"))[[1]][2]
    colnames(floors) <- "floors"
    data <- cbind(data, floors)
  }

  if("minutesSedentary" %in% resource.options){
    minutesSedentary.url <- paste0(url_activity, "minutesSedentary", sprintf("/date/%s/%s.json", start.date, end.date))
    minutesSedentary <- jsonlite::fromJSON(httr::content(httr::GET(minutesSedentary.url, token), as = "text"))[[1]][2]
    colnames(minutesSedentary) <- "minutesSedentary"
    data <- cbind(data, minutesSedentary)
  }

  if("minutesLightlyActive" %in% resource.options){
    minutesLightlyActive.url <- paste0(url_activity, "minutesLightlyActive", sprintf("/date/%s/%s.json", start.date, end.date))
    minutesLightlyActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesLightlyActive.url, token), as = "text"))[[1]][2]
    colnames(minutesLightlyActive) <- "minutesLightlyActive"
    data <- cbind(data, minutesLightlyActive)
  }

  if("minutesFairlyActive" %in% resource.options){
    minutesFairlyActive.url <- paste0(url_activity, "minutesFairlyActive", sprintf("/date/%s/%s.json", start.date, end.date))
    minutesFairlyActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesFairlyActive.url, token), as = "text"))[[1]][2]
    colnames(minutesFairlyActive) <- "minutesFairlyActive"
    data <- cbind(data, minutesFairlyActive)
  }

  if("minutesVeryActive" %in% resource.options){
    minutesVeryActive.url <- paste0(url_activity, "minutesVeryActive", sprintf("/date/%s/%s.json", start.date, end.date))
    minutesVeryActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesVeryActive.url, token), as = "text"))[[1]][2]
    colnames(minutesVeryActive) <- "minutesVeryActive"
    data <- cbind(data, minutesVeryActive)
  }

  if("steps" %in% resource.options){
    steps.url <- paste0(url_activity, "steps", sprintf("/date/%s/%s.json", start.date, end.date))
    steps <- jsonlite::fromJSON(httr::content(httr::GET(steps.url, token), as = "text"))[[1]][2]
    colnames(steps) <- "steps"
    data <- cbind(data, steps)
  }

  database <- grep(id, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbWriteTable(con, "activities", data, overwrite = TRUE)
  DBI::dbDisconnect(con)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param directory PARAM_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @param limit PARAM_DESCRIPTION, Default: 25
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname exerciseLog
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite

exerciseLog <- function(directory, id, limit = 25){
  token.pathname <- grep(id, list.files(paste0(directory, "/tokens"), full.names = TRUE), value = TRUE)
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  activity.url <- paste0("https://api.fitbit.com/1/", "user/", user, "/", sprintf("activities/list.json?beforeDate=%s&sort=desc&offset=0&limit=%s", Sys.Date(), limit))
  activities <- jsonlite::fromJSON(httr::content(httr::GET(activity.url, token), as = "text"))
  date <- format(as.POSIXct(activities[[1]]$startTime, format = "%Y-%m-%dT%H:%M:%OS"), "%Y-%m-%d")
  time <- format(as.POSIXct(activities[[1]]$startTime, format = "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
  type <- activities[[1]]$activityName
  duration <- round(activities[[1]]$activeDuration/60000, 2)
  steps <- activities[[1]]$steps
  calories <- activities[[1]]$calories
  hr <- activities[[1]]$averageHeartRate
  data <- data.frame(cbind(date, time, type, duration, steps, calories, hr))
  database <- grep(id, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbWriteTable(con, "exerciseLog", data, overwrite = TRUE)
  DBI::dbDisconnect(con)
}


