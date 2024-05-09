# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Activities from the Fitbit API
#' @description Extract calories, distance, elevation, physical activity, and steps from the Fitbit API.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param resource The resource of the data to be returned, Default: "All Resources"
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param returnData Return the data to the user's R environment, Default: TRUE
#' @param toSQL Write the data to a SQL database, Default: FALSE
#' @return Writes the activities to a SQL database stored in the data folder.
#' @details Extract calories, distance, elevation, physical activity, and steps from the Fitbit API.
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get_fitbit_activities
#' @export

get_fitbit_activities <- function(token.pathname, resource = "All Resources",
                                  start.date = Sys.Date(), end.date = Sys.Date(),
                                  returnData = TRUE, toSQL = FALSE){

  tkn <- .extract_token(token.pathname)

  url_activity <- paste0("https://api.fitbit.com/1/", "user/", tkn$user, "/", "activities/")

  # Check to see if data is greater than 100 days
  dateDiff <- difftime(end.date, start.date)

  if(dateDiff > 100){
    mid.date <- as.Date(start.date) + floor(dateDiff / 2)
    start.date <- c(start.date, as.character(mid.date))
    end.date <- c(as.character(mid.date + 1), end.date)
  }

  if("All Resources" %in% resource){
    resource <- c("activityCalories", "calories", "distance", "elevation", "floors", "minutesSedentary",
                  "minutesLightlyActive", "minutesFairlyActive", "minutesVeryActive", "steps")
  }

  data <- data.frame(matrix(nrow = 0, ncol = length(resource) + 1))
  colnames(data) <- c("date", resource)

  for(i in 1:length(start.date)){
    temp_dates <- seq.Date(as.Date(start.date[i]), as.Date(end.date[i]), "day")
    temp_data <- data.frame(date = as.character(temp_dates))

    if("activityCalories" %in% resource){
      activityCalories.url <- paste0(url_activity, "activityCalories", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      activityCalories <- jsonlite::fromJSON(httr::content(httr::GET(activityCalories.url, tkn$token), as = "text"))[[1]][2]
      colnames(activityCalories) <- "activityCalories"
      temp_data <- cbind(temp_data, activityCalories)
    }

    if("calories" %in% resource){
      calories.url <- paste0(url_activity, "calories", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      calories <- jsonlite::fromJSON(httr::content(httr::GET(calories.url, tkn$token), as = "text"))[[1]][2]
      colnames(calories) <- "calories"
      temp_data <- cbind(temp_data, calories)
    }

    if("distance" %in% resource){
      distance.url <- paste0(url_activity, "distance", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      distance <- jsonlite::fromJSON(httr::content(httr::GET(distance.url, tkn$token), as = "text"))[[1]][2]
      colnames(distance) <- "distance"
      temp_data <- cbind(temp_data, distance)
    }

    if("elevation" %in% resource){
      elevation.url <- paste0(url_activity, "elevation", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      elevation <- jsonlite::fromJSON(httr::content(httr::GET(elevation.url, tkn$token), as = "text"))[[1]][2]
      colnames(elevation) <- "elevation"
      temp_data <- cbind(temp_data, elevation)
    }

    if("floors" %in% resource){
      floors.url <- paste0(url_activity, "floors", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      floors <- jsonlite::fromJSON(httr::content(httr::GET(floors.url, tkn$token), as = "text"))[[1]][2]
      colnames(floors) <- "floors"
      temp_data <- cbind(temp_data, floors)
    }

    if("minutesSedentary" %in% resource){
      minutesSedentary.url <- paste0(url_activity, "minutesSedentary", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesSedentary <- jsonlite::fromJSON(httr::content(httr::GET(minutesSedentary.url, tkn$token), as = "text"))[[1]][2]
      colnames(minutesSedentary) <- "minutesSedentary"
      temp_data <- cbind(temp_data, minutesSedentary)
    }

    if("minutesLightlyActive" %in% resource){
      minutesLightlyActive.url <- paste0(url_activity, "minutesLightlyActive", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesLightlyActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesLightlyActive.url, tkn$token), as = "text"))[[1]][2]
      colnames(minutesLightlyActive) <- "minutesLightlyActive"
      temp_data <- cbind(temp_data, minutesLightlyActive)
    }

    if("minutesFairlyActive" %in% resource){
      minutesFairlyActive.url <- paste0(url_activity, "minutesFairlyActive", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesFairlyActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesFairlyActive.url, tkn$token), as = "text"))[[1]][2]
      colnames(minutesFairlyActive) <- "minutesFairlyActive"
      temp_data <- cbind(temp_data, minutesFairlyActive)
    }

    if("minutesVeryActive" %in% resource){
      minutesVeryActive.url <- paste0(url_activity, "minutesVeryActive", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesVeryActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesVeryActive.url, tkn$token), as = "text"))[[1]][2]
      colnames(minutesVeryActive) <- "minutesVeryActive"
      temp_data <- cbind(temp_data, minutesVeryActive)
    }

    if("steps" %in% resource){
      steps.url <- paste0(url_activity, "steps", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      steps <- jsonlite::fromJSON(httr::content(httr::GET(steps.url, tkn$token), as = "text"))[[1]][2]
      colnames(steps) <- "steps"
      temp_data <- cbind(temp_data, steps)
    }

    data <- rbind(data, temp_data)
  }

  if(toSQL){
    database <- .checkDatabase(tkn$directory, tkn$user)
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    DBI::dbWriteTable(con, "activities", data, overwrite = TRUE)
    DBI::dbDisconnect(con)
  }

  if(returnData) {
    return(data)
  }

}















