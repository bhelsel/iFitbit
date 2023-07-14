# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Activities from the Fitbit API
#' @description Extract calories, distance, elevation, physical activity, and steps from the Fitbit API.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param resource The resource of the data to be returned, Default: "All Resources"
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @return Writes the activities to a SQL database stored in the data folder.
#' @details Extract calories, distance, elevation, physical activity, and steps from the Fitbit API.
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get_fitbit_activities
#' @export

get_fitbit_activities <- function(token.pathname, resource = "All Resources", start.date = Sys.Date(), end.date = Sys.Date()){
  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  url_activity <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "activities/")

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
      activityCalories <- jsonlite::fromJSON(httr::content(httr::GET(activityCalories.url, token), as = "text"))[[1]][2]
      colnames(activityCalories) <- "activityCalories"
      temp_data <- cbind(temp_data, activityCalories)
    }

    if("calories" %in% resource){
      calories.url <- paste0(url_activity, "calories", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      calories <- jsonlite::fromJSON(httr::content(httr::GET(calories.url, token), as = "text"))[[1]][2]
      colnames(calories) <- "calories"
      temp_data <- cbind(temp_data, calories)
    }

    if("distance" %in% resource){
      distance.url <- paste0(url_activity, "distance", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      distance <- jsonlite::fromJSON(httr::content(httr::GET(distance.url, token), as = "text"))[[1]][2]
      colnames(distance) <- "distance"
      temp_data <- cbind(temp_data, distance)
    }

    if("elevation" %in% resource){
      elevation.url <- paste0(url_activity, "elevation", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      elevation <- jsonlite::fromJSON(httr::content(httr::GET(elevation.url, token), as = "text"))[[1]][2]
      colnames(elevation) <- "elevation"
      temp_data <- cbind(temp_data, elevation)
    }

    if("floors" %in% resource){
      floors.url <- paste0(url_activity, "floors", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      floors <- jsonlite::fromJSON(httr::content(httr::GET(floors.url, token), as = "text"))[[1]][2]
      colnames(floors) <- "floors"
      temp_data <- cbind(temp_data, floors)
    }

    if("minutesSedentary" %in% resource){
      minutesSedentary.url <- paste0(url_activity, "minutesSedentary", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesSedentary <- jsonlite::fromJSON(httr::content(httr::GET(minutesSedentary.url, token), as = "text"))[[1]][2]
      colnames(minutesSedentary) <- "minutesSedentary"
      temp_data <- cbind(temp_data, minutesSedentary)
    }

    if("minutesLightlyActive" %in% resource){
      minutesLightlyActive.url <- paste0(url_activity, "minutesLightlyActive", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesLightlyActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesLightlyActive.url, token), as = "text"))[[1]][2]
      colnames(minutesLightlyActive) <- "minutesLightlyActive"
      temp_data <- cbind(temp_data, minutesLightlyActive)
    }

    if("minutesFairlyActive" %in% resource){
      minutesFairlyActive.url <- paste0(url_activity, "minutesFairlyActive", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesFairlyActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesFairlyActive.url, token), as = "text"))[[1]][2]
      colnames(minutesFairlyActive) <- "minutesFairlyActive"
      temp_data <- cbind(temp_data, minutesFairlyActive)
    }

    if("minutesVeryActive" %in% resource){
      minutesVeryActive.url <- paste0(url_activity, "minutesVeryActive", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      minutesVeryActive <- jsonlite::fromJSON(httr::content(httr::GET(minutesVeryActive.url, token), as = "text"))[[1]][2]
      colnames(minutesVeryActive) <- "minutesVeryActive"
      temp_data <- cbind(temp_data, minutesVeryActive)
    }

    if("steps" %in% resource){
      steps.url <- paste0(url_activity, "steps", sprintf("/date/%s/%s.json", start.date[i], end.date[i]))
      steps <- jsonlite::fromJSON(httr::content(httr::GET(steps.url, token), as = "text"))[[1]][2]
      colnames(steps) <- "steps"
      temp_data <- cbind(temp_data, steps)
    }

    data <- rbind(data, temp_data)
  }

  database <- grep(user, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbWriteTable(con, "activities", data, overwrite = TRUE)
  DBI::dbDisconnect(con)
}

#' @title Extract Exercise Log from the Fitbit API
#' @description Extract date, time, type, duration, steps, calories, and heart
#'     rate in exercise sessions from the Fitbit API.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param limit Number of exercise records to extract from the Fitbit API, Default: 25
#' @return Writes the exercise log to a SQL database stored in the data folder.
#' @details Extract date, time, type, duration, steps, calories, and heart
#'     rate in exercise sessions from the Fitbit API.
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}},
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}},
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}},
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get_fitbit_exercise_log
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr bind_rows

get_fitbit_exercise_log <- function(token.pathname, limit = 25){
  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  activity.url <- paste0("https://api.fitbit.com/1/", "user/", user, "/", sprintf("activities/list.json?beforeDate=%s&sort=desc&offset=0&limit=%s", Sys.Date(), limit))
  activities <- jsonlite::fromJSON(httr::content(httr::GET(activity.url, token), as = "text"))

  if(length(activities[[1]]) != 0){
    date <- format(as.POSIXct(activities[[1]]$startTime, format = "%Y-%m-%dT%H:%M:%OS"), "%Y-%m-%d")
    time <- format(as.POSIXct(activities[[1]]$startTime, format = "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
    type <- if(!is.null(activities[[1]]$activityName)) activities[[1]]$activityName else NA
    duration <- if(!is.null(activities[[1]]$activeDuration)) round(activities[[1]]$activeDuration/60000, 2) else NA
    steps <- if(!is.null(activities[[1]]$steps)) activities[[1]]$steps else NA
    calories <- if(!is.null(activities[[1]]$calories)) activities[[1]]$calories else NA
    hr <- if(!is.null(activities[[1]]$averageHeartRate)) activities[[1]]$averageHeartRate else NA
    distance <- if(!is.null(activities[[1]]$distance)) activities[[1]]$distance else NA
    distanceUnit <- if(!is.null(activities[[1]]$distanceUnit)) activities[[1]]$distanceUnit else NA
    logType <- if(!is.null(activities[[1]]$logType)) activities[[1]]$logType else NA

    if(!is.null(activities[[1]]$activityLevel)){
      activityLevel <-
        1:length(activities[[1]]$activityLevel) %>%
        lapply(.,
               function(x) {
                 temp <- activities[[1]]$activityLevel[[x]][c("name", "minutes")]
                 temp <- purrr::as_vector(temp$minutes) %>%
                   `names<-`(c("sedentary", "lightly_active", "fairly_active", "very_active"))
               }
        ) %>%
        dplyr::bind_rows()
    } else{
      activityLevel <- NA
    }

    if(!is.null(activities[[1]]$heartRateZones)){
      heartZones <-
        1:length(activities[[1]]$heartRateZones) %>%
        lapply(.,
               function(x) {
                 temp_names <- c("out_of_range", "fat_burn", "cardio", "peak")
                 if(!is.null(activities[[1]]$heartRateZones[[x]])){
                   temp <- activities[[1]]$heartRateZones[[x]][c("min", "max", "minutes")]
                   heart_range <- paste0(temp$min, "-", temp$max, " bpm") %>% `names<-`(paste0(temp_names, "_heart_range"))
                   minutes <- purrr::as_vector(temp$minutes) %>% `names<-`(paste0(temp_names, "_min"))
                   if("caloriesOut" %in% names(activities[[1]]$heartRateZones[[x]])){
                     calories <-
                       activities[[1]]$heartRateZones[[x]]["caloriesOut"] %>%
                       purrr::as_vector() %>%
                       `names<-`(paste0(temp_names, "_kcals"))
                     temp <- c(heart_range, minutes, calories)
                   } else {
                     temp <- c(heart_range, minutes)
                   }
                 } else {
                   temp <- rep(NA, 12)
                   names <- c(paste0(temp_names, "_heart_range"), paste0(temp_names, "_min"), paste0(temp_names, "_kcals"))
                   names(temp) <- names
                 }
                 return(temp)
               }
        ) %>%
        dplyr::bind_rows()
    } else{
      heartZones <- NA
    }
  } else{
    date = time = type = duration = steps = calories = hr = distance = distanceUnit = logType = activityLevel = heartZones = NA
  }


  data <- data.frame(cbind(date, time, type, duration, steps, calories, hr, distance, distanceUnit, logType, activityLevel, heartZones))
  data <- data[data$duration >= 1 & !is.na(data$duration), ]
  database <- grep(user, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbWriteTable(con, "exercise_log", data, overwrite = TRUE)
  DBI::dbDisconnect(con)
}















