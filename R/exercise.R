# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Exercise Log from the Fitbit API
#' @description Extract date, time, type, duration, steps, calories, and heart
#'     rate in exercise sessions from the Fitbit API.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param after.date The date in the format Y-m-d to limit the extraction of the exercise records.
#' @param limit Number of exercise records to extract from the Fitbit API (Maximum: 100), Default: 100
#' @param returnData Return the data to the user's R environment, Default: TRUE
#' @param toSQL Write the data to a SQL database, Default: FALSE
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

get_fitbit_exercise_log <- function(token.pathname, after.date = Sys.Date(),
                                    limit = 100, returnData = TRUE, toSQL = FALSE){

  tkn <- .extract_token(token.pathname)

  activity.url <- paste0("https://api.fitbit.com/1/", "user/", tkn$user, "/", sprintf("activities/list.json?afterDate=%s&sort=desc&offset=0&limit=%s", after.date, limit))
  activities <- jsonlite::fromJSON(httr::content(httr::GET(activity.url, tkn$token), as = "text"))

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

  # data <- data[data$duration >= 1 & !is.na(data$duration), ] # Removed this filter for now as I am not sure the best option

  data <- dplyr::arrange(data, date, time)

  if(toSQL){
    database <- .checkDatabase(tkn$directory, tkn$user)
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    DBI::dbWriteTable(con, "exercise_log", data, overwrite = TRUE)
    DBI::dbDisconnect(con)
  }

  if(returnData){
    return(data)
  }

}
