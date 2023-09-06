# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title get_fitbit_heart_intraday
#' @description Retreives a continuous measure of heart rate from the Fitbit API
#' @param token.pathname Full pathname to the location of the access token
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param force_start_date Force the start.date to be used, Default: FALSE
#' @param age Age of the participant (extracts from the Fitbit user's profile if NULL), Default: NULL
#' @param max_hr_formula An age-based formula to use to calculate maximal heart rate, Default: '220 - age'
#' @param heart_intensity Heart rate percentages expressed as decimal points to use when calculating
#'  intensity minutes (uses ACSM as default if NULL), Default: NULL
#' @param heart_intensity_labels Labels to go with the `heart_intensity` percentages (uses ACSM default
#'  of sedentary, very light, light, moderate, vigorous, and near maximal if NULL), Default: NULL
#' @param heart_rate_method A numeric value of 1, 2, or 3 to specify the approach to calculating heart
#'  rate intensities (see details for more information), Default: 1
#' @param resting_heart_rate A constant resting heart rate to use with `heart_rate_method` = 2 if it
#'  is a measure collected by the study, Default: NULL
#' @param detail.level Detail level of the data points that should be included.
#'  The detail level can be '1sec', '1min', '5min', or '15min', Default: '1min'
#' @param returnRawData Return the time stamped heart rate data with 0 values for any missing heart
#'  rate data, Default: FALSE
#' @param returnData Return a summary of the intensity minutes to the user's R environment, Default: TRUE
#' @param toSQL Write a summary of the intensity minutes to a SQL database, Default: FALSE
#' @param verbose Print the progress of the Fitbit API heart rate data extraction and calculation of intensity minutes, Default: FALSE
#' @return A measure of day-level non-wear, sleep, sedentary time, and physical activity
#' @details This function retrieves a continuous measure of heart rate from the Fitbit API. The user has access to a variety of
#'  arguments within this function to customize how intensity minutes are calculated For example, a user can use heart rate reserve
#'  or maximal heart rate percentages from ACSM by changing the heart_rate_method to (1) heart rate reserve using a resting heart rate
#'  value from the Fitbit API for that day (or the last stored resting heart rate value if there is not one available for that day),
#'  (2) heart rate reserve by providing a resting heart rate in the `resting_heart_rate` argument, or (3) maximal heart rate. The user
#'  can also change how the max heart rate is calculated by specifying an age-based formula to use in the `max_hr_formula` argument.
#'  By default, the ACSM intensities for heart rate reserve or percent of maximal heart rate are used (ACSM Guidelines for Exercise Testing
#'  and Prescription, 11th Edition, page 148, table 5.2).
#' @seealso
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbExistsTable}}, \code{\link[DBI]{dbReadTable}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[tidyr]{complete}}
#'  \code{\link[dplyr]{bind_cols}}, \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{summarise_all}}
#'  \code{\link[plyr]{rbind.fill}}
#' @rdname get_fitbit_heart_intraday
#' @export

get_fitbit_heart_intraday <- function(token.pathname, start.date = Sys.Date(), end.date = Sys.Date(),
                                      force_start_date = FALSE, age = NULL, max_hr_formula = "220 - age",
                                      heart_intensity = NULL, heart_intensity_labels = NULL,
                                      heart_rate_method = 1, resting_heart_rate = NULL,
                                      detail.level = "1min", returnRawData = FALSE, returnData = TRUE,
                                      toSQL = FALSE, verbose = FALSE){

  if(length(heart_intensity) != length(heart_intensity_labels)) stop("Heart intensities and their labels must be the same length")

  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  device <- get_fitbit_device(token.pathname, returnData = TRUE)
  lastSync <- as.POSIXct(device$lastSyncTime, format = "%Y-%m-%dT%H:%M:%OS")

  if(!force_start_date) {
    if(start.date < as.Date(lastSync)) start.date <- as.Date(lastSync) - 1
  }

  if(end.date > Sys.Date()) end.date <- Sys.Date()

  database <- grep(user, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)

  if(length(database) != 0){
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    if(!force_start_date){
      if(DBI::dbExistsTable(con, "heart")) {
        current_data <- DBI::dbReadTable(con, "heart")
        current_data <- current_data[1:(min(which(current_data$date == start.date))-1), ]
      }
    }
  }

  # Calculate max heart rate
  if(is.null(age)) age <- get_fitbit_profile(token.pathname)$user$age
  max.hr <- round(eval(parse(text = max_hr_formula)))

  # Extract sleep date
  sleep <- get_fitbit_sleep(token.pathname, start.date = start.date, end.date = end.date)

  if(is.null(heart_intensity_labels)) {
    intensity_labels <- c("sedentary", "very.light", "light", "moderate", "vigorous", "near.maximal")
  } else{
    intensity_labels <- heart_intensity_labels
  }

  heart_raw_data <- data.frame()

  if(exists("current_data")){
    heart_activities <- current_data
  } else{
    heart_activities <- data.frame()
  }

  # Loop through the days to pull values from the heart rate intraday data and calculate intensities
  for(date in as.character(seq(start.date, end.date, by = "day"))){
    if(verbose) print(sprintf("Extracting Fitbit Heart Rate Intraday Data for %s on %s", user, format(as.Date(date), "%B %d, %Y")))
    heart.url <- sprintf("https://api.fitbit.com/1/user/%s/activities/heart/date/%s/1d/%s.json", user, date, detail.level)
    heart <- jsonlite::fromJSON(httr::content(httr::GET(heart.url, token), as = "text"))
    if(length(heart$`activities-heart-intraday`$dataset) == 0) break
    heart_data <- heart$`activities-heart-intraday`$dataset %>% `colnames<-`(c("time", "hr"))

    # Calculate intensity by heart rate reserve or max heart rate
    if(heart_rate_method == 1){
      if(!is.null(heart[[1]]$value$restingHeartRate)) {
        rest.hr <- heart[[1]]$value$restingHeartRate
      } else if (any(!is.na(heart_activities$rest.hr))){
          rest.hr <- heart_activities[which.max(as.Date(heart_activities[!is.na(heart_activities$rest.hr), "date"])), "rest.hr"]
      } else {
          warning(sprintf("Could not find resting heart rate for %s", format(as.Date(date), "%B %d, %Y")))
          break
      }
      if(min(heart_data$hr) < rest.hr) rest.hr <- min(heart_data$hr)
      if(is.null(heart_intensity)) {
          intensities <- c(0, 0.20, 0.30, 0.40, 0.60, 0.90, 1) # Heart Rate Reserve
      } else {
        intensities <- heart_intensity
      }
      heart_categories <- do.call("c", lapply(intensities, function(x) ((max.hr - rest.hr) * x) + rest.hr))
    }

    if(heart_rate_method == 2 & !is.null(resting_heart_rate)){
      rest.hr <- resting_heart_rate
      if(min(heart_data$hr) < rest.hr) rest.hr <- min(heart_data$hr)
      if(is.null(heart_intensity)) {
        intensities <- c(0, 0.20, 0.30, 0.40, 0.60, 0.90, 1) # Heart Rate Reserve
      } else {
        intensities <- heart_intensity
      }
      heart_categories <- do.call("c", lapply(intensities, function(x) ((max.hr - rest.hr) * x) + rest.hr))
    }

    if(heart_rate_method == 3 | (is.null(heart[[1]]$value$restingHeartRate & is.null(resting_heart_rate)))){
      if(is.null(heart_intensity)) {
        intensities <- c(0, 0.40, 0.57, 0.64, 0.77, 0.96, 1) # Max HR
      } else{
        intensities <- heart_intensity
      }
      heart_categories <- do.call("c", lapply(intensities, function(x) (max.hr * x)))
    }

    if(max(heart_data$hr) > max.hr) max.hr <- max(heart_data$hr)

    if(length(heart_data)!=0){
      heart_data$time <- as.POSIXct(paste(date, heart_data$time), tz = "UTC")
      start_time <- as.POSIXct(paste(date, "00:00:00"), tz = "UTC")
      stop_time <- as.POSIXct(paste(date, "23:59:59"), tz = "UTC")
      heart_data %<>% tidyr::complete(time = seq.POSIXt(start_time, stop_time, by = "min"), fill = list(hr = 0))

      # Add resting heart rate
      heart_data$rest.hr <- rest.hr

      # Add sleep
      if(nrow(sleep)!=0){
        SleepStartTime <- as.POSIXct(sleep[sleep$dateOfSleep==date, c("startTime")], format = "%Y-%m-%dT%H:%M:%OS")
        SleepEndTime <- as.POSIXct(sleep[sleep$dateOfSleep==date, c("endTime")], format = "%Y-%m-%dT%H:%M:%OS")
        heart_data$sleep <- 0
        if(length(SleepStartTime)!=0){
          for(i in 1:length(SleepStartTime)){
            if(format(SleepStartTime[i], "%Y-%m-%d") != date){
              SleepStartTime[i] == as.POSIXct(date, format = "%Y-%m-%d")
            }
            heart_data$sleep <- ifelse(heart_data$time>=SleepStartTime[i] & heart_data$time<=SleepEndTime[i], 1, heart_data$sleep)
          }
        }
      }
      if(nrow(sleep)==0){
        heart_data$sleep <- 0
      }

      # Add Nonwear and Intensity
      heart_data$hrr.percent <- cut(heart_data$hr, breaks = heart_categories, labels = intensity_labels, include.lowest = TRUE, right = FALSE)
      heart_data$nonwear <- ifelse(heart_data$hr == 0, 1, 0)
      heart_data$sleep <- ifelse(heart_data$sleep == 1 & heart_data$nonwear == 0, 1, 0)
      heart_data[which(heart_data$nonwear == 1 | heart_data$sleep == 1), "hrr.percent"] <- NA
      heart_data <- dplyr::bind_cols(cbind(heart_data[, c("time", "hr", "rest.hr", "sleep", "nonwear")], sapply(intensity_labels, function(x) heart_data[x] <- dplyr::if_else(heart_data$hrr.percent == x, 1, 0, 0))))

      # Calculate MVPA if not already provided in intensities
      if(!any(grepl("mvpa", colnames(heart_data), ignore.case = TRUE))){
        mod_pos <- min(grep("mod", colnames(heart_data), ignore.case = TRUE))
        heart_data$mvpa <- rowSums(heart_data[, mod_pos:ncol(heart_data)])
      }

      heart_raw_data <- plyr::rbind.fill(heart_raw_data, heart_data)
      heart_activities <- plyr::rbind.fill(heart_activities, cbind(date = date, rest.hr = rest.hr, dplyr::summarise_all(heart_data[, c(3:ncol(heart_data))], sum)))
    }
  }

  if(nrow(heart_activities) != 0){
    if(verbose & is.null(heart_intensity)) print(.print_heart_intensity(heart_rate_method))

    if(toSQL){
      DBI::dbWriteTable(con, "heart", heart_activities, overwrite = TRUE)
      DBI::dbDisconnect(con)
    }

    if(returnData & !returnRawData) return(heart_activities)
    if(!returnData & returnRawData) return(heart_raw_data)
    if(returnData & returnRawData) return(list(heart_activities, heart_raw_data))
  } else{
    warning(sprintf("Did not find any heart rate data for %s. Please check the user's account to verify the start date.", format(as.Date(start.date), "%B %d, %Y")))
    return(NULL)
  }
}
