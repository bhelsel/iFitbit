# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title get_fitbit_heart_intraday
#' @description Retreives a continuous measure of heart rate from the Fitbit API
#' @param token.pathname Full pathname to the location of the access token
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param detail.level Detail level of the data points that should be included.
#'  The detail level can be '1sec', '1min', '5min', or '15min', Default: '1min'
#' @param age Age of the participant (extracts from the Fitbit user's profile if NULL), Default: NULL
#' @param max_hr_formula An age-based formula to use to calculate maximal heart rate, Default: '220 - age'
#' @param intensity Heart rate percentages expressed as decimal points to use when calculating
#'  intensity minutes (uses ACSM as default if NULL), Default: NULL
#' @param intensity_labels Labels to go with the `intensity` percentages (uses ACSM default
#'  of sedentary, very light, light, moderate, vigorous, and near maximal if NULL), Default: NULL
#' @param heart_rate_method A numeric value of 1, 2, or 3 to specify the approach to calculating heart
#'  rate intensities (see details for more information), Default: 1
#' @param rest.hr A constant resting heart rate to use with `heart_rate_method` = 2 if it
#'  is a measure collected by the study, Default: NULL
#' @param do.parallel Process dates in parallel to speed up extraction of the heart rate data
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

get_fitbit_heart_intraday <- function(token.pathname, start.date = Sys.Date(),
                                      end.date = Sys.Date(), detail.level = "1min",
                                      age = NULL, max_hr_formula = "220 - age",
                                      intensity = NULL, intensity_labels = NULL,
                                      heart_rate_method = 1, rest.hr = NULL, do.parallel = FALSE,
                                      returnData = TRUE, returnRawData = FALSE,
                                      toSQL = FALSE, verbose = FALSE){

  if(!equals(length(intensity), length(intensity_labels))) {
    stop("Heart intensities and their labels must be the same length")
  }

  tkn <- .extract_token(token.pathname)

  # Device
  device <- get_fitbit_device(token.pathname, returnData = TRUE)
  lastSync <- as.POSIXct(device$lastSyncTime, format = "%Y-%m-%dT%H:%M:%OS")
  if(end.date > Sys.Date()) end.date <- Sys.Date()

  # Age
  if(is.null(age)) age <- get_fitbit_profile(token.pathname)$user$age

  # Sleep
  sleep <- get_fitbit_sleep(token.pathname, start.date = start.date, end.date = end.date)
  sleep <- sleep[, c("startTime", "endTime")]

  # Raw Heart Rate Data
  heart_raw_data <- extract_heart_intraday(token.pathname, start.date, end.date, detail.level,
                                           imputeZeros = TRUE, do.parallel = do.parallel, verbose = verbose)

  if(nrow(heart_raw_data)==0) stop("No raw heart rate data was found.")

  heart_data <- calculate_heart_intensities(heart_raw_data, sleep, age, max_hr_formula, intensity,
                                            intensity_labels, heart_rate_method, rest.hr, verbose)

  if(toSQL){
    database <- .checkDatabase(directory, tkn$user)
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    DBI::dbWriteTable(con, "heart", heart_data, overwrite = TRUE)
    DBI::dbDisconnect(con)
  }

  if(!returnRawData & returnData) return(heart_data)
  if(returnRawData & !returnData) return(heart_raw_data)
  if(returnRawData & returnData) return(list(heart_data, heart_raw_data))

}


#' @title extract_heart_intraday
#' @description Extracts a continuous measure of heart rate data from the Fitbit API
#' @details This function retrieves a continuous measure of heart rate from the Fitbit API
#' at the 1 second, 1 minute, 5 minute, or 15 minute intervals.
#' @return A data set with time and heart rate variables at the specified interval
#' @inheritParams get_fitbit_heart_intraday
#' @param imputeZeros Adds zeros to the heart rate variable for missing data

extract_heart_intraday <- function(token.pathname, start.date, end.date,
                                   detail.level, imputeZeros = FALSE,
                                   do.parallel = FALSE, verbose = FALSE){

  heart_raw_data <- data.frame()

  dates <- as.character(seq.Date(as.Date(start.date), as.Date(end.date), by = "day"))

  main_extract <- function(token.pathname, date, imputeZeros = FALSE, verbose = FALSE){
    tkn <- .extract_token(token.pathname)
    Date <- as.Date(date)
    if(verbose) print(sprintf("Extracting Fitbit Heart Rate Intraday Data for %s on %s", tkn$user, format(Date, "%B %d, %Y")))
    heart.url <- sprintf("https://api.fitbit.com/1/user/%s/activities/heart/date/%s/1d/%s.json", tkn$user, Date, detail.level)
    heart <- jsonlite::fromJSON(httr::content(httr::GET(heart.url, tkn$token), as = "text"))
    if(!is.null(heart$errors)) stop(heart$errors$message)
    if(length(heart$`activities-heart-intraday`$dataset) != 0){
      heart_data <- heart$`activities-heart-intraday`$dataset %>% `colnames<-`(c("time", "hr"))
      heart_data$time <- as.POSIXct(paste(Date, heart_data$time)) %>% lubridate::force_tz("UTC")
      start_time <- as.POSIXct(paste(Date, "00:00:00"), tz = "UTC")
      stop_time <- as.POSIXct(paste(Date, "23:59:59"), tz = "UTC")
      if(imputeZeros) heart_data %<>% tidyr::complete(time = seq.POSIXt(start_time, stop_time, by = "min"), fill = list(hr = 0))
      return(heart_data)
    }
  }

  if(do.parallel){
    cores = parallel::detectCores()
    Ncores = cores - 1
    cl = parallel::makeCluster(Ncores)
    doParallel::registerDoParallel(cl)
    `%dopar%` = foreach::`%dopar%`
    heart_raw_data <- foreach::foreach(i = dates, .packages = c("iFitbit", "magrittr", "dplyr", "httr", "tidyr", "jsonlite")) %dopar% {
      heart_data <- main_extract(token.pathname, date = i, imputeZeros = imputeZeros, verbose = verbose)
    }
    heart_raw_data <- dplyr::bind_rows(heart_raw_data)
    parallel::stopCluster(cl)
  } else{
    for(i in dates){
      heart_data <- main_extract(token.pathname, date = i, imputeZeros = imputeZeros, verbose = verbose)
      heart_raw_data <- plyr::rbind.fill(heart_raw_data, heart_data)
    }
  }

  return(heart_raw_data)

}


#' @title calculate_heart_intensities
#' @description Calculates heart rate intensities from raw heart rate data
#' @details This function creates heart rate intensities from raw heart rate data.
#' Sleep time stamps can be passed to remove sleep data from the intensity classification
#' and customized intensities and intensity labels can be specified if the user chooses
#' not to use the default ACSM maximal heart rate or heart rate reserve intensities.
#' @return A data set with sleep, nonwear, and intensity classifications.
#' @inheritParams get_fitbit_heart_intraday
#' @param heart_raw_data Raw heart rate data with time stamp and heart rate columns
#' @param sleep_data Sleep data set with startTime and endTime POSIXt time stamps as the columns.

calculate_heart_intensities <- function(heart_raw_data = NULL, sleep_data = NULL,
                                        age = NULL, max_hr_formula = "220 - age",
                                        intensity = NULL, intensity_labels = NULL,
                                        heart_rate_method = 1, rest.hr = NULL, verbose = FALSE){

  if(!equals(length(intensity), length(intensity_labels))) {
    stop("Heart intensities and their labels must be the same length")
  }

  start.date <- min(as.Date(heart_raw_data$time))
  end.date <- max(as.Date(heart_raw_data$time))

  # Calculate max heart rate
  max.hr <- round(eval(parse(text = max_hr_formula)))

  # Extract resting heart rate
  if(heart_rate_method == 1){
    rest.hr <- min(heart_raw_data[heart_raw_data$hr > 0, "hr"])
  }

  if(max(heart_raw_data$hr) > max.hr) max.hr <- max(heart_raw_data$hr)

  if(heart_rate_method == 2 & is.null(rest.hr)) stop("Expecting resting heart rate to be passed as an argument")

  # Define heart rate intensities
  if(is.null(intensity_labels)) intensity_labels <- c("sedentary", "very.light", "light", "moderate", "vigorous", "near.maximal")

  if(heart_rate_method %in% 1:2 & is.null(intensity)) {
    intensities <- c(0, 0.20, 0.30, 0.40, 0.60, 0.90, 1) # Heart Rate Reserve
    heart_categories <- do.call("c", lapply(intensities, function(x) ((max.hr - rest.hr) * x) + rest.hr))
  }

  if(heart_rate_method == 3 & is.null(intensity)) {
    intensities <- c(0, 0.40, 0.57, 0.64, 0.77, 0.96, 1) # Max HR
    heart_categories <- do.call("c", lapply(intensities, function(x) (max.hr * x)))
  }

  # Add resting heart rate
  heart_raw_data$rest.hr <- rest.hr

  # Add sleep
  if(!is.null(sleep_data)){
    if(nrow(sleep_data)!=0){
      sleepStartTime <- lubridate::round_date(as.POSIXct(sleep_data$startTime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), "minute")
      sleepEndTime <- lubridate::round_date(as.POSIXct(sleep_data$endTime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), "minute")
      sleepTimes = c()
      for(n in 1:length(sleepStartTime)){
        ts <- seq.POSIXt(from = sleepStartTime[n], to = sleepEndTime[n], by = "min")
        sleepTimes <- sort(append(sleepTimes, ts))
      }
      heart_raw_data$sleep <- ifelse(heart_raw_data$time %in% sleepTimes, 1, 0)
    } else{
      heart_raw_data$sleep <- 0
    }
  }

  # Add Nonwear and Intensity
  heart_raw_data$hrr.percent <- cut(heart_raw_data$hr, breaks = heart_categories, labels = intensity_labels, include.lowest = TRUE, right = FALSE)
  heart_raw_data$nonwear <- ifelse(heart_raw_data$hr == 0, 1, 0)
  if(!is.null(sleep_data)) {
    heart_raw_data$sleep <- ifelse(heart_raw_data$sleep == 1 & heart_raw_data$nonwear == 0, 1, 0)
    heart_raw_data[which(heart_raw_data$sleep == 1), "hrr.percent"] <- NA
    }

  heart_raw_data[which(heart_raw_data$nonwear == 1), "hrr.percent"] <- NA
  heart_raw_data <- dplyr::bind_cols(cbind(dplyr::select(heart_raw_data, time:nonwear), sapply(intensity_labels, function(x) heart_raw_data[x] <- dplyr::if_else(heart_raw_data$hrr.percent == x, 1, 0, 0))))
  cols <- names(heart_raw_data)[names(heart_raw_data) %in% c("sleep", "nonwear", intensity_labels)]
  heart_data <- heart_raw_data %>% dplyr::group_by(date = as.character(as.Date(time))) %>% dplyr::summarise_at(vars(dplyr::all_of(cols)), sum, na.rm = TRUE)

  # Calculate MVPA if not already provided in intensities
  if(!any(grepl("mvpa", colnames(heart_data), ignore.case = TRUE))){
    mod_pos <- min(grep("mod", colnames(heart_data), ignore.case = TRUE))
    heart_data$mvpa <- rowSums(heart_data[, mod_pos:ncol(heart_data)])
  }

  if(verbose & is.null(intensity)) print(.print_heart_intensity(heart_rate_method))

  if(nrow(heart_data) == 0){
    warning("Did not find any heart rate data. Please check the user's account to verify the dates.")
    return(NULL)
  }

  return(heart_data)
}

