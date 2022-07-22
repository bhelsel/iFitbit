####### Add part to extract last resting heart rate from file, if needed. Need to add resting heart rate to heart files.
### This will be useful if not exporting all the date range to make it less likely to assign a default value.


get.fitbit.heart.intraday <- function(directory, id, start.date = "", end.date = "", detail.level = "1min"){
  token.pathname <- grep(id, list.files(paste0(directory, "/tokens"), full.names = TRUE), value = TRUE)
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  database <- grep(id, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)

  if(DBI::dbExistsTable(con, "heart")){
    currentData <- DBI::dbReadTable(con, "heart")
    currentData <- dplyr::distinct(currentData)
    currentData <- currentData[1:(nrow(currentData)-8), ]
    start.date <- as.character(as.Date(utils::tail(currentData$date, 1)) + 1)
  }

  # Calculate max heart rate using a Down syndrome specific equation
  profile <- get.fitbit.profile(token.pathname)
  age <- profile$user$age
  max.hr <- round(210 - (0.56 * age) - 31)
  # Create a vector of dates from start to end date
  dates <- format(seq.POSIXt(as.POSIXct(start.date, format = "%Y-%m-%d", tz = "UTC"), as.POSIXct(end.date, format = "%Y-%m-%d", tz = "UTC"), by = "day"), "%Y-%m-%d")

  # Create an empty data frame to bind rows
  heartActivities <- data.frame(matrix(ncol=10, nrow=0))
  colnames(heartActivities) <- c("date", "sleep", "nonwear", "sedentary", "very.light", "light", "moderate", "vigorous", "near.maximal", "mvpa")

  # Add file if file exists
  if(exists("currentData")){
    heartActivities <- rbind(heartActivities, currentData)
  }

  # Extract sleep date
  sleep <- get.fitbit.sleep(directory = directory, id = id, start.date = start.date, end.date = end.date, toSQL = FALSE, returnData = TRUE)

  # Loop through the days to pull values from the heart rate intraday data and calculate intensities
  for(date in dates){
    `%>%` <- dplyr::`%>%`
    print(paste0("Extracting Fitbit Heart Rate Intraday Data for ", id, " on ", date))
    heart.url <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "activities/heart", sprintf("/date/%s/1d/%s.json", date, detail.level))
    datetime <- seq.POSIXt(as.POSIXct(paste(date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(paste(date, "23:59:59"), format = "%Y-%m-%d %H:%M:%S"), "min")
    datetime <- format(datetime, "%Y-%m-%d %H:%M:%S")
    data <- data.frame(datetime)
    heart <- jsonlite::fromJSON(httr::content(httr::GET(heart.url, token), as = "text"))

    # Calculate intensity by heart rate reserve: ACSM Guidelines for Exercise Testing and Prescription, 11 Edition (page 148, table 5.2)
    if(is.null(heart[[1]]$value$restingHeartRate) & exists("rest.hr")==TRUE){
      print(paste0("No resting heart rate available for ", format(as.Date(date), "%B %d, %Y"),
                   ". The resting heart rate data from ", format(as.Date(date)-1, "%B %d, %Y"), " will be used."))
    }

    if(is.null(heart[[1]]$value$restingHeartRate)==FALSE){
      rest.hr <- heart[[1]]$value$restingHeartRate
    }

    if(exists("rest.hr")==FALSE){
      rest.hr <- 60
      print("No resting heart rate found. Using a default of 60 bpm for the resting heart rate.")
    }

    calculateHRR <- function(max.hr, rest.hr, p){((max.hr - rest.hr)*p)+rest.hr}
    hrr20 <- calculateHRR(max.hr, rest.hr, 0.20)
    hrr30 <- calculateHRR(max.hr, rest.hr, 0.30)
    hrr40 <- calculateHRR(max.hr, rest.hr, 0.40)
    hrr60 <- calculateHRR(max.hr, rest.hr, 0.60)
    hrr90 <- calculateHRR(max.hr, rest.hr, 0.90)

    if(length(heart$`activities-heart-intraday`$dataset)!=0){
      data <- merge(x = data, y = data.frame(datetime = paste(date, heart$`activities-heart-intraday`$dataset$time), hr.bpm = heart$`activities-heart-intraday`$dataset$value), by = "datetime", all.x = TRUE)

      # Add sleep
      if(nrow(sleep)!=0){
        SleepStartTime <- as.POSIXct(sleep[sleep$dateOfSleep==date, c("startTime")], format = "%Y-%m-%dT%H:%M:%OS")
        SleepEndTime <- as.POSIXct(sleep[sleep$dateOfSleep==date, c("endTime")], format = "%Y-%m-%dT%H:%M:%OS")
        data$sleep <- 0

        if(length(SleepStartTime)!=0){
          for(i in 1:length(SleepStartTime)){
            if(format(SleepStartTime[i], "%Y-%m-%d") != date){
              SleepStartTime[i] == as.POSIXct(date, format = "%Y-%m-%d")
            }
            data$sleep <- ifelse(data$datetime>=SleepStartTime[i] & data$datetime<=SleepEndTime[i], 1, data$sleep)
          }
        }
      }

      if(nrow(sleep)==0){
        data$sleep <- 0
      }

      # Add Nonwear and Intensity
      data$hrr.percent <- dplyr::case_when(data$sleep==1 ~ "Sleep",
                                           is.na(data$hr.bpm) & data$sleep!=1 ~ "Nonwear",
                                           data$hr.bpm < hrr20 & data$sleep!=1 ~ "Sedentary (< 20% HRR)",
                                           data$hr.bpm >= hrr20 & data$hr.bpm < hrr30 & data$sleep!=1 ~ "Very Light (20-29% HRR)",
                                           data$hr.bpm >= hrr30 & data$hr.bpm < hrr40 & data$sleep!=1 ~ "Light (30-39% HRR)",
                                           data$hr.bpm >= hrr40 & data$hr.bpm < hrr60 & data$sleep!=1 ~ "Moderate (40-59% HRR)",
                                           data$hr.bpm >= hrr60 & data$hr.bpm < hrr90 & data$sleep!=1 ~ "Vigorous (60-89% HRR)",
                                           data$hr.bpm >= hrr90 & data$sleep!=1 ~ "Near Maximal (≥ 90% HRR)")

      data$nonwear <- ifelse(data$hrr.percent=="Nonwear", 1, 0)
      data$sedentary <- ifelse(data$hrr.percent=="Sedentary (< 20% HRR)", 1, 0)
      data$very.light <- ifelse(data$hrr.percent=="Very Light (20-29% HRR)", 1, 0)
      data$light <- ifelse(data$hrr.percent=="Light (30-39% HRR)", 1, 0)
      data$moderate <- ifelse(data$hrr.percent=="Moderate (40-59% HRR)", 1, 0)
      data$vigorous <- ifelse(data$hrr.percent=="Vigorous (60-89% HRR)", 1, 0)
      data$near.maximal <- ifelse(data$hrr.percent=="Near Maximal (≥ 90% HRR)", 1, 0)
      data$mvpa <- ifelse(data$hrr.percent=="Moderate (40-59% HRR)" | data$hrr.percent=="Vigorous (60-89% HRR)" | data$hrr.percent=="Near Maximal (≥ 90% HRR)", 1, 0)
      heartActivities <- rbind(heartActivities, cbind(date = date, data %>% dplyr::summarise_at(c("sleep", names(dplyr::select(data, nonwear:mvpa))), sum, na.rm = TRUE)))
    }

    # Run this code if the participant does not have intraday data
    if(length(heart$`activities-heart-intraday`$dataset)==0){
      data <- data.frame(matrix(ncol=9, nrow=1))
      data[1, ] <- c(0, 1440, rep(0, 7))
      colnames(data) <- c("sleep", "nonwear", "sedentary", "very.light", "light", "moderate", "vigorous", "near.maximal", "mvpa")
      heartActivities <- rbind(heartActivities, cbind(date = date, data))
      print(paste0("Could not find intraday heart rate date for ", format(as.Date(date), format = "%B %d, %Y")))
    }
  }

  DBI::dbWriteTable(con, "heart", heartActivities, overwrite = TRUE)
  DBI::dbDisconnect(con)

}