# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract device information from the Fitbit API
#' @description Extract device, last sync time, and battery level from the Fitbit API.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param toSQL Write the device information to a SQL database, Default: TRUE
#' @param returnDevice Return the device information to the users R environment, Default: FALSE
#' @return Writes the device information to a SQL database stored in the data folder.
#' @details Extract device, last sync time, and battery level from the Fitbit API.
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get_fitbit_device
#' @export

get_fitbit_device <- function(token.pathname, toSQL = TRUE, returnDevice = FALSE){
  directory <- dirname(dirname(token.pathname))
  devicesData <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(devicesData) <- c("user", "deviceVersion", "lastSyncTime", "battery", "batteryLevel")
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  url_devices <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "devices.json")
  device <- jsonlite::fromJSON(httr::content(httr::GET(url_devices, token), as = "text"))

  if(length(device) != 0){
    data <- data.frame(cbind(deviceVersion=device$deviceVersion, lastSyncTime=device$lastSyncTime, battery=device$battery, batteryLevel=device$batteryLevel, type=device$type))
  } else{
    data <- data.frame(cbind(deviceVersion=NA, lastSyncTime=NA, battery=NA, batteryLevel=NA, type=NA))
  }

  database <- grep(user, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)

  if(toSQL){
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    DBI::dbWriteTable(con, "device", data, overwrite = TRUE)
    DBI::dbDisconnect(con)
  }

  if(returnDevice){
    return(device)
  }

}
