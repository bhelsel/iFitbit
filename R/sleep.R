# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Sleep from the Fitbit API
#' @description Extract sleep outcomes including duration, efficiency, time
#'     asleep, time awake, minutes to fall asleep, and time in bed.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param returnData Return the data to the user's R environment, Default: TRUE
#' @param toSQL Write the data to a SQL database, Default: FALSE
#' @param overwrite Data extraction should be continued from the most recent date in the SQL database, Default: FALSE
#' @return Output is either written to a SQL database or returned as a dataframe
#' @details Extract sleep outcomes including duration, efficiency, time
#'     asleep, time awake, minutes to fall asleep, and time in bed.
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}},
#'  \code{\link[DBI]{dbExistsTable}}, \code{\link[DBI]{dbRemoveTable}},
#'  \code{\link[DBI]{dbDisconnect}}, \code{\link[RSQLite]{SQLite}}
#' @rdname get_fitbit_sleep
#' @export

get_fitbit_sleep <- function(token.pathname, start.date = Sys.Date(),
                             end.date = Sys.Date(), returnData = TRUE,
                             toSQL = FALSE, overwrite = FALSE){

  tkn <- .extract_token(token.pathname)

  start.date <- .adjustDates(token.pathname, database = "sleep", overwrite, start.date, end.date)$start.date

  end.date <- .adjustDates(token.pathname, database = "sleep", overwrite, start.date, end.date)$end.date

  sleep.url <- paste0("https://api.fitbit.com/1.2/user/", tkn$user, sprintf("/sleep/date/%s/%s.json", start.date, end.date))
  sleep <- jsonlite::fromJSON(httr::content(httr::GET(sleep.url, tkn$token), as = "text"))
  data <- data.frame(matrix(ncol = 12, nrow = 0))
  cols <- c("dateOfSleep", "startTime", "endTime", "isMainSleep", "duration", "efficiency", "minutesAfterWakeup", "minutesAsleep", "minutesAwake", "minutesToFallAsleep", "timeInBed", "type")
  colnames(data) <- cols
  if(length(sleep$sleep)!=0){
    data <- sleep[[1]][cols]
    data$duration <- as.numeric(data$duration/3600000) # Convert milliseconds to hours

    add.prefix <- function(x) {paste0(x, ".", colnames(sleep[[1]]$levels$summary[[x]]))}

    if("deep" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["deep"]]) <- add.prefix("deep")}
    if("light" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["light"]]) <- add.prefix("light")}
    if("rem" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["rem"]]) <- add.prefix("rem")}
    if("wake" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["wake"]]) <- add.prefix("wake")}
    if("asleep" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["asleep"]]) <- add.prefix("asleep")}
    if("awake" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["awake"]]) <- add.prefix("awake")}
    if("restless" %in% colnames(sleep[[1]]$levels$summary)){colnames(sleep[[1]]$levels$summary[["restless"]]) <- add.prefix("restless")}

    stages <- Reduce(cbind, sleep[[1]]$levels$summary)
    stages[, grep("thirtyDayAvg", colnames(stages))] <- NULL

    data <- cbind(data, stages)

    data <- dplyr::arrange(data, "dateOfSleep")

  }

  if(toSQL){
    database <- .checkDatabase(tkn$directory, tkn$user)
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    if(nrow(data) != 0){
      if(DBI::dbExistsTable(con, "sleep")){
        DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE dateOfSleep BETWEEN '%s' AND '%s'", "sleep", data$dateOfSleep[1], data$dateOfSleep[nrow(data)]))
      }
    }
    DBI::dbWriteTable(con, "sleep", data, overwrite = overwrite, append = !overwrite)
    DBI::dbDisconnect(con)
  }

  if(returnData){
    return(data)
  }
}

