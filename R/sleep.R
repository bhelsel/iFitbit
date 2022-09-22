#' @title Extract Sleep from the Fitbit API
#' @description Extract sleep outcomes including duration, efficiency, time
#'     asleep, time awake, minutes to fall asleep, and time in bed.
#' @param token.pathname Path name to the Fitbit API access token.
#' @param start.date The start date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param end.date The end date specified in the format YYYY-MM-DD or today, Default: Sys.Date()
#' @param toSQL Writes the activities to a SQL database stored in the data folder, Default: TRUE
#' @param returnData Returns the sleep data, Default: FALSE
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

get_fitbit_sleep <- function(token.pathname, start.date = Sys.Date(), end.date = Sys.Date(), toSQL = TRUE, returnData = FALSE){
  directory <- dirname(dirname(token.pathname))
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  sleep.url <- paste0("https://api.fitbit.com/1.2/user/", user, sprintf("/sleep/date/%s/%s.json", start.date, end.date))
  sleep <- jsonlite::fromJSON(httr::content(httr::GET(sleep.url, token), as = "text"))
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
  }

  if(toSQL){
    database <- grep(user, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), database)
    if(length(sleep$sleep)!=0){
      DBI::dbWriteTable(con, "sleep", data, overwrite = TRUE)
    }
    if(length(sleep$sleep)==0 & DBI::dbExistsTable(con, "sleep")){
      DBI::dbRemoveTable(con, "sleep")
    }
    if(length(sleep$sleep)==0 & !DBI::dbExistsTable(con, "sleep")){
      DBI::dbWriteTable(con, "sleep", data, overwrite = TRUE)
    }

    DBI::dbDisconnect(con)
  }

  if(returnData){
    return(data)
  }
}

