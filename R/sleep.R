
get.fitbit.sleep <- function(directory, id, start.date = "", end.date = "", toSQL = TRUE, returnData = FALSE){
  token.pathname <- grep(id, list.files(paste0(directory, "/tokens"), full.names = TRUE), value = TRUE)
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
    database <- grep(id, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
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

