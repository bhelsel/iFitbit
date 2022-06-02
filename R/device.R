#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param directory PARAM_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#'  \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbWriteTable}}, \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname get.fitbit.device
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite

get.fitbit.device <- function(directory, id){
  token.pathname <- grep(id, list.files(paste0(directory, "/tokens"), full.names = TRUE), value = TRUE)
  devicesData <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(devicesData) <- c("user", "deviceVersion", "lastSyncTime", "battery", "batteryLevel")
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  url_devices <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "devices.json")
  device <- jsonlite::fromJSON(httr::content(httr::GET(url_devices, token), as = "text"))
  data <- data.frame(cbind(deviceVersion=device$deviceVersion, lastSyncTime=device$lastSyncTime, battery=device$battery, batteryLevel=device$batteryLevel, type=device$type))
  database <- grep(id, list.files(paste0(directory, "/data"), full.names = TRUE), value = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbWriteTable(con, "device", data, overwrite = TRUE)
  DBI::dbDisconnect(con)
}
