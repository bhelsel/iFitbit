# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Authorize app for Fitbit API use
#' @description Use your Fitbit key and secret to authorize your app for Fitbit API use
#' @param directory Directory location where the access tokens and data should be stored.
#' @inheritParams httr::oauth_app
#' @return Returns the token retrieved from Fitbit API authorization and saves it
#'     to a token folder within the directory. It also creates a data folder and
#'     a SQL database within the directory.
#' @details Use your Fitbit key and secret to authorize your app for Fitbit API use
#' @seealso
#'  \code{\link[httr]{add_headers}}
#'  \code{\link[httr]{content_type}}
#'  \code{\link[httr]{oauth_endpoint}}
#'  \code{\link[httr]{oauth_app}}
#'  \code{\link[httr]{oauth2.0_token}}
#'  \code{\link[RCurl]{base64}}
#'  \code{\link[DBI]{dbConnect}}
#'  \code{\link[DBI]{dbDisconnect}}
#'  \code{\link[RSQLite]{SQLite}}
#' @rdname authorize_fitbit_app
#' @export

authorize_fitbit_app <- function(directory, appname, key, secret = NULL, redirect_uri = httr::oauth_callback()){
  header <- httr::add_headers(Authorization=paste0("Basic ", RCurl::base64Encode(charToRaw(paste0(key, ":", secret)))))
  content_type <- httr::content_type("application/x-www-form-urlencoded")

  # Define scopes to request
  scope <- c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight")

  # Create Endpoint
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  endpoint <- httr::oauth_endpoint(request, authorize, access)

  myapp <- httr::oauth_app(appname=appname, key=key, secret=secret, redirect_uri=redirect_uri)

  # Authorize
  token <- httr::oauth2.0_token(endpoint, myapp, scope=scope, config_init=c(header, content_type), cache=FALSE)
  token <- list(timestamp = Sys.time(), token)
  expires.at <- token$timestamp + token[[2]]$credentials$expires_in

  # Save Token
  if(!dir.exists(paste0(directory, "/tokens"))){
    dir.create(paste0(directory, "/tokens"))
  }
  user_id <- token[[2]]$credentials$user_id
  saveRDS(token, file = paste0(directory, "/tokens/", user_id, ".RData"))
  print(paste0("Saved the access and refresh tokens to: ", directory, ". Access token will expire at ", expires.at))

  # Create a SQL Database to store the data
  if(!dir.exists(paste0(directory, "/data"))){
    dir.create(paste0(directory, "/data"))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(directory, "/data/", user_id, ".db"))
  DBI::dbDisconnect(con)

  return(token)
}



