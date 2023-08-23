# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Refresh Fitbit API Token
#' @description Refresh the Fitbit API Token after it expires to access Fitbit API
#' @param token.pathname Path name to the token that needs to be refreshed.
#' @inheritParams httr::oauth_app
#' @param force.refresh Force the refresh token when it has not expired, Default: FALSE
#' @return Refreshes the token retrieved from Fitbit API authorization and saves it to directory.
#' @details Refresh the Fitbit API Token after it expires to access Fitbit API
#' @seealso
#'  \code{\link[httr]{add_headers}}
#'  \code{\link[httr]{content_type}}
#'  \code{\link[httr]{POST}}
#'  \code{\link[httr]{content}}
#'  \code{\link[RCurl]{base64}}
#'  \code{\link[utils]{modifyList}}
#' @rdname refresh_fitbit_token
#' @export

refresh_fitbit_token <- function(token.pathname, key, secret = NULL, force.refresh = FALSE){
  token <- readRDS(token.pathname)
  if(difftime(token$timestamp + token[[2]]$credentials$expires_in, Sys.time()) <= 1 | force.refresh==TRUE){
    print("Token expired. Refreshing the token now.")
    header <- httr::add_headers(Authorization=paste0("Basic ", RCurl::base64Encode(charToRaw(paste0(key, ":", secret)))))
    content_type <- httr::content_type("application/x-www-form-urlencoded")
    if("errors" %in% names(token[[2]]$credentials)){
      token[[2]]$credentials$errors <- NULL
      token[[2]]$credentials$success <- NULL
    }
    response <- httr::POST("https://api.fitbit.com/oauth2/token", encode = "form",
                           body = list(grant_type = "refresh_token", refresh_token = token[[2]]$credentials$refresh_token),
                           config=c(header, content_type))
    
    refresh.data <- httr::content(response)
    new.credentials <- utils::modifyList(token[[2]]$credentials, refresh.data)
    token[[2]]$credentials <- new.credentials # Update access and refresh tokens
    token$timestamp <- Sys.time() # Update time stamp
    saveRDS(token, file = token.pathname)
    expires.at <- token$timestamp + token[[2]]$credentials$expires_in
    print(paste0("Saved the new token. The new token will expire at ", expires.at))
    return(token)
  }
  
  if(token$timestamp + token[[2]]$credentials$expires_in > Sys.time()){
    expires.at <- token$timestamp + token[[2]]$credentials$expires_in
    time.left <- round(difftime(expires.at, Sys.time(), units = "secs"))
    print(paste0("Token has not expired, but will be expiring in ", sprintf("%s seconds at %s", time.left, expires.at)))
  }
}
