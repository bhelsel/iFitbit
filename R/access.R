# Authorize App and Retrieve Access and Refresh Tokens -----------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param directory PARAM_DESCRIPTION
#' @param study.name PARAM_DESCRIPTION
#' @param study.id PARAM_DESCRIPTION
#' @param fitbit.key PARAM_DESCRIPTION
#' @param fitbit.secret PARAM_DESCRIPTION
#' @param fitbit.callback PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
#' @rdname authorize.bhelselR.fitbit.app
#' @export
#' @importFrom httr add_headers content_type oauth_endpoint oauth_app oauth2.0_token
#' @importFrom RCurl base64Encode
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite

authorize.bhelselR.fitbit.app <- function(directory, study.name, study.id, fitbit.key, fitbit.secret, fitbit.callback){
  header <- httr::add_headers(Authorization=paste0("Basic ", RCurl::base64Encode(charToRaw(paste0(fitbit.key, ":", fitbit.secret)))))
  content_type <- httr::content_type("application/x-www-form-urlencoded")

  # Define scopes to request
  scope <- c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight")

  # Create Endpoint
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  endpoint <- httr::oauth_endpoint(request, authorize, access)

  myapp <- httr::oauth_app("bhelselr", key=fitbit.key, secret=fitbit.secret, redirect_uri=fitbit.callback)

  # Authorize
  token <- httr::oauth2.0_token(endpoint, myapp, scope=scope, config_init=c(header, content_type), cache=FALSE)
  token <- list(timestamp = Sys.time(), token)
  expires.at <- token$timestamp + token[[2]]$credentials$expires_in

  # Save Token
  if(!dir.exists(paste0(directory, "/tokens"))){
    dir.create(paste0(directory, "/tokens"))
  }

  saveRDS(token, file = paste0(directory, "/tokens/", study.name, study.id, ".RData"))
  print(paste0("Saved the access and refresh tokens to: ", directory, ". Access token will expire at ", expires.at))

  # Create a SQL Database to store the data
  if(!dir.exists(paste0(directory, "/data"))){
    dir.create(paste0(directory, "/data"))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(directory, "/data/", study.name, study.id, ".ChefBoyID"))
  DBI::dbDisconnect(con)

  return(token)
}

# Refresh Token -----------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param token.pathname PARAM_DESCRIPTION
#' @param fitbit.key PARAM_DESCRIPTION
#' @param fitbit.secret PARAM_DESCRIPTION
#' @param force.refresh PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{add_headers}}
#'  \code{\link[httr]{content_type}}
#'  \code{\link[httr]{POST}}
#'  \code{\link[httr]{content}}
#'  \code{\link[RCurl]{base64}}
#'  \code{\link[utils]{modifyList}}
#' @rdname refresh.token.bhelselR.fitbit.app
#' @export
#' @importFrom httr add_headers content_type POST content
#' @importFrom RCurl base64Encode
#' @importFrom utils modifyList

refresh.token.bhelselR.fitbit.app <- function(token.pathname, fitbit.key, fitbit.secret, force.refresh = FALSE){
  token <- readRDS(token.pathname)
  if(difftime(token$timestamp + token[[2]]$credentials$expires_in, Sys.time()) <= 5 | force.refresh==TRUE){
    print("Token expired. Refreshing the token now.")
    header <- httr::add_headers(Authorization=paste0("Basic ", RCurl::base64Encode(charToRaw(paste0(fitbit.key, ":", fitbit.secret)))))
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
    token$timestamp <- Sys.time() # Update timestamp
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


