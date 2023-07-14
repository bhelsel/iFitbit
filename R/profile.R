# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Profile from the Fitbit API
#' @description Extract profile information from the Fitbit API
#' @param token.pathname Path name to the Fitbit API access token.
#' @return Returns the Fitbit Profile of the user
#' @details Extract profile information from the Fitbit API
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr]{content}}, \code{\link[httr]{GET}}
#' @rdname get_fitbit_profile
#' @export

get_fitbit_profile <- function(token.pathname){
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  url.profile <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "profile.json")
  profile <- jsonlite::fromJSON(httr::content(httr::GET(url.profile, token), as = "text"))
  return(profile)
}

# magick::image_read displays images
# profile <- get.fitbit.profile(token)
# names.profile <- names(profile$user)
