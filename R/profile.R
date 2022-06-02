
# magick::image_read displays images
get.fitbit.profile <- function(token.pathname){
  id <- gsub(".RData", "", basename(token.pathname))
  token <- readRDS(token.pathname)
  token <- token[[2]]
  user <- token$credentials$user_id
  url.profile <- paste0("https://api.fitbit.com/1/", "user/", user, "/", "profile.json")
  profile <- jsonlite::fromJSON(httr::content(httr::GET(url.profile, token), as = "text"))
  return(profile)
}

# 
# profile <- get.fitbit.profile(token)
# names.profile <- names(profile$user)
