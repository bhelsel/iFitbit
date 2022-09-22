FitbitAPI.Render.Reports <- function(directory, id){
  Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
  id <- unlist(strsplit(grep(id, list.files(paste0(directory, "/data")), value = TRUE), "[.]"))[1]
  rmarkdown::render(input = "/Users/bhelsel/Desktop/Fitbit API/reports/FitbitReport.Rmd",
                    output_file = paste0("/Users/bhelsel/Desktop/Fitbit API/reports/Individual Reports/", id, ".html"),
                    quiet = TRUE)
}




