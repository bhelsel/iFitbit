FitbitAPI.Render.Reports <- function(directory, id){
  Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
  id <- unlist(strsplit(grep(id, list.files(paste0(directory, "/data")), value = TRUE), "[.]"))[1]
  rmarkdown::render(input = "/Users/bhelsel/Desktop/Fitbit API/reports/FitbitReport.Rmd",
                    output_file = paste0("/Users/bhelsel/Desktop/Fitbit API/reports/Individual Reports/", id, ".html"),
                    quiet = TRUE)
}

FitbitAPI.DropBoxTransfer <- function(){
  zip(zipfile = "/Users/bhelsel/Desktop/Fitbit API/reports/Individual Reports/ChefBoyID_FitbitReports.zip", 
      files = dir("/Users/bhelsel/Desktop/Fitbit API/reports/Individual Reports", full.names = TRUE), flags = "-r9Xj")
  
  file.rename(from = "/Users/bhelsel/Desktop/Fitbit API/reports/Individual Reports/ChefBoyID_FitbitReports.zip",
              to = "/Users/bhelsel/KUMC- CPAWM Dropbox/CPAWM KUMC/ChefBoyID/ChefBoyID_FitbitReports.zip")
  
  unzip(zipfile = "/Users/bhelsel/KUMC- CPAWM Dropbox/CPAWM KUMC/ChefBoyID/ChefBoyID_FitbitReports.zip",
        junkpaths = TRUE, exdir = "/Users/bhelsel/KUMC- CPAWM Dropbox/CPAWM KUMC/ChefBoyID")
  
  file.remove("/Users/bhelsel/KUMC- CPAWM Dropbox/CPAWM KUMC/ChefBoyID/ChefBoyID_FitbitReports.zip")
  
}




