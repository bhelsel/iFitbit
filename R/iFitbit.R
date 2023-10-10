# Copyright © 2022 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title iFitbit: R Package for Extracting Data from the Fitbit API and Generating Interactive Reports
#'
#' @description This R Package extracts data from the Fitbit API and generates
#'     interactive reports, tables, and visualizations.
#'
#' @section iFitbit functions:
#'
#' \code{\link{authorize_fitbit_app}}
#'
#' \code{\link{refresh_fitbit_token}}
#'
#' \code{\link{get_fitbit_activities}}
#'
#' \code{\link{get_fitbit_exercise_log}}
#'
#' \code{\link{get_fitbit_sleep}}
#'
#' \code{\link{get_fitbit_profile}}
#'
#' \code{\link{get_fitbit_device}}
#'
#' \code{\link{get_fitbit_heart_intraday}}
#'
#' @docType package
#' @name iFitbit
#' @import magrittr
#' @import ggplot2
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET POST add_headers content_type oauth_endpoint oauth_app oauth2.0_token
#' @importFrom DBI dbConnect dbWriteTable dbReadTable dbExistsTable dbRemoveTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom RCurl base64Encode
#' @importFrom utils modifyList tail menu
#' @importFrom rmarkdown render
#' @importFrom dplyr distinct case_when summarise_at select desc mutate_if top_n mutate arrange rename bind_cols if_else summarise_all
#' @importFrom reactable colDef colGroup reactable reactableTheme
#' @importFrom purrr as_vector
#' @importFrom tidyr pivot_longer complete
#' @importFrom plotly ggplotly subplot
#' @importFrom plyr rbind.fill
#' @importFrom gridExtra grid.arrange
#' @importFrom graphics layout
#' @importFrom glue glue glue_collapse
#' @importFrom rlang inform
#' @importFrom lubridate force_tz
NULL
