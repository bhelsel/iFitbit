# iFitbit 0.1.4
* Update license to include University of Kansas copyright information.
* Update `get_fitbit_exercise_log` function to check for NULL values.
* Change toCSV to toXLSX in the `get_fitbit_report` so that the tables in the SQL database can be written to separate Excel sheets.
* Provide an option to return device information to the users R environment

# iFitbit 0.1.3
* Add a method to `get_fitbit_activities` that separates Fitbit API calls for time periods greater than 100 days.
* Removed a merge call in `get_fitbit_heart_intraday` that didn't work for an incomplete day of heart rate data.
* Add a start and end date argument to `get_fitbit_report` to allow filtering of the data when `toCSV` or `returnData` is TRUE

# iFitbit 0.1.2

* Added distance, log type, activity level, and heart zones to exercise log
* Added a vignette for setting up a Fitbit application
* Added GitHub actions for R-CMD-Check
* Added a sample SQL database containing Fitbit data
* Updated the README with detailed text and examples
* Added toCSV and returnData arguments to `get_fitbit_report`


# iFitbit 0.1.1

* Added the license information
* Made the arguments for all of the iFitbit functions more consistent
* Added documentation and updated the namespace files
* Added global variables

# iFitbit 0.1.0

* iFitbit initial commit
