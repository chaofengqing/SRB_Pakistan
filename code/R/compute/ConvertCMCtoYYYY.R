

ConvertCMCtoYYYY <- function(
  cmc.date, # input date should be CMC value
  is.Nepali = FALSE, # default is CMC for Gregorian calendar
  is.Ethiopia = FALSE, # default is CMC for Gregorian calendar
  is.Persian = FALSE, # Afghanistan calendar
  survey.time = NULL
) {
  
  ## convert CMC to YYYY format ##
  
  cmc.date <- as.numeric(as.character(cmc.date))
  # if CMC date is Nepali calendar, should conver to Gregorian CMC value first
  # for the Nepal 1996 survey, the years were recorded using
  # two digit years. E.g. the Nepali year 2052 was recorded
  # as 52.
  if (is.Nepali) {
    if (as.character(survey.time) == "1996") {
      cmc.date <- cmc.date + 519
    } else {
      cmc.date <- cmc.date - 681
    }#end of ifelse(as.character(survey.time) == "1996")
  }#end of if(is.Nepali)
  
  # Ethiopia DHS data use Ethiopian calendar which is 92 months behind the
  # Gregorian calendar.
  if (is.Ethiopia) {
    cmc.date <- cmc.date + 92
  }#end of if(is.Ethiopia)
  
  # convert Gregorian CMC date to Gregorian YYYY date
  if (is.Persian) {
    # Afghanistan 2015 DHS data use Persian calendar which is 621 years behind the
    # Gregorian calendar
    yyyy.date <- cmc.date + 621
  } else {
    yyyy.date <- floor((cmc.date - 1) / 12) + 1900
  }#end of ifelse(is.Persian)
  
  
  return(yyyy.date)
}#end of ConvertCMCtoYYYY() function
