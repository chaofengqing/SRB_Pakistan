

ConvertYYtoYYYY <- function(yy.date) {
  ## convert YY to YYYY format ##
  yy.date <- as.numeric(yy.date)
  
  yyyy.date <- yy.date + 1900
  
  # note: 13 means the year of 2013, will change by time
  if (min(yy.date, na.rm = TRUE) <= 17) {
    yyyy.date <- yy.date + 2000
  }
  
  return(yyyy.date)
}#end of ConvertYYtoYYYY() function
