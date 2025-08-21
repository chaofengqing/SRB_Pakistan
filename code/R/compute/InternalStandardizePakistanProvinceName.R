

InternalStandardizePakistanProvinceName <- function(name.in) {
  
  ## Shorten India state names (and make consistent) ##
  name.in <- ifelse(name.in == "NW Frontier" | name.in == "NWFP" |
                      name.in == "Khyber Pakhtunkhwa", "KPK", paste(name.in))
  
  name.in <- ifelse(name.in == "Islamabad (ICT)", "ICT", paste(name.in))

  name.in <- ifelse(name.in == "Gilgit Baltistan", "GB", paste(name.in))

  name.out <- name.in
  return(name.out)
}#end of InternalStandardizePakistanProvinceName function
