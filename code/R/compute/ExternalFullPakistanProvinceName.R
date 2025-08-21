

ExternalFullPakistanProvinceName <- function(name.in) {
  
  name.in <- ifelse(name.in == "AJK" | name.in == "Azad Kashmir", "Azad Jammu and Kashmir", paste(name.in))
  name.in <- ifelse(name.in == "Baluchistan", "Balochistan", paste(name.in))
  name.in <- ifelse(name.in == "FATA" | name.in == "F.A.T.A.", "Federally Administered Tribal Areas", paste(name.in))
  name.in <- ifelse(name.in == "Sind", "Sindh", paste(name.in))
  name.in <- ifelse(name.in == "GB" | name.in == "Northern Areas", "Gilgit Baltista", paste(name.in))
  name.in <- ifelse(name.in == "ICT" | name.in == "F.C.T.", "Islamabad (ICT)", paste(name.in))
  name.in <- ifelse(name.in == "KPK" | name.in == "KP" | name.in == "N.W.F.P.", "Khyber Pakhtunkhwa", paste(name.in))
  
  name.out <- name.in
  return(name.out)
}#end of ExternalFullPakistanProvinceName function
