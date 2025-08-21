
GetIndiaStateCode <- function(name.in) {
  
  ## get India state code names ##
  code.out <- rep(NA, length(name.in))
  
  code.out <- ifelse(name.in == "Andhra Pradesh" | name.in == "former state of Andhra Pradesh", "AP", paste(code.out))
  code.out <- ifelse(name.in == "Arunachal Pradesh", "AR", paste(code.out))
  code.out <- ifelse(name.in == "Assam", "AS", paste(code.out))
  code.out <- ifelse(name.in == "Bihar", "BH", paste(code.out))
  code.out <- ifelse(name.in == "Chhattisgarh", "CH", paste(code.out))
  code.out <- ifelse(name.in == "Delhi", "DL", paste(code.out))
  code.out <- ifelse(name.in == "Gujarat", "GJ", paste(code.out))
  code.out <- ifelse(name.in == "Goa", "GO", paste(code.out))
  code.out <- ifelse(name.in == "Himachal Pradesh", "HP", paste(code.out))
  code.out <- ifelse(name.in == "Haryana", "HR", paste(code.out))
  code.out <- ifelse(name.in == "Jharkhand", "JH", paste(code.out))
  code.out <- ifelse(name.in == "Jammu and Kashmir", "JM", paste(code.out))
  code.out <- ifelse(name.in == "Karnataka", "KA", paste(code.out))
  code.out <- ifelse(name.in == "Kerala", "KE", paste(code.out))
  code.out <- ifelse(name.in == "Meghalaya", "MG", paste(code.out))
  code.out <- ifelse(name.in == "Maharashtra", "MH", paste(code.out))
  code.out <- ifelse(name.in == "Manipur", "MN", paste(code.out))
  code.out <- ifelse(name.in == "Madhya Pradesh", "MP", paste(code.out))
  code.out <- ifelse(name.in == "Mizoram", "MZ", paste(code.out))
  code.out <- ifelse(name.in == "Nagaland", "NA", paste(code.out))
  code.out <- ifelse(name.in == "Orissa", "OR", paste(code.out))
  code.out <- ifelse(name.in == "Punjab", "PJ", paste(code.out))
  code.out <- ifelse(name.in == "Rajasthan", "RJ", paste(code.out))
  code.out <- ifelse(name.in == "Sikkim", "SK", paste(code.out))
  code.out <- ifelse(name.in == "Tamil Nadu", "TN", paste(code.out))
  code.out <- ifelse(name.in == "Tripura", "TR", paste(code.out))
  code.out <- ifelse(name.in == "Uttarakhand", "UT", paste(code.out))
  code.out <- ifelse(name.in == "Uttar Pradesh", "UP", paste(code.out))
  code.out <- ifelse(name.in == "West Bengal", "WB", paste(code.out))
  
  return(code.out)
}#end of GetIndiaStateCode function
