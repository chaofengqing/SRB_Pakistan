###############################################################################
# Levels and trends estimate of sex ratio at birth for seven provinces of Pakistan 
# from 1980 to 2020 with scenario-based probabilistic projections 
# of missing female birth to 2050: A Bayesian modeling approach
#
# Code constructed by: Fengqing CHAO
# Code last revised by: Qiqi Qiang on 29 Aug 2025
# 
# ExternalFullPakistanProvinceName.R
# 
# This script contains a utility function standardizes Pakistani province names 
# by converting abbreviations or old spellings into their full official forms.
# 
# Functions are: function1(.., function2(3), ..); means function2 is called
# three times inside function1.
# ExternalFullPakistanProvinceName(..)
# 
#################################################

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
} # end of ExternalFullPakistanProvinceName function
