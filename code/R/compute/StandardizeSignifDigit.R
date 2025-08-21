

StandardizeSignifDigit <- function (
  number.in, # input number that you want to standardize
  number.ref # standardize the number.in to have the same number of significant
  # digits as number.ref.
) {
  ## make number.in to have the same number of significant digits as number.ref
  for (power in c(5:1)) {
    if (number.ref %% 10^power == 0) {
      break
    } else {
      power <- 0
    }
  }#end of power loop
  
  number.out <- round(number.in / 10^power) * 10^power
  
  return(number.out)
  
}#end of StandardizeSignifDigit function
