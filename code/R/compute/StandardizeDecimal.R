

StandardizeDecimal <- function(
  number.in,
  n.round) {
  ## make number.in to round to n.round. If resulting number has decimal places
  # smaller than rounded number, add 0 behind. E.g. round(1.999, 2) to be 2.00
  # instead of 2
  number.out <- format(as.numeric(round(number.in, n.round)), nsmall = n.round)
  
  return(number.out)
}#end of StandardizeDecimal()
