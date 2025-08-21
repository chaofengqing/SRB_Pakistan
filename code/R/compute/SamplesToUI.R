

SamplesToUI <- function (
  samples.jt, # inputs, note that it also works for samples.j
  NA.jt = 0, # indicate which entries should be set to NA. If NULL, it's not used.
  percentile.q = percentiles # output percentiles for input
) {
  # output: result.qt
  
  if (is.vector(samples.jt)) {
    result.qt <- quantile(samples.jt + NA.jt, probs = percentile.q, na.rm = TRUE)
  }
  if (is.matrix(samples.jt)) {
    result.qt <- apply(samples.jt + NA.jt, 2, quantile, percentile.q, na.rm = TRUE)
  }
  
  return (result.qt)
  
}#end of SamplesToUI function
