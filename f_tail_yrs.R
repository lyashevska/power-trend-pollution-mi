# function to generate sequence of tail years
f_tail_yrs <- function(x) {
  if (x > 20) {
    vec <- c(x, 20, 15, 10)
  } else {
    if (x > 15) {
      vec <- c(x, 15, 10)
    } else {
      if (x > 10) {
        vec <- c(x, 10)
      } else {
        vec <- c(x)
      }
    }
  }
  return(vec)
}
