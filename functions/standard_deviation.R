standard_deviation <- function(datum, rounding=2, scale){
  
  round(sapply(datum[, scale], sd, 2), digits = rounding)
  
}