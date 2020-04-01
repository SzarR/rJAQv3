likert_scale <- function(datum, scale, rounding=2){
  
  round(colMeans(datum[, scale], na.rm = T), digits = rounding)
  
}