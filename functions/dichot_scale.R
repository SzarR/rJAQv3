dichot_scale <- function(datum, scale, yes=1, no=2, rounding=2){

  round((apply(datum[, scale], 2, function(x)
    (sum(x == yes, na.rm = T)) / (sum(x == no, na.rm = T) + sum(x == yes, na.rm =
                                                               T))) * 100), digits = rounding)
  
}