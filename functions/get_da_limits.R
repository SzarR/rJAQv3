get_da_limits <- function(DA_Limits) {

  # Reads in a vector of type numeric and
  # creates a tibble with duty area start
  # and stop limits for inclusion in final
  # task data XLSX output file.

  if (!is.numeric(DA_Limits)) {
    stop("You cannot include letters in your string!")
  }

  x <- tibble(
    DA_Label = paste0("DA_", 1:length(DA_Limits)),
    Begin = c(1, head(DA_Limits, -1) + 1),
    End = DA_Limits
  )

  x <-
    x %>%
    mutate(Count = (End - Begin)+1)

  return(x)
}