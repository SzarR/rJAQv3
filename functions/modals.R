# Scale Selection FYI for Tasks -------------------------------------------

make_alert <- function(title = title, 
                       text = text, 
                       type = type,
                       closeOnEsc = TRUE,
                       closeOnClickOutside = TRUE,
                       showConfirmButton = TRUE,
                       animation = TRUE) {

  shinyalert::shinyalert(title = title,
                         text = text,
                         type = type)
  
}
