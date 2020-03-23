# rename_variables_ksao <- function(datum, knowledge=TRUE){
#   
#   # Written by RWS
#   # This function builds off rename_variables, but includes
#   # an argument for knowledge statements, as that brings
#   # in an entirely new scale for analysis.
#   
#   # FLAG: Ensure the ordering of teh variables is always
#   # consistent. Not sure how to accomplish this just yet.
#   
#   # Create renaming scales.
# 
#   
#   if(knowledge==TRUE){
#     
#     if("RvR" %in% Scale_Choices_ksao){
#       StatementNames <- c(StatementNames, "RvR_")
#     }
#     
#     #Knowledge Renamer
#     First_Know_Location  <-
#       which(colnames(datum) == rename_begin_know)
#     Last_Know_Location   <-
#       which(colnames(datum) == rename_end_know)
#     
#     KnowNumbers <- TaskNumbers - SAONumbers
#     
#     Know_NewVariable_Names <- as.vector(outer(StatementNames, (SAONumbers+1):(SAONumbers + KnowNumbers),paste0))
#     colnames(datum)[First_Know_Location:Last_Know_Location] <- Know_NewVariable_Names
#   }   
#   
#   return(datum)
#   
# }