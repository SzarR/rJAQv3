get_importance <- function(datum){

  if("Importance" %in% Scale_Choices){

    Task_IMP_Sum <- colMeans(datum[,paste0("IMP_", 1:TaskNumbers)][1:nrow(TaskNumbers)], na.rm=T)
    Task_IMP_SD <- sapply(datum[,paste0("IMP_", 1:TaskNumbers)][1:nrow(TaskNumbers)], sd, 2)
    
    #Statements_Task <- cbind(Statements_Task, Task_IMP_Sum, Task_IMP_SD)
    
    return(Task_IMP_Sum)

  }
  
}