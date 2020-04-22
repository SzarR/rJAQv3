get_demographics <- function(datum) {
  
  # This function takes as an input the task dataframe and
  # as an output runs a for loop through every row of the df
  # demo_task_df and runs the correct analysis.

  #Run task analysis demographics
  if(nrow(demo_task_df) > 0) {
    
    demo_task_df[] <- lapply(demo_task_df, as.character)
    
    # Subset those columns we want to grab means/SD/min/max on.
    demo_task_df_means <-
      demo_task_df %>%
      filter(Analysis_Type == 1) %>%
      dplyr::select(-Analysis_Type) %>%
      add_column(Mean=NA,
                 SD=NA,
                 Median=NA,
                 Minimum=NA,
                 Maximum=NA)
    
    # Subset those columns we want to grab frequencies on.
    demo_task_df_freqs <-
      demo_task_df %>%
      filter(Analysis_Type == 2) %>%
      dplyr::select(-Analysis_Type) %>%
      add_column(Frequency=NA,
                 Percentage=NA)
    
    # Final output graphic. To be sent to table output 
    print_list <- list()
    temp_list <- list()
    
    #Run for loop to create the various tables.
    if(nrow(demo_task_df_means) > 0) {
    for (i in 1:nrow(demo_task_df_means)) {
  
        # Get column value.
        Column <- demo_task_df_means[i,'Column_Name']

        # Run stats.
        demo_task_df_means[i,'Mean'] <- colMeans(x = datum[,Column],na.rm = TRUE)
        demo_task_df_means[i,'SD'] <- apply(datum[,Column],2, sd, na.rm=TRUE)
        demo_task_df_means[i,'Median'] <- apply(datum[,Column],2, median, na.rm=TRUE)
        demo_task_df_means[i,'Minimum'] <- min(datum[,Column],na.rm = TRUE)
        demo_task_df_means[i,'Maximum'] <- max(datum[,Column],na.rm = TRUE)
        
        # Output.
        print_list[[1]] = demo_task_df_means
    }
    }
    
    if (nrow(demo_task_df_freqs) > 0) {
      # Create this list as a placeholder.
      temp_list = list()
      
    for (i in 1:nrow(demo_task_df_freqs)) {

        # Get column value
        Column <- demo_task_df_freqs[i,'Column_Name']

        temp_list[[i]] <- 
          summarytools::freq(datum[,Column])
    }
    }
    
    print_list = c(print_list, temp_list)
    
    return(print_list)

  }

}