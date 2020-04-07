# Set working directory ----------------------------------------------------
setwd("G:/IOSolutions/Research and Development/rJAQv3/")

# Package requirements ----------------------------------------------------
library(shiny)
library(shinythemes)
library(DT)
library(haven)
library(sjlabelled)
library(tidyverse)
library(stringr)
library(xlsx) #For workbook exports.

# Read in Folder of Custom Functions --------------------------------------
for (i in list.files('./functions/')) {
  source(paste0('./functions/',i))
  rm(i)
}

# Read in Folder of Custom Modules --------------------------------------
for (i in list.files('./modules/')) {
  source(paste0('./modules/',i))
  rm(i)
}

# # Launch the app ----------------------------------------------------------
# runApp(appDir = getwd(),
#        launch.browser = TRUE)

