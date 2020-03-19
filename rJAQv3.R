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
library(xlsx)

# Read in Folder of Custom Functions --------------------------------------
for (i in list.files('./functions/')) {
  source(paste0('./functions/',i))
  rm(i)
}

