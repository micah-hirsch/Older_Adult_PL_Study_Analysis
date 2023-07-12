# Data Cleaning and Processing

# Author: Micah E. Hirsch
# Date: 7/7/2023

# R Version 4.3.1
# Purpose: To prepare data for analysis

# Packages

library(tidyverse) #install.packages("tidyverse")
library(rio) #install.packages("rio")
library(remotes) #install.packages("remotes")
library(autoscore) #remotes::install_github("autoscore/autoscore")
library(SnowballC) #install.packages("SnowballC")

# Importing Participant Data and Calculating Number of Words in Target Phrase

## Specifying the path two the folders where our listener data is located
file_path <- c("Raw Data/Participant Data/AM1", "Raw Data/Participant Data/PDM10")

## Creating an empty list to temporarily hold dfs created in the loop
data_list <- list()

## Initiating Loop

for (path in file_path) {
  ## Specifying to look for the csv files in the folder
  file_names <- list.files(path, pattern = ".csv$", full.names = T)
  
  ### Looping through each file name
  for (file_name in file_names) {
    
    ### Importing Data
    data <- rio::import(file_name) %>%
      ### Creating Speaker, Phrase, and Target Number Variables
      dplyr::mutate(speaker = basename(path),
                    phrase = paste(strsplit(File,"_")[[1]][1:2],collapse = "_"),
                    target_number = str_count(Target, "\\S+")) %>%
      ### Removing Date and File variables since we do not need them
      dplyr::select(!c(Date, File)) %>%
      ### Relocating new variables
      dplyr::relocate(speaker, .before = Target) %>%
      dplyr::relocate(phrase, .before = Target) %>%
      dplyr::relocate(target_number, .after = Target)
    
    data_list[[length(data_list) + 1]] <- data
    
  }
  
}

## Merging the individual dfs together
transcriptions <- do.call(rbind, data_list)

## Removing unneeded items from the environment

rm(data, data_list, file_name, file_names, file_path, path)

# Fix Transcriptions for AM1

## There was an error in matching the target phrase with the correct participant responses for AM1.
## This next section of code fixes this error.


