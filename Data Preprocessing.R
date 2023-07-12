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
                    code = str_remove(File, "_AM1\\.wav"),
                    code = str_remove(code, "_PDM10\\.wav"),
                    code = str_remove(code, "_ALSM1\\.wav"),
                    target_number = str_count(Target, "\\S+")) %>%
      ### Removing Date and File variables since we do not need them
      dplyr::select(!c(Date, File)) %>%
      ### Relocating new variables
      dplyr::relocate(speaker, .before = Target) %>%
      dplyr::relocate(code, .before = Target) %>%
      dplyr::relocate(target_number, .after = Target)
    
    data_list[[length(data_list) + 1]] <- data
    
  }
  
}

## Merging the individual dfs together
transcriptions <- do.call(rbind, data_list) %>%
  rename_all(., .funs = tolower)

## Removing unneeded items from the environment

rm(data, data_list, file_name, file_names, file_path, path)

# Fix Transcriptions for AM1

## There was an error in matching the target phrase with the correct participant responses for AM1.
## This next section of code fixes this error.

targets <- rio::import_list("Raw Data/Stimuli List/Stimuli Testing Sets.xlsx", 
                            which = c("Pretest", "Posttest"), rbind = T, rbind_label = "Type") %>%
  rename_all(., .funs = tolower)

transcriptions2 <- transcriptions %>%
  dplyr::rename(target2 = target) %>%
  dplyr::left_join(targets, by = "code") %>%
  mutate(target = if_else(speaker %in%  "PDM10", target2, target)) %>% 
  select(-c(target2, type.y)) %>%
  dplyr::rename(type = type.x) %>%
  dplyr::relocate(target, .before = target_number)

## Removing unneedeed items from the environment

rm(targets, transcriptions)

# Autoscore

## Counting the number of words correctly transcribed using Autoscore

transcriptions2 <- autoscore::autoscore(
  transcriptions2,
  double_letter_rule = T,
  acceptable_df = autoscore::acceptable_spellings,
  plural_rule = T,
  plural_add_rule = T,
  tense_rule = T,
  tense_add_rule = T,
  a_the_rule = T,
  double_letter_rule = T) %>%
  dplyr::rename(correct_words = autoscore)

# Cognitive Data

## In the sections below, the cognitive data collected from the participants will be cleaned
## and merged with the transcription df

## Importing raw cognitive data

cog <- rio::import("Raw Data/Cognitive Data/2023-01-20 12.45.01 Assessment Scores.csv")

## Selecting needed participants and variables. Data
