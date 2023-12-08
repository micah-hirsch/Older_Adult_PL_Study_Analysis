# Data Preprocessing

# Author: Micah E. Hirsch (mhirsch@fsu.edu)
# Date: 12/7/2023

# R Version: 4.3.2
# Purpose: To prepare data for analysis

# Packages

library(rio) #install.packages("rio")
library(tidyverse) #install.packages("tidyverse")
library(remotes) #install.packages("remotes")
library(autoscore) #remotes::install_github("autoscore/autoscore")
library(SnowballC) #install.packages("SnowballC")

# Set Working Directory

setwd("~/Documents/Github Repositories/Older Adult PL Study Analysis/Raw Data")

# Specifying paths for where listener data is located
file_path <- list.dirs(path = "./Participant Data", full.names = T, recursive = F)

# Creating empty list to temporarily hold dfs
data_list <- list()

for (path in file_path) {
  
  file_list <- list.files(path, pattern = "csv", full.names = T)
  
  for (file in file_list) {
    
    data = rio::import(file) %>%
      dplyr::rename_all(., .funs = tolower) %>%
      dplyr::select(-date) %>%
      # Creating new variables
      dplyr::mutate(speaker = basename(path),
                    code = sub("^(.*?)_[^_]+\\.wav$", "\\1", file),
                    target = tolower(target),
                    target = trimws(target, "both"),
                    response = tolower(response),
                    response = trimws(response, "both"),
                    target_number = str_count(target, "\\S+")) %>%
      ### Relocating new variables
      dplyr::relocate(speaker, .before = target) %>%
      dplyr::relocate(code, .before = target) %>%
      dplyr::relocate(target_number, .after = target) %>%
      dplyr::relocate(file, .after = response)
    
    data_list[[length(data_list) + 1]] <- data
    
  }
  
}

# Merging the individual dfs together
transcriptions <- do.call(rbind, data_list)

# Removing unneeded items from the environment
rm(data, data_list, file, file_list, file_path, path)

# Fix transcriptions for AM1

## There is an error for matching the correct target and response phrases for listeners trained with speaker AM1.
## This next section of code fixes this error

targets <- rio::import_list("Stimuli List/Stimuli Testing Sets.xlsx", 
                            which = c("Pretest", "Posttest"), rbind = T, rbind_label = "Type") %>%
  rename_all(., .funs = tolower)

transcriptions_fixed <- transcriptions %>%
  dplyr::rename(target2 = target) %>%
  dplyr::left_join(targets, by = "code") %>%
  mutate(target = dplyr::if_else(speaker %in%  "PDM10", target2, target)) %>% 
  select(-c(target2, type.y)) %>%
  dplyr::rename(type = type.x) %>%
  dplyr::relocate(target, .before = target_number)

# Removing unneeded items from the environment
rm(targets, transcriptions)

# Autoscore

## Count number of correct words using Autoscore

transcriptions_fixed <- autoscore::autoscore(
  transcriptions_fixed,
  acceptable_df = autoscore::acceptable_spellings,
  plural_rule = T,
  plural_add_rule = T,
  tense_rule = T,
  tense_add_rule = T,
  a_the_rule = T,
  double_letter_rule = T) %>%
  dplyr::rename(., correct_words = autoscore)
