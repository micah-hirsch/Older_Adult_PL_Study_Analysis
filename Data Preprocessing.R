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

## Selecting needed participants and variables.

cog1 <- cog %>%
  ## Selecting needed variables
  dplyr::select(`PIN`, `Inst`, `Uncorrected Standard Score`, 
                `Age-Corrected Standard Score`, `Threshold Right Ear`, `Threshold Left Ear`) %>%
  ## Filtering out any participants that are "test", "lab assistant", etc.
  dplyr::filter(!grepl("Lab Assistant", PIN)) %>%
  dplyr::filter(!grepl("Test", PIN)) %>%
  dplyr::filter(!grepl("test", PIN)) %>%
  dplyr::filter(!grepl("Volunteer", PIN)) %>%
  dplyr::filter(!grepl("P3redo", PIN)) %>%
  dplyr::filter(!grepl("ABC", PIN)) %>%
  ## Removing P from beginning of ID column
  dplyr::mutate(PIN = ifelse(grepl("P", PIN), gsub("P", "", PIN), PIN)) %>%
  ## Filtering out composite scares
  dplyr::filter(Inst != "Cognition Early Childhood Composite v1.1") %>%
  ## Rename PIN to id
  dplyr::rename(id = PIN) %>%
  ## Recode NIH subtests
  dplyr::mutate(Inst = str_remove(Inst, "NIH Toolbox ")) %>%
  dplyr::mutate(Inst = str_remove(Inst, " Age\\s*\\d+\\+\\s*(Form\\s+A\\s+)?v2\\.1$")) %>%
  dplyr::mutate(Inst = tolower(gsub(" ", "_", Inst))) %>%
  ## Making variable names all in lower case
  rename_all(., .funs = ~tolower(gsub(" ", "_", ., fixed = T))) %>%
  ## Shortening NIH variable names
  dplyr::mutate(inst = case_when(inst == "words-in-noise_test" ~ "win",
                                 inst == "list_sorting_working_memory_test" ~ "list_sort",
                                 inst == "flanker_inhibitory_control_and_attention_test" ~ "flanker",
                                 inst == "dimensional_change_card_sort_test" ~ "card_sort",
                                 inst == "picture_vocabulary_test" ~ "vocab",
                                 inst == "pattern_comparison_processing_speed_test" ~ "pattern",
                                 inst == "picture_sequence_memory_test" ~ "pic_seq",
                                 inst == "cognition_fluid_composite_v1.1" ~ "fluid_cog",
                                 TRUE ~ inst)) %>%
  ## Removing unnecessary variables
  dplyr::filter(inst != "picture_vocabulary_test_age_3+_practice_v2.1") %>%
  dplyr::filter(inst != "pattern_comparison_processing_speed_test_age_7+_practice_v2.1")

# Cleaning Cognitive Subtest Scores

## Creating a separate df for cog subtest scores without words-in-noise test

cog_subtests <- cog1 %>%
  dplyr::filter(inst != "win") %>%
  dplyr::select(!c(threshold_right_ear, threshold_left_ear))

### Getting the uncorrected scores

uncorrected <- cog_subtests %>%
  dplyr::select(c(id, inst, uncorrected_standard_score)) %>%
  tidyr::pivot_wider(names_from = inst,
                     values_from = uncorrected_standard_score)

### Renaming uncorrected score variables

new_names <- names(uncorrected) %>%
  map_chr(~ if (. != "id") paste0(., "_u") else .)

names(uncorrected) <- new_names  

### Getting the age-corrected scores

corrected <- cog_subtests %>%
  dplyr::select(c(id, inst, `age-corrected_standard_score`)) %>%
  tidyr::pivot_wider(names_from = inst,
                     values_from = `age-corrected_standard_score`)

### Renaming age-corrected score variables

new_names <- names(corrected) %>%
  map_chr(~ if (. != "id") paste0(., "_c") else .)

names(corrected) <- new_names  

### Merging uncorrected and age-corrected standard scores together

cog_subtests <- dplyr::full_join(uncorrected, corrected, by = "id") %>%
  dplyr::mutate(., id = as.numeric(id))

## Creating a separate df for words-in-noise test scores

words_in_noise <- cog1 %>%
  dplyr::filter(inst == "win") %>%
  dplyr::select(!2:4)

### Creating two win variables for left and right ear thresholds

new_names <- names(words_in_noise) 

for (i in seq_along(new_names)) {
  if (new_names[i] != "id") {
    new_names[i] <- gsub("threshold", "win", new_names[i])
  }
}

names(words_in_noise) <- new_names

words_in_noise <- words_in_noise %>%
  dplyr::rename(win_r = "win_right_ear",
                win_l = "win_left_ear") %>%
  dplyr::mutate(id = as.numeric(id))

