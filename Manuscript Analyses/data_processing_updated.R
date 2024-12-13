# Data Preprocessing

# Author: Micah E. Hirsch (mhirsch@fsu.edu)
# Date: 12/13/2024

# R Version: 4.3.2
# Purpose: To prepare data for analysis

# Packages

library(rio) #install.packages("rio")
library(tidyverse) #install.packages("tidyverse")
library(remotes) #install.packages("remotes")
library(autoscore) #remotes::install_github("autoscore/autoscore")
library(SnowballC) #install.packages("SnowballC")
library(janitor) #install.packages("janitor")
library(lubridate) #install.packages("lubridate")

# Set Working Directory

setwd("~/Documents/Github/Older Adult PL Study Analysis/Raw Data")

# Specifying paths for where listener data is located
file_path <- list.dirs(path = "./Participant Data", full.names = T, recursive = F)

# Creating empty list to temporarily hold dfs
data_list <- list()

for (path in file_path) {
  
  file_list <- list.files(path, pattern = "csv", full.names = T)
  
  for (file in file_list) {
    
    data = rio::import(file) %>%
      dplyr::rename_all(., .funs = tolower) %>%
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
transcriptions <- do.call(rbind, data_list) %>%
  dplyr::mutate(date = as.Date(date))

# Removing unneeded items from the environment
rm(data, data_list, file, file_list, file_path, path)

# Participant Demographics 

## Extracting date of participation from transcription df (need this to calculate age)
date <- transcriptions %>%
  dplyr::select(id, date) %>%
  dplyr::distinct()

## Loading and cleaning participant demo information 
demo <- rio::import("participant_demo_raw.xlsx") %>%
  dplyr::select(2:6) %>%
  janitor::clean_names() %>%
  dplyr::rename(id = participant_id,
                dob = date_of_birth,
                race = race_you_can_choose_multiple) %>%
  dplyr::mutate(id = gsub("P", "", id),
                id = trimws(id, "both"),
                id = as.numeric(id),
                dob = as.Date(as.character(dob), "%m/%d/%Y"),
                ethnicity = gsub("of", "or", ethnicity),
                gender = tolower(gender),
                race = tolower(race),
                ethnicity = tolower(ethnicity))

demo_clean <- left_join(date, demo, by = "id")

demo_clean <- demo_clean %>%
  dplyr::mutate(age = lubridate::time_length(interval(ymd(dob), ymd(date)), "year"),
                age = floor(age)) %>%
  dplyr::select(!c(date, dob)) %>%
  dplyr::relocate(age, .after = "id")

## Remove unneeded objects from the environment

rm(date, demo)

## Remove date of participation from transcription df (we no longer need it)

transcriptions <- transcriptions %>%
  dplyr::select(-date)

# Fix transcriptions for AM1

## There is an error for matching the correct target and response phrases for listeners trained with speaker AM1.
## This next section of code fixes this error

targets <- rio::import_list("Stimuli List/Stimuli Testing Sets.xlsx", 
                            which = c("Pretest", "Posttest"), rbind = T, rbind_label = "Type") %>%
  rename_all(., .funs = tolower) %>%
  dplyr::mutate(target = tolower(target),
                target = trimws(target, "both"),
                target_number = str_count(target, "\\S+"))

transcriptions_fixed <- transcriptions %>%
  dplyr::rename(target2 = target,
                target_number2 = target_number) %>%
  dplyr::left_join(targets, by = "code") %>%
  mutate(target = dplyr::if_else(speaker %in%  "PDM10", target2, target),
         target_number = dplyr::if_else(speaker %in% "PDM10", target_number2, target_number)) %>% 
  select(-c(target2, target_number2, type.y)) %>%
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

# Cognitive Data

## Cleaning cognitive data collected from NIH Toolbox Battery and merging with transcription data

cog <- rio::import("Cognitive Data/2023-01-20 12.45.01 Assessment Scores.csv")

# Selecting needed participants and variables

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
  ## Removing unnecessary subtests
  dplyr::filter(inst != "picture_vocabulary_test_age_3+_practice_v2.1") %>%
  dplyr::filter(inst != "pattern_comparison_processing_speed_test_age_7+_practice_v2.1")

## Subsetting cognitive scores from words-in-noise test
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

### Merging uncorrected and age-corrected scores

cog_subtests <- dplyr::full_join(uncorrected, corrected, by = "id") %>%
  dplyr::mutate(., id = as.numeric(id))

## Creating words-in-noise df
words_in_noise <- cog1 %>%
  dplyr::filter(inst == "win") %>%
  dplyr::select(!2:4)

## creating new variable names for words-in-noise

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

# I found a duplicate entry for participant 327, so we are removing them 
# and any other participants with no win scores
words_in_noise <- words_in_noise %>%
  dplyr::filter(!is.na(win_r))

# Merging the cognitive, win, and transcription dfs together
cleaned_data <- transcriptions_fixed %>%
  left_join(cog_subtests, by = "id")

cleaned_data <- cleaned_data %>%
  dplyr::left_join(words_in_noise, by = "id")

# Removing unneeded items from the environment
rm(cog, cog_subtests, cog1, corrected, uncorrected, win, words_in_noise, i, new_names)

# Export 
setwd("~/Documents/Github/Older Adult PL Study Analysis/Manuscript Analyses")

## intelligibility and cog df
rio::export(cleaned_data, "cleaned_data.csv")

## participant demo df
rio::export(demo_clean, "participant_demo.csv")
