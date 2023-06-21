## Merger file for protected understanding society 

library(tidyverse)
library(data.table)
library(haven)

## File location setter 

setwd("C:/Users/40344509/Documents/Data/UKDA-6931-tab/tab/ukhls")

## Assign variables of interest - format: '_var' where wave prefix (e.g. a_, b_ etc. is omitted) - this vector is later used with the 'select_vars' function to select variables.

vars_wanted = c('idp',"_sex_dv","_intdaty_dv", "_intdatm_dv", "_age_dv", "_birthm", "_birthy", "_gor_dv", "_mastat_dv", "_health", "_racel", "_hiqual_dv", '_jbstat', "_jbft_dv", '_jbhrs', "_jbsic07_cc", "_fimnnet_dv", "_fimnsben_dv", "nkids_dv", "agechy_dv", "_ccare", "wrkch2a1", "wrkch2a2", "wrkch2a3", "wrkch2a4", "wrkch2a5", "wrkch2a6", "wrkch2a7", "wrkch2a8", "wrkch2a9", "wrkch2a10", "wrkch2a11", "wrkch2a12", "wrkch2a13", "wrkch2a14", "wrkch2a15", "_pns1pid", "_pns2pid")

select_vars_ending_with = function(df, var_wanted) {
  select(df, ends_with(vars_wanted))
}

## File lister - only takes files beginning with a - w to pull wave specific files. i.e. no x-wave files

files = list.files(pattern = '^[a-w]')

## Specific file listers - this creates separate lists for their respective thematic areas.

list_indall = list.files(pattern = '*_indall_protect.tab')
list_indresp = list.files(pattern = '*_indresp_protect.tab')
list_hhresp = list.files(pattern = '*_hhresp_protect.tab')
list_child = list.files(pattern = '*_child_protect.tab')

list_full = c(list_indall, list_indresp, list_hhresp, list_child)

## This reads in each thematic grouping of files (e.g. individual responses, then household responses) applies the variable selecter and then garbage collects for memory capacity

indresp = lapply(list_indresp, fread)
indresp = map(indresp, ~ select_vars_ending_with(.x, vars_wanted))
hhresp = lapply(list_hhresp, fread)
hhresp = map(hhresp, ~ select_vars_ending_with(.x, vars_wanted))
child = lapply(list_child, fread)
child = map(child, ~ select_vars_ending_with(.x, vars_wanted))
gc()

## This simply reassigns the names for each file in line with the file listers before (e.g. individual response file for 2009 is assigned a_indresp_protect.tab etc.)

names(indresp) = gsub("\\.tab$", "", list_indresp)
names(child) = gsub("\\.tab$", "", list_child)
names(hhresp) = gsub("\\.tab$", "", list_hhresp)

## This function takes a dataframe as an argument and then renames columns to remove wave-specific prefixes. E.g. 'a_hidp' (household identifier for wave A) is renamed to 'hidp'
## This is then mapped to the lists of individual responses and child data files read in above

rename_cols <- function(df) {
  colnames(df) <- gsub("^[a-l]_", "", colnames(df))
  return(df)
}

indresp = map(indresp, rename_cols)
child = map(child, rename_cols)
hhresp = map(hhresp, rename_cols)

## This function takes the datafiles above and simply appends a character argument as a suffix to the column names. 
## This is then mapped on to the list of child responses (e.g. so child year of birth variables are renamed to 'birthy_child' to differentiate from parental response)

append_suffix_to_variables <- function(list_of_dataframes, suffix, exclude_vars = character(0)) {
  modified_dataframes <- map(list_of_dataframes, ~ .x %>%
                               rename_with(~ if_else(. %in% exclude_vars, ., paste0(., suffix)), everything()))
  return(modified_dataframes)
}

child = append_suffix_to_variables(child, '_child', 'hidp')
hhresp = append_suffix_to_variables(hhresp, '_hhresp', 'hidp')

## Export to main environment - this unpacks the lists of individual and child response dataframes

list2env(indresp)
list2env(child)
list2env(hhresp)

## The function below takes two lists and a 'by' argument to join the dataframes together (i.e. wave A individual response is merged with wave A child response etc.)
## This is then applied to the individual response and child lists by the household identifier number ('hidp')

merger_function <- function(list1, list2, by) {
  if (length(list1) != length(list2)) {
    stop("Error: The input lists must have the same length.")
  }
  
  merged_list <- map2(list1, list2, ~ left_join(.x, .y, by = by))
  
  return(merged_list)
}

usoc = merger_function(indresp, hhresp, 'hidp')
usoc = merger_function(usoc, child, 'hidp')

## Row_binder function simply takes the within wave merged list above and then merges across waves (by binding rows).
## This is then run and assigned to the usoc_final object - the final merged data file.

row_binder <- function(listofdf) {
  merged_dataframe <- bind_rows(listofdf)
  return(merged_dataframe)
}

usoc_final = row_binder(usoc)

## This generates date-type variables for parent DOB, child DOB and interview dates (to be used for generating treatment variables)

usoc_final = usoc_final %>%
                  mutate(child_dob = as.Date(paste(birthy_child, birthm_child, "01", sep = "-"), format = "%Y-%m-%d"))
usoc_final = usoc_final %>%
                  mutate(parent_dob = as.Date(paste(birthy, birthm, "01", sep = "-"), format = "%Y-%m-%d"))
usoc_final = usoc_final %>%
                  mutate(int_dat = as.Date(paste(intdaty_dv, intdatm_dv, "01", sep = "-"), format = "%Y-%m-%d"))

# Dropping some irrelevant variables

usoc_final = usoc_final %>% select(-gor_dv_child, -gor_dv_hhresp)

## This simply removes superfluous files from the environment and applies garbage collector to free memory.

rm(list=ls(pattern = "^[a-l]"), list_ind, list_hh)
rm(usoc)
gc()

## This writes the final merged file outward. 

write_csv(usoc_final, "C:/Users/40344509/Documents/Ch1 Rep/usoc_final.csv")
