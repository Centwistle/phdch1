## Package install and load.

pack_check = c("tidyverse", "haven", "data.table")

packages.check <- lapply(
  pack_check,
  FUN = function(x) {
    if(!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## WD Getting and setting
## Please set WD to where Understanding Society (US) files are located

setwd("C:/Users/Craig/Documents/UKDA-6614-tab/tab/ukhls")

## List files and assign to list

file_list = list.files(pattern = "^[a-w]")

## Read in lists of individual and household response files. To add any more from the full directory, use formula: "list_** list.files(pattern = "*_FILENAME.DTA)"

list_ind = list.files(pattern = "*_indresp.tab")
list_hh = list.files(pattern = "*_hhresp.tab")

## Create function to read in individual files and filter for select variables and then remove file for memory management ##

ind = lapply(list_ind, fread)
ind = lapply(ind, function(x) x%>% select(ends_with("idp"),
                                           ends_with("_jbstat"),
                                           ends_with("_jbhrs"),
                                           ends_with("_jbft_dv"),
                                           ends_with("_jbsic07_cc"),
                                           ends_with("_jlsic07_cc"),
                                           ends_with("_intdaty_dv"),
                                           ends_with("_health"),
                                           ends_with("_birthy"),
                                           ends_with("_sex_dv"), 
                                           ends_with("_age_dv"),
                                           ends_with("_racel"),
                                           ends_with("_hiqual_dv"),
                                           ends_with("_mastat_dv"), 
                                           ends_with("_ccare"),
                                           ends_with("_gor_dv"),
                                           ends_with("_pn1pid"),
                                           ends_with("_pn1pno"),
                                           ends_with("_fimnnet_dv"),
                                           ends_with("_fimnsben_dv"),
                                           ends_with("_benbase3")))
names(ind) = gsub("\\.tab$", "", list_ind)

## Function to remove wave specific prefixes, then applying to list of waves

rename_cols <- function(df) {
  colnames(df) <- gsub("^[a-l]_", "", colnames(df))
  return(df)
}

ind <- lapply(ind, rename_cols)

## Exporting waves to environment ## 

list2env(ind, .GlobalEnv)

## Merging waves and then dropping erroneous variable included

usoc = bind_rows(a_indresp, b_indresp, c_indresp, d_indresp, e_indresp, f_indresp, g_indresp, h_indresp, i_indresp, j_indresp, k_indresp, l_indresp)
usoc = usoc %>% select(-ff_jbstat)

usoc = as.data.frame(usoc)

## Removing extraneous files from environment ##

rm(list=ls(pattern = "^[a-l]"), list_ind, list_hh)

## Writing out file ##

write_csv(usoc, "C:/Users/Craig/Documents/Programming/PhD Ch1/usoc_individuals.csv")