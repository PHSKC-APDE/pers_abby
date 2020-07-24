## Header ####
# Author:  Abby Schachter 
# 
# Created: 7/23/2020
#
# R version: 3.6.2
#
# Purpose: Analysis of Census Household Pulse Survey data for CDC-funded evaluation of 
#   economic, social, and overall health impacts of COVID-19
#
# Census Pulse Survey website: https://www.census.gov/programs-surveys/household-pulse-survey/
# Downloaded pulse survey public use files: //PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Census Pulse Public CSV files
#        

## Set up environment ----
## Get RADS package
# devtools::install_github("PHSKC-APDE/rads", ref= 'danny')  # uncomment and run this line to install RADS
#     If that doesn't work, you might need to use your personal access token (PAT)
#     Follow instructions on Teams (APDE Training >> R >> Wiki >> "How to access files from a private repo in R")
# devtools::install_github("PHSKC-APDE/apdeRecodes", ref= 'danny', auth_token = Sys.getenv("GITHUB_PAT"))    

rm(list=ls()) # clear memory
library(data.table)
library(srvyr)
library(rads)
library(tidyverse)
library(openxlsx)
library(zoo)

## get Census Household Pulse Survey data ----
files <- list.files(path = "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Census Pulse Public CSV files", pattern = "pulse2020_puf_*", full.names = T)
weight_files <- list.files(path = "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Census Pulse Public CSV files", pattern = "pulse2020_repwgt_puf_*", full.names = T)

#create function to load in survey data and join with survey weights
load_data = function(i) {
  print(files[[i]])
  
  # read in csv
  dat = read.csv(files[[i]], header = T)
  dat <- dat %>% filter(EST_ST == 53)
  
  # read in weights
  weights = read.csv(weight_files[[i]], header = T)
  
  # join weights with data
  dat = left_join(dat, weights, by = c("SCRAM", "WEEK"))
  dat = rename_all(dat, tolower)
  
  na_convert = function(x){
    x[x%in% c(-88, -99)] <- NA
    return(x)
  } 
  dat <- dat %>% mutate_all(na_convert)

  return(dat)
}

data <- bind_rows(lapply(1:length(files), load_data))

## get data dicationary ----
dict_files <-list.files(path = "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Census Pulse Public CSV files", pattern = "pulse2020_data.dictionary_*", full.names = T)

# create function to load and process dictionary file
load_dict <- function(f) {
  print(paste("Loading dictionary week", as.numeric(regmatches(basename(f), gregexpr("[[:digit:]]+", text = basename(f)))[[1]][2])))  
  
  dict = read.xlsx(f, startRow = 3)
  setDT(dict)
  dict = dict[, lapply(.SD, na.locf)] 
  
  # clean up
  dict = dict %>% mutate(
    Variable = tolower(Variable),
    Description = gsub(":", "", Description),
    X4 = gsub("'", "", X4))
    # X4 = gsub("-9", "9", X4),
    # X4 = gsub("-8", "8", X4)) 
    # 
  # pivot values, description into columns
  dict = spread(dict, Description, X4, fill=NA)
  
  # separate each response option into separate row
  dict = dict %>% 
    separate_rows(., Values, sep = "\n") %>% 
    mutate(Values = str_trim(Values)) %>% 
    separate(Values, c("value", "label"), sep = "[^[:alnum:]^[-]]", extra = "merge", fill = "left") %>% 
    mutate(label = gsub(";", "", label),
           label = gsub("=", "", label),
           label = str_trim(label)) %>% 
    rename_all(tolower)
  
  # create column for week 
  dict$week <- as.numeric(regmatches(basename(f), gregexpr("[[:digit:]]+", text = basename(f)))[[1]][2])
  
  return(dict)
}

dict <- bind_rows(lapply(dict_files, load_dict))

## pull in data labels from data dictionary
#data <- left_join(data, dict %>% select(variable, week, value, label), by = c("variable", "week"))

# set survey design
survey.design <-
  srvyr::as_survey_rep(
    data ,
    weights = pweight ,
    combined.weights = TRUE ,
    repweights = grep('pweight[0-9]+', names(data), value  = T) ,
    scale = 4 / 80 ,
    rscales = rep( 1 , 80 ) ,
    mse = TRUE ,
    type = "JK1"
  )

# test output
results <- rads::calc(copy(survey.design), 
                      what = c('curfoodsuf'), 
                      est_st==53, est_msa==42660, 
                      time_var = "week", 
                      metrics = c("numerator", "denominator", "mean"))
