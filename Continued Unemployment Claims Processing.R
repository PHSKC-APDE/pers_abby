# Author: Abby Schachter 
# Date: June 2020
# Project: CDC Evaluation of Economic, Social, and Overall Health Impacts of COVID-19
# Purpose: Combine and reshape Continued Uninsurance Claims reports from ESD for analysis
#


library(openxlsx)
library(readxl)
library(tidyverse)
library(lubridate)

# 1.	Import all the excel workbooks in the folder

files <- list.files(path = "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Continued Claims", pattern = "Continued Claims Week*", full.names = T)

files = files[!grepl('~$', files, fixed = T)]
latest_file = files[length(files)]

demographics <- read.xlsx("//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Continued Claims/Continued Claims_combined.xlsx", sheet ="Demographics", detectDates = T)
industry <- read.xlsx("//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Continued Claims/Continued Claims_combined.xlsx", sheet ="Industry", detectDates = T)
occupation <- read.xlsx("//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Continued Claims/Continued Claims_combined.xlsx", sheet ="Occupation", detectDates = T)

# create function to load specific worksheets from each excel workbook
load_sheet = function(f, sheet){
  print(paste(f,sheet, sep=': '))
  
  # read in excel file
  dat = read.xlsx(f, sheet, startRow = 6, skipEmptyRows = FALSE)
  
  # select columns to keep
  keep_cols = c(names(dat)[!grepl('County', names(dat), fixed = T)], 'King.County')
  keep_cols = setdiff(keep_cols, c('Out.of.State', 'Not.Disclosed', 'State.Total'))
  dat = dat[, keep_cols]
  dat$King.County = as.numeric(dat$King.County)
  dat$origin = f
  
  # keep only specific sheets with "Claimants" in the sheet name
  if(grepl('Claimants', sheet, fixed = T)){
    # find first blank row in the sheet
    end = min(which(is.na(dat[[1]])))
    #convert sheet names to category column
    dat$category = names(dat[1])
    dat$category = case_when(
      grepl('Education', dat$category) ~ 'Education',
      grepl('Veteran', dat$category) ~ "Veteran status",
      grepl("Gender", dat$category) ~ 'Gender',
      grepl('Race', dat$category) ~ 'Race/ethnicity',
      grepl('Disability', dat$category) ~ 'Disability',
      grepl('Age', dat$category) ~ 'Age'
    )
    names(dat)[1] = 'group'
    names(dat)[2] = 'claimants'
    # import sheet up until first blank row in the sheet
    if(length(end) >0 && end < Inf){
      dat = dat[seq(1, end-1),]
    }
    
  }
  
  else {
    names(dat)[3] = 'claims'
  }

  
  # pull week number from filename
  dat$week <- as.numeric(regmatches(basename(f), gregexpr("[[:digit:]]+",text = basename(f)))[[1]])
  # convert week number to week start and end dates
  dat$week.start <- ymd( "2020-01-05" ) + weeks( dat$week - 1 )
  dat$week.end <- ymd( "2020-01-05" ) + weeks( dat$week - 1 ) + 6
  dat$sheet = sheet
  return(dat)
  
}

# select list of sheets to import
sheets = c("Claimants by gender", "Claimants by race and ethnicity", "Claimants by age", 
           "Claimants by education", "Claimants by veteran status", "Claimants by disability")


# create function to loop over files 
run_wb = function(wbpath, sheets){
  s1 = lapply(sheets, function(x) load_sheet(wbpath, x))
  return(bind_rows(s1))
}

# create table of claims by demographics by importing all files
demographics = bind_rows(demographics, run_wb(latest_file, sheets)) %>% 
  unique() %>% filter(group != 'Not Latino/Hispanic:')

# clean up category and group names per APDE standard terminology
demographics = demographics %>% 
  mutate(group = trimws(group),
         group = case_when(
            group == 'African American' ~ 'Black/African American',
            group == 'American Indian' ~ 'American Indian/Alaska Native',
            group == 'Pacific Islander' ~ 'Native Hawaiian/Pacific Islander',
            group == 'Caucasian' ~ 'White',
            group == 'Two or More Races' ~ 'Multiple Race',
            group == 'Latino/Hispanic of any race' ~ 'Hispanic/Latinx',
            group %in% c('No Schooling', 'Did not finish high school') ~ 'Less than high school',
            group == 'High School Diploma, including GED' ~ 'High school or equivalent',
            group %in% c('Some College', "Associate's Degree") ~ 'Some college or Associate degree',
            group %in% c("Bachelor's Degree","Master's Degree", "Post-Baccalaureate Degree","PhD") ~ "Bachelor's degree or higher",
            TRUE ~ group)
         )
# import and bind together claims by occupation
occupation = bind_rows(lapply(files, function(x) run_wb(x, "2 Digit SOC")))

#import and bind together claims by industry
industry = bind_rows(lapply(files, function(x) run_wb(x, "NAICS2")))


# create Excel workbook shell for output
wb <- createWorkbook()

# write claims by demographics to excel
addWorksheet(wb, "Demographics")
writeDataTable(wb, sheet = "Demographics", demographics, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write claims by occupation to excel
addWorksheet(wb, "Occupation")
writeDataTable(wb, sheet = "Occupation", occupation, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write claims by industry to excel
addWorksheet(wb, "Industry")
writeDataTable(wb, sheet = "Industry", industry, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# save excel workbook
saveWorkbook(wb, file = "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Continued Claims/Continued Claims_combined.xlsx", overwrite = TRUE)         
