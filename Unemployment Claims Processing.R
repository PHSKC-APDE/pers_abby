# Author: Abby Schachter 
# Date: June 2020
# Project: CDC Evaluation of Economic, Social, and Overall Health Impacts of COVID-19
# Purpose: Combine and reshape Continued Uninsurance Claims reports from ESD for analysis
#

rm(list=ls())
library(openxlsx)
library(readxl)
library(tidyverse)
library(lubridate)

# 1.	Import all the excel workbooks in the folder

file.path <- "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/"

files <- list.files(paste0(file.path , "Continued Claims", pattern = "Continued Claims Week*", full.names = T))

files = files[!grepl('~$', files, fixed = T)]

continued_demographics <- read.xlsx(paste0(file.path, "Unemployment Claims Combined.xlsx"), sheet ="Continued Demographics", detectDates = T)
continued_industry <- read.xlsx(paste0(file.path, "Unemployment Claims Combined.xlsx"), sheet ="Continued NAICS", detectDates = T)
continued_occupation <- read.xlsx(paste0(file.path, "Unemployment Claims Combined.xlsx"), sheet ="Continued SOC", detectDates = T)

new_files <- setdiff(files, unique(continued_demographics$origin))
new_files

# create function to load specific worksheets from each excel workbook
load_sheet = function(f, sheet){
  print(paste(basename(f),sheet, sep=': '))
  
  # read in excel file
  dat = read.xlsx(f, sheet, startRow = 6, skipEmptyRows = FALSE)
  
  # select columns to keep
  keep_cols = c(names(dat)[!grepl('County', names(dat), fixed = T)], 'King.County')
  keep_cols = setdiff(keep_cols, c('Out.of.State', 'Not.Disclosed', 'State.Total'))
  dat = dat[, keep_cols]
  dat$King.County = as.numeric(dat$King.County)
  
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
  dat$origin <- f
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
continued_demographics = bind_rows(continued_demographics, bind_rows(lapply(new_files, function(x)(run_wb(x, sheets)))) %>% 
  unique() %>% filter(group != 'Not Latino/Hispanic:'))

# clean up category and group names per APDE standard terminology
continued_demographics = continued_demographics %>% 
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
         ) %>% unique() %>% select(category, group, week, week.start, week.end, claimants, sheet, origin)

# import and bind together claims by occupation
continued_occupation = bind_rows(continued_occupation, bind_rows(lapply(new_files, function(x) run_wb(x, "2 Digit SOC")))) %>% unique()

#import and bind together claims by industry
continued_industry = bind_rows(continued_industry, bind_rows(lapply(new_files, function(x) run_wb(x, "NAICS2")))) %>% unique()


##### Initial Unemployment Claims Demographics #####

# import demographics data

demog_file <- list.files(file.path, pattern = "Initial Claims Demographics*", full.names = T)

  
load_sheet_init = function(f, sheet){
  print(paste(f,sheet, sep=': '))
  
  # read in excel file
  dat = read.xlsx(f, sheet, startRow = 6, skipEmptyRows = FALSE)
  
  # select columns to keep
  keep_cols = c(names(dat)[!grepl('County', names(dat), fixed = T)], 'King.County')
  keep_cols = setdiff(keep_cols, c('Out.of.State', 'Not.Disclosed', 'State.Total'))
  dat = dat[, keep_cols]
  dat$King.County = as.numeric(dat$King.County)
  
  # keep only specific sheets with "Claimants" in the sheet name
  if(grepl('Claimants', sheet, fixed = T)){
    # find first blank row in the sheet
    end = min(which(is.na(dat[[1]])))
    #convert sheet names to category column
    dat$category = names(dat[1])
    dat$category = case_when(
      grepl('Education', dat$category) ~ 'Education',
      grepl('Veteran', dat$category) ~ "Veteran status",
      grepl("Gender", dat$category) ~ 'Sex',
      grepl('Race', dat$category) ~ 'Race/ethnicity',
      grepl('Disability', dat$category) ~ 'Disability',
      grepl('Age', dat$category) ~ 'Age'
    )
    names(dat)[1] = 'group'
    names(dat)[2] = 'claimants'
    # import sheet up until first blank row in the sheet
    if(length(end) > 0 && end < Inf){
      dat = dat[seq(1, end-1),]
    }
    
  }
  
  else {
    names(dat)[3] = 'claims'
  }
  
  
  # pull week number from filename
  dat$week_start <- as.numeric(regmatches(basename(f), gregexpr("[[:digit:]]+",text = basename(f)))[[1]][1])
  dat$week_end <- as.numeric(regmatches(basename(f), gregexpr("[[:digit:]]+",text = basename(f)))[[1]][2])
  dat$origin <- f
  # convert week number to week start and end dates
  dat$date.start <- ymd( "2020-01-05" ) + weeks( dat$week_start - 1 )
  dat$date.end <- ymd( "2020-01-05" ) + weeks( dat$week_end - 1 ) + 6
  dat$sheet = sheet
  return(dat)
  
}

# select list of sheets to import
sheets2 = c("Claimants by gender", "Claimants by race and ethnicity", "Claimants by age", 
           "Claimants by education")


# create table of claims by demographics by importing all files
demogs_initial = bind_rows(lapply(sheets2, function(x) load_sheet_init(demog_file, x))) %>% 
                           unique() %>% filter(group != 'Not Latino/Hispanic:')

# clean up category and group names per APDE standard terminology
demogs_initial = demogs_initial %>% 
  mutate(group = trimws(group),
         group = case_when(
           group == 'African American' ~ 'Black/African American',
           group == 'American Indian' ~ 'American Indian/Alaska Native',
           group == 'Pacific Islander' ~ 'Native Hawaiian/Pacific Islander',
           group == 'Caucasian' ~ 'White',
           group == 'Two or More Races' ~ 'Multiple Race',
           group == 'Latino/Hispanic of any race' ~ 'Hispanic/Latinx',
           grepl("Nonbinary", group) ~ "Nonbinary/Other", 
           group %in% c('No Schooling', 'Did not finish high school') ~ 'Less than high school',
           group == 'High School Diploma, including GED' ~ 'High school or equivalent, no college',
           group %in% c('Some College', "Associate's Degree") ~ 'Some college or Associate degree',
           group %in% c("Bachelor's Degree","Bachelor's degree or higher", "Master's Degree", "Post-Baccalaureate Degree","PhD") ~ "Bachelor's degree or advanced degree",
           TRUE ~ group)) %>% 
  unique() %>% 
  filter(!is.na(claimants) & group != "<24")

# combine groups  
demogs_combined <- demogs_initial %>% group_by(category, group) %>% summarize(claimants = sum(claimants))

# join combined totals back to metadata
demographics_initial <- left_join(demogs_combined, demogs_initial %>% select(-group, -claimants) %>% unique(), by = c("category"))

#### Import workforce demographics data ####


# workforce by age
age <- read.csv(paste0(file.path, "KC Worker Demographics Q2 2019/Age.csv"), header = T) %>% 
  mutate(agegrp_label.value = as.character(agegrp_label.value),
         category = "Age",
         group = case_when(
          agegrp_label.value == '14-18' ~ '<18',
          agegrp_label.value == '19-21' ~ '18-24',
          agegrp_label.value == '22-24' ~ '18-24',
          agegrp_label.value == '65-99' ~ '65+',
          TRUE ~ agegrp_label.value)
         ) %>% 
  select(category, group, Emp)

# workforce by education
education <- read.csv(paste0(file.path, "KC Worker Demographics Q2 2019/Education.csv"), header = ) %>% 
  mutate(education_label.value = as.character(education_label.value),
         category = "Education",
         group = education_label.value) %>% 
  select(category, group, Emp)
  
# workforce by race/ethnicity
race_ethnicity <- read.csv(paste0(file.path, "KC Worker Demographics Q2 2019/RaceEthnicity.csv"), header = T) %>% 
  mutate(ethnicity_label.value = as.character(ethnicity_label.value),
         category = 'Race/ethnicity',
         group = case_when(
            ethnicity_label.value == 'Hispanic or Latino' ~ "Hispanic/Latinx",
            grepl("Two", race_label.value) ~ "Multiple Race",
            ethnicity_label.value == 'Not Hispanic or Latino' ~ gsub(" or ", "/", gsub(" or Other ", "/", gsub(" Alone", "", as.character(race_label.value)))))) %>% 
  group_by(category, group) %>% summarize(Emp = sum(Emp))

# workforce by sex
sex <- read.csv(paste0(file.path, "KC Worker Demographics Q2 2019/Sex.csv"), header = T) %>% 
  mutate(category = 'Sex',
         group = as.character(sex_label.value)) %>% 
  select(category, group, Emp)

workforce_demogs <- bind_rows(age, sex, education, race_ethnicity)

#### Join unemployment demographics to workforce demographics

demographics_initial <- left_join(demographics_initial, workforce_demogs, by = c("category","group"))

##### Initial Claims by Occupation and Industry #####

initial_soc <- read.xlsx(paste0(file.path, "Initial Claims 2 Digit SOC by County Published Web.xlsx"),  sheet ="King", startRow = 6, colNames = T) %>% 
  gather(., key = "Week", value = "Claims", -SOC, -Occupational.Group) %>% 
  mutate(Week = gsub(".", " ", Week, fixed=T),
         Week = as.numeric(str_extract(Week, "[[:digit:]]+")),
         date.start = ymd( "2020-01-05" ) + weeks( Week - 1 ),
         date.end = ymd( "2020-01-05" ) + weeks( Week - 1 ) + 6,
         Claims = as.numeric(Claims))


initial_naics <- read.xlsx("//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Initial Claims 2 Digit NAICS by County Published Web.xlsx",  sheet ="King", startRow = 6, colNames = T) %>% 
  gather(., key = "Week", value = "Claims", -NAICS, -Industry) %>% 
  mutate(Week = gsub(".", " ", Week, fixed=T),
         Week = as.numeric(str_extract(Week, "[[:digit:]]+")),
         date.start = ymd( "2020-01-05" ) + weeks( Week - 1 ),
         date.end = ymd( "2020-01-05" ) + weeks( Week - 1 ) + 6,
         Claims = as.numeric(Claims),
         NAICS = ifelse(Industry == "Government", "92", NAICS))

# workforce by industry

qwi_industry <- read.csv("//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/Employment by Industry and County Q2 2019.csv") %>% 
  mutate(NAICS = as.character(industry),
         Industry = as.character(industry_label.value)) %>% 
  select(NAICS, Emp)

industry_combined <- left_join(initial_naics, qwi_industry, by = "NAICS")


##### Initial & Continued Claims by ZIP Code #####

claims_zip_file <- "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/Unemployment claims data and info/King County Claims by ZIP.xlsx"
  
load_sheet_zip = function(f, sheet){
    print(paste(basename(f), sheet, sep=': '))
    
    # read in excel file
    dat = if(sheet == "Initial claims") {
      read.xlsx(f, sheet, skipEmptyRows = FALSE)
    } else {
      read.xlsx(f, sheet, startRow = 3, skipEmptyRows = F) }
    
    # pivot weeks to columns
    dat = dat %>% gather(., key = "Week", value = !!sheet, -ZIP.Code) %>% 
      rename(ZIP_Code = ZIP.Code) %>% 
      mutate(Week = as.numeric(str_extract(Week, "[[:digit:]]+")),
             Date_Start = ymd( "2020-01-05" ) + weeks( Week - 1 ),
             Date_End = ymd( "2020-01-05" ) + weeks( Week - 1 ) + 6)
}
 
# create list of sheets/claim types to load
claim_types <- c("Initial claims", "Unduplicated Continued claims", "Initial PUA",  "PUA continued")

# load in each sheet
claims_by_zip_list <- lapply(claim_types, function(x)(load_sheet_zip(claims_zip_file, x)))

# join all claims
claims_by_zip <- left_join(claims_by_zip_list[[1]], claims_by_zip_list[[2]]) %>%  
  left_join(., claims_by_zip_list[[3]]) %>% 
  left_join(., claims_by_zip_list[[4]]) %>% 
  rename("Initial" = "Initial claims", Continued = "Unduplicated Continued claims", "Initial_PUA"="Initial PUA", "Continued_PUA" = "PUA continued") %>% 
  select(ZIP_Code, Week, Date_Start, Date_End, everything()) %>% 
  mutate(Suppressed_initial = ifelse(Initial == "*", "^", NA_character_),
         Suppressed_initialPUA = ifelse(Initial_PUA == "*", "^", NA_character_),
         Suppressed_continued = ifelse(Continued == "*", "^", NA_character_),
         Suppressed_continuedPUA = ifelse(Continued_PUA == "*", "^", NA_character_)) %>% 
  mutate_at(vars(Initial,Initial_PUA,Continued,Continued_PUA), as.numeric)

# Bring in population age 16-64 by ZIP to calculate claims per capita
pop_by_zip <- read.xlsx(paste0(file.path, "Population Age 16-64 by ZIP 2019.xlsx")) %>% select(ZIP, Population)

# join population to claims by ZIP
claims_by_zip <- left_join(claims_by_zip, pop_by_zip, by = c("ZIP_Code" = "ZIP"))

# create second version, pivoted long for Initial/PUA grouped bar chart
claims_by_zip_long <- claims_by_zip %>% 
  gather(., key = "Claim_Type", value = "Claims", Initial, Initial_PUA, Continued, Continued_PUA) %>% 
  mutate(Suppressed = case_when(
    Claim_Type == 'Initial' & Suppressed_initial == "^" ~ "^",
    Claim_Type == 'Initial_PUA ' & Suppressed_initialPUA == "^" ~ "^",
    Claim_Type ==  'Continued' & Suppressed_continued == "^" ~ "^",
    Claim_Type ==  'Continued_PUA' & Suppressed_continuedPUA == "^" ~ "^",
    TRUE ~ NA_character_)) %>% 
  select(-starts_with("Suppressed_"))


##### Write everything to Excel #####

# create Excel workbook shell for output
wb <- createWorkbook()


# write claims by ZIP to excel
addWorksheet(wb, "Claims by ZIP")
writeDataTable(wb, sheet = "Claims by ZIP", claims_by_zip, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  


# write continued claims by demographics to excel
addWorksheet(wb, "Claims by ZIP 2")
writeDataTable(wb, sheet = "Claims by ZIP 2", claims_by_zip_long, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  


# write continued claims by demographics to excel
addWorksheet(wb, "Initial Claims SOC")
writeDataTable(wb, sheet = "Initial Claims SOC", initial_soc, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write continued claims by demographics to excel
addWorksheet(wb, "Initial Claims NAICS")
writeDataTable(wb, sheet = "Initial Claims NAICS", initial_naics, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write continued claims by demographics to excel
addWorksheet(wb, "Initial Demographics")
writeDataTable(wb, sheet = "Initial Demographics", demographics_initial, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write continued claims by demographics to excel
addWorksheet(wb, "Continued Demographics")
writeDataTable(wb, sheet = "Continued Demographics", continued_demographics, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write continued claims by occupation to excel
addWorksheet(wb, "Continued SOC")
writeDataTable(wb, sheet = "Continued SOC", continued_occupation, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# write continued claims by industry to excel
addWorksheet(wb, "Continued NAICS")
writeDataTable(wb, sheet = "Continued NAICS", continued_industry, colNames = TRUE, rowNames = FALSE) # write the data to the new tab  

# save excel workbook
saveWorkbook(wb, file = paste0(file.path, "Unemployment Claims Combined.xlsx"), overwrite = TRUE)         


