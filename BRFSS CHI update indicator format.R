##########################################################################################
##BRFSS Community Health Indicators 
## By: Abby Schachter, May 2018
## Update BRFSS indicators to new Tableau-ready format and update data with new indicators
#########################################################################################

rm(list=ls())
options(max.print = 350, tibble.print_max = 30, scipen = 999)

# install.packages("tidyverse")
# install.packages("openxlsx")
# install.packages("readxl")
library(tidyverse) # Manipulate data
library(openxlsx) # Read and write Excel files
library(odbc)

db_store <- dbConnect(odbc(), "PHExtractStore51")

chi_path <- "//Phshare01/epe_share/WORK/CHI Visualizations/BRFSS Indicators (all)/BRFSS (all)"

brfss <- read.xlsx(file.path(chi_path, "BRFSS_combined_tableau_suppressed.xlsx"), sheet=1)

brfss_path <- "//Phshare01/epe_share/WORK/CHI Visualizations/BRFSS Indicators (all)/BRFSS (all)/BRFSS 2017 indicator updates/NewFormat"

# look into paste functions

#import new indicator data
# ecig <-  read.xlsx(file.path(brfss_path, "ECIGA_suppress_2019-0523 (new format).xlsm"), sheet="results")
# smoker <- read.xlsx(file.path(brfss_path, "CIGA_suppress_2019-0523 (new format).xlsm"), sheet="results") 
# firearm <- read.xlsx(file.path(brfss_path, "GUNHOME_suppress_2019-0523 (new format).xlsm"), sheet="results") 
# # replace with suppressed version!
# obese <- read.xlsx(file.path(brfss_path, "OBEA_suppress_2919-0523 (new format).xlsm"), sheet="results")
# physact <- read.xlsx(file.path(brfss_path, "PHYSAMET_suppress_2019-0523 (new format).xlsm"), sheet="results")
# physact1824 <- read.xlsx(file.path(brfss_path, "PHYSAMET1824_suppress_2019-0523 (new format).xlsm"), sheet="results")
# mjuse <- read.xlsx(file.path(brfss_path, "MJUSE_suppress_2019-0523 (new format).xlsm"), sheet="results")
# 
# # combine new indicators into single table
# brfss_new <-bind_rows(ecig, smoker, firearm, obese, physact, physact1824, mjuse) 


# import new indicator data
files = list.files(brfss_path, pattern='*.xlsm')

# import each new indicator based on file suffix; change data type to numeric for num, denom, and chi columns
brfss_new <- lapply(file.path(brfss_path, files), function(x){
  a = read.xlsx(x, sheet = 'results')
  a$numerator = as.numeric(a$numerator)
  a$denominator = as.numeric(a$denominator)
  a$chi = as.numeric(a$chi)
  return(a)
})

# convert new indicators from list to dataframe
brfss_new = bind_rows(brfss_new) %>% 
  #format new indicators
  mutate(cat1 = case_when(
    tab == 'trends' & (cat1_varname == 'race3'| cat1_varname == 'race4') ~ 'Race/ethnicity',
    cat1_varname == 'race3' & cat1_group == "Hispanic" ~ 'Ethnicity',
    (cat1_varname == 'race3' & cat1_group != "Hispanic") | cat1_varname == 'race4' ~ 'Race',
    TRUE ~ cat1),
    cat2 = case_when(
      cat2_varname == 'race3' & cat2_group == "Hispanic" ~ 'Ethnicity',
      (cat2_varname == 'race3' & cat2_group != "Hispanic") | cat2_varname == 'race4' ~ 'Race',
      TRUE ~ cat2),
    cat1_group_alias = case_when(
      cat1 == "Gender" ~ cat1_group,
      TRUE ~ cat1_group_alias),
    cat2_group_alias = case_when(
      cat2 == "Gender" ~ cat2_group,
      TRUE ~ cat2_group_alias),
    cat1_group = gsub(" NH", "", cat1_group),
    cat2_group = gsub(" NH", "", cat2_group),
    cat1_group_alias = gsub(" NH", "", cat1_group_alias),
    cat2_group_alias = gsub(" NH", "", cat2_group_alias),
    comparison_with_kc = ifelse(comparison_with_kc == 'nodiff', 'no different', comparison_with_kc)) %>% 
   filter(!(indicator_key == '_pastaer_v2' & cat1_varname == 'hracode')) %>%
  filter(!(indicator_key == '_pastaer_v2' & tab == 'crosstabs' & cat2_varname == 'hracode'))


# list new indicators that were imported
keys = brfss_new[, 'indicator_key', drop=T] %>% unique

#reformat old brfss data into new Tableau-ready output format
brfss_old <- brfss %>% 
#update category labels
    mutate(
      chi = 1,
      Tab = case_when(
        Tab == "Subgroups" ~ "demgroups",
        Tab == "SmallGroups" ~ "crosstabs",
        Tab == "_King County" ~ "_king county",
        Tab == "Trends" ~ "trends"),
      Cat1varname = case_when(
        Cat1varname == 'mrace' ~ "race3",
        Cat1varname == 'hispanic' ~ "race3",
        Category1 == 'Age' ~ "age4",
        Category1 == 'Gender' ~ 'sex',
        Category1 == 'Race/Ethnicity' ~ 'race3',
        Category1 == 'Household Income' ~ 'income6',
        Category1 == 'Sexual orientation' ~ 'sexorien2',
        Category1 == 'King County regions' ~ 'ccreg',
        Category1 == "Health Reporting Areas" ~ 'hracode',
        Category1 == "King County" ~ "kingco",
        TRUE ~ Cat1varname),
      Cat2varname = case_when(
        Cat2varname == 'mrace' ~ "race3",
        Cat2varname == 'hispanic' ~ "race3",
        Category2 == 'Age' ~ "age4",
        Category2 == 'Gender' ~ 'sex',
        Category2 == 'Household Income' ~ 'income6',
        Category2 == 'Sexual orientation' ~ 'sexorien2',
        Category2 == 'King County regions' ~ 'ccreg',
        Category2 == "Health Reporting Areas" ~ 'hracode',
        Category2 == "King County" ~ "kingco",
        TRUE ~ Cat2varname),
      Category1 = case_when(
        Tab == 'trends' & (Cat1varname == 'race3' | Cat1varname == 'race4') ~ 'Race/ethnicity',
        Tab != 'trends' & Category1 == "Race/Ethnicity" & Group == "Hispanic" ~ 'Ethnicity',
        Tab != 'trends' & Category1 == "Race/Ethnicity" &  Group != "Hispanic" ~ 'Race',
        Tab != 'trends' & Cat1varname == 'race3' & Group == "Hispanic" ~ 'Ethnicity',
        (Cat1varname == 'race3' & Group != "Hispanic") | Cat1varname == 'race4' ~ 'Race',
        Category1 == "Health Reporting Areas" ~ "Cities/neighborhoods",
        Category1 == "Income" ~ "Household income",
        Category1 == "Household Income" ~ "Household income",
        Category1 == "King County regions" ~ "Regions",
        TRUE ~ Category1),
      Category2 = case_when(
        Category2 == "Race/Ethnicity" & Subgroup == "Hispanic" ~ 'Ethnicity',
        Category2 == "Race/Ethnicity" &  Subgroup != "Hispanic" ~ 'Race',
        Cat2varname == 'race3' & Group == "Hispanic" ~ 'Ethnicity',
        (Cat2varname == 'race3' & Group != "Hispanic") | Cat2varname == 'race4' ~ 'Race',
        Category2 == "Health Reporting Areas" ~ "Cities/neighborhoods",
        Category2 == "King County regions" ~ "Regions",
        Category2 == "Region" ~ "Regions",
        Tab == "Subgroups" ~ "Overall",
        TRUE ~ Category2),
      Group = case_when(
        Group == 'Fed Way-Dash Point/Woodmont' ~ 'Fed Way-Dash Pt',
        Group == '>=$100,000' ~ '$100,000+',
        TRUE ~ Group),
      Subgroup = case_when(
        Tab == "Subgroups" ~ "Overall",
        Subgroup == 'Fed Way-Dash Point/Woodmont' ~ 'Fed Way-Dash Pt',
        Subgroup == '>=$100,000' ~ '$100,000+',
        TRUE ~ Subgroup),
      cat1_group_alias = case_when(
        Group == "Beacon/Gtown/S.Park" ~ "Beacon Hill/Georgetown/South Park",
        Group == "Bellevue-NE" ~ "Bellevue-Northeast",
        Group == "Black Diamond/Enumclaw/SE County" ~ "Black Diamond/Enumclaw/Southeast County",
        Group == 'Capitol Hill/E.lake' ~ "Capitol Hill/Eastlake",
        Group == "Fed Way-Central/Military Rd" ~ "Federal Way-Central/Military Rd",
        Group == "Fed Way-Dash Pt" ~ "Federal Way-Dash Point/Woodmont",
        Group == "Kenmore/LFP" ~ "Kenmore/Lake Forest Park",
        Group == "Kent-SE" ~ "Kent-Southeast",
        Group == "Mercer Isle/Pt Cities" ~ "Mercer Island/Point Cities",
        Group == "NE Seattle" ~ "Northeast Seattle",
        Group == "NW Seattle" ~ "Northwest Seattle",
        Group == "SE Seattle" ~ "Southeast Seattle",
        Group == "QA/Magnolia" ~ "Queen Anne/Magnolia",
        grepl(' NH', Group) == TRUE ~ sub(' NH', '', Group),
        TRUE ~ Group),
      cat2_group_alias = case_when(
        Subgroup == "Beacon/Gtown/S.Park" ~ "Beacon Hill/Georgetown/South Park",
        Subgroup == "Bellevue-NE" ~ "Bellevue-Northeast",
        Subgroup == "Black Diamond/Enumclaw/SE County" ~ "Black Diamond/Enumclaw/Southeast County",
        Subgroup == 'Capitol Hill/E.lake' ~ "Capitol Hill/Eastlake",
        Subgroup == "Fed Way-Central/Military Rd" ~ "Federal Way-Central/Military Rd",
        Subgroup == "Fed Way-Dash Pt" ~ "Federal Way-Dash Point/Woodmont",
        Subgroup == "Kenmore/LFP" ~ "Kenmore/Lake Forest Park",
        Subgroup == "Kent-SE" ~ "Kent-Southeast",
        Subgroup == "Mercer Isle/Pt Cities" ~ "Mercer Island/Point Cities",
        Subgroup == "NE Seattle" ~ "Northeast Seattle",
        Subgroup == "NW Seattle" ~ "Northwest Seattle",
        Subgroup == "SE Seattle" ~ "Southeast Seattle",
        Subgroup == "QA/Magnolia" ~ "Queen Anne/Magnolia",
        grepl(" NH", Subgroup) == TRUE ~ sub(' NH', "", Subgroup),
        TRUE ~ Subgroup),
      caution = ifelse(rse>30, "!", ""),
      Comparisonwith.KC = ifelse(Comparisonwith.KC == 'nodiff', 'no different', Comparisonwith.KC),
      suppression = ifelse(Suppress == "Y", "^", ""),
      source_date = "") %>% 
rename("indicator_key" ="Indicator", 
        "tab" = "Tab",
        "year" = "Year",
        "cat1" = "Category1",
        "cat1_group" = "Group",
        "cat1_varname" = "Cat1varname",
        "cat2" = "Category2",
        "cat2_group" = "Subgroup",
        "cat2_varname" = "Cat2varname",
        "result" = "Percent",
        "lower_bound" = "Lower.bound",
        "upper_bound" = "Upper.bound",
        "comparison_with_kc" = "Comparisonwith.KC",
        "time_trends" = "Timetrends",
        "significance" = "Significance.level",
        "numerator" = "Numerator",
        "denominator" = "Sample_size",
        "run_date" = "runid") %>% 
  select(data_source, indicator_key, tab, year, cat1, cat1_group, cat1_group_alias, cat1_varname,
         cat2, cat2_group, cat2_group_alias, cat2_varname, result, lower_bound, upper_bound, se, rse, 
         comparison_with_kc, time_trends, significance, caution, numerator, denominator, suppression, chi, run_date)
  
  
# delete old versions of indicators from existing BRFSS table and replace with updated data
brfss_updated <- brfss_old %>% 
  filter(!indicator_key %in% c(keys,"_pastaer")) %>% 
  bind_rows(., brfss_new) 

# metadata - read in metadata from all new indicator files
brfss_metadata <- read.xlsx(file.path(chi_path, "BRFSS_combined_tableau_suppressed.xlsx"), sheet=2)


brfss_meta_new <- lapply(file.path(brfss_path, files), function(x){
  a = read.xlsx(x, sheet = 'metadata')
  return(a)
})

# combine into single metadata 
brfss_meta_new = bind_rows(brfss_meta_new) %>% 
  filter(!is.na(indicator_key)) %>% 
  mutate(latest_year_result = as.numeric(gsub('%', "", latest_year_result))/100,
         latest_year_kc_pop = as.numeric(gsub(",", "", latest_year_kc_pop)),
         latest_year_count = as.numeric(gsub(",", "", latest_year_count)),
         chi = as.numeric(chi),
         map_type = ifelse(indicator_key=="_pastaer_v2", "Region", map_type),
         valence = ifelse(indicator_key == 'firearm4', 'neutral', valence),
         unit = ifelse(indicator_key == '_pastaer_v2', 'adults 18-24', unit)) %>% 
  rename("run_date" = "rundate")

# filter(Item == "title") %>% 
#   mutate(title = case_when(
#           varname == "firearm4" ~ "Firearms Stored in Home",
#           TRUE ~ title),
#          ratetype = 'percent',
#          Indicator = varname, 
#          latest_rate = as.numeric(sub("%","", latest_rate))/100) %>% 
#   select(-Item,-Value) %>% 
#   select(Indicator, title, latest_data, ratetype:trendsummary, varname, everything())


brfss_meta_updated <- brfss_metadata %>% 
  filter(!Indicator %in% c(keys,"_pastaer")) %>% 
  mutate(valence = "negative",
         map_type = "HRA",
         result_type = ifelse(ratetype == "percent", "proportion", ratetype),
         data_source = tolower(data_source),
         latest_year_kc_pop = NA_integer_,
         latest_year_count = NA_real_,
         unit = ifelse(Indicator == "mam2yrs", "females 50-74", "adults"),
         chi = 1) %>% 
  rename("indicator_key" = "Indicator",
         "short_name" = "title",
         "latest_year_result" = "latest_rate",
         "run_date" = "runid") %>% 
  select(-latest_data, -trendsummary, -short_name, -varname, -Topic_new, -Domain_new, -savname, -sav_abbrev, -task_id) %>% 
  select (data_source, indicator_key, result_type, valence, latest_year, latest_year_result, latest_year_kc_pop, latest_year_count, map_type, unit, valid_years, chi, run_date) %>% 
  bind_rows(., brfss_meta_new)

# compare new vs. updated indicators for QA using 3-point threshold to flag changes
brfss_QA <- brfss_old %>% 
  filter(indicator_key %in% c(keys,"_pastaer")) %>% 
  mutate(result = if_else(indicator_key=="_pastaer", (1-result), result),
         indicator_key = ifelse(indicator_key=="_pastaer", "_pastaer_v1", indicator_key)) %>%
  select(indicator_key:cat1_varname, cat2:cat2_varname, result) %>% 
  rename("old_result" = "result") 

QA_groups <- left_join(brfss_QA, brfss_new, by = c("indicator_key", "tab", "cat1", "cat1_group", "cat2_varname", "cat2", "cat2_group", "cat2_varname")) %>% 
  select(indicator_key:old_result, year.y, result) %>% 
  left_join(., select(brfss_meta_updated, indicator_key, result_type), by = "indicator_key") %>% 
  filter(tab !="trends") %>% 
  mutate(difference = round(result-old_result, 3),
         flag = case_when(
           result_type == "proportion" & abs(difference) >= .03 ~ 'flag',
           result_type == "mean" & abs(difference) >= 3 ~ 'flag',
           TRUE ~ NA_character_
         ))

QA_trends <- left_join(brfss_QA, brfss_new, by = c("indicator_key", "tab", "year", "cat1", "cat1_group", "cat1_varname")) %>% 
  filter(tab=="trends")  %>% 
  select(indicator_key:old_result, result) %>% 
  left_join(., select(brfss_meta_updated, indicator_key, result_type), by = "indicator_key") %>% 
  mutate(difference = round(result-old_result, 3),
         flag = ifelse(difference == 0, 'flag', NA_character_)) %>% 
  filter(flag=='flag')

# write results to excel
list_of_datasets <- list("results" = brfss_updated, "metadata" = brfss_meta_updated, "QA" = QA_groups, "QA trends" = QA_trends)
currentDate <- Sys.Date()
xlsxFileName <- paste0("//Phshare01/epe_share/WORK/CHI Visualizations/BRFSS Indicators (all)/BRFSS (all)/BRFSS_combined_tableau_suppressed_", currentDate, ".xlsx")
write.xlsx(list_of_datasets, file = xlsxFileName)


# write results to SQL
tbl_id_meta <- DBI::Id(schema = "APDE_WIP", table = "brfss_metadata")
tbl_id_results <- DBI::Id(schema = "APDE_WIP", table = "brfss_results")


dbWriteTable(db_store, tbl_id_meta, brfss_meta_updated, overwrite = T)
dbWriteTable(db_store, tbl_id_results, brfss_updated, overwrite = T)

rm(tbl_id_meta, tbl_id_results)

