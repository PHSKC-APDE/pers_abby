options(max.print = 350, tibble.print_max = 30, scipen = 999)

# install.packages("tidyverse")
# install.packages("openxlsx")
# install.packages("readxl")
library(tidyverse) # Manipulate data
library(openxlsx) # Read and write Excel files
library(readxl)
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

brfss_new <- lapply(file.path(brfss_path, files), function(x){
  a = read_excel(x, sheet = 'results')
  a$numerator = as.numeric(a$numerator)
  a$denominator = as.numeric(a$denominator)
  a$chi = as.numeric(a$chi)
  return(a)
})

brfss_new = bind_rows(brfss_new) 

# list new indicators that were imported
keys = brfss_new[, 'indicator_key', drop=T] %>% unique


# delete old versions of indicators from existing BRFSS table and replace with updated data
brfss_updated <- brfss %>% 
  filter(!Indicator %in% c(keys,"_pastaer")) %>% 
#update category labels
    mutate(
        chi = 1,
      Tab = case_when(
        Tab == "Subgroups" ~ "demgroups",
        Tab == "SmallGroups" ~ "crosstabs",
        Tab == "_King County" ~ "_kingco",
        Tab == "Trends" ~ "trends"),
      Category1 = case_when(
        Category1 == "Race/Ethnicity" ~ "Race/ethnicity",
        Category1 == "Health Reporting Areas" ~ "Cities/neighborhoods",
        Category1 == "Income" ~ "Household income",
        Category1 == "Household Income" ~ "Household income",
        Category1 == "King County regions" ~ "Regions",
        TRUE ~ Category1),
      Category2 = case_when(
        Category2 == "Race/Ethnicity" ~ "Race/ethnicity",
        Category2 == "Health Reporting Areas" ~ "Cities/neighborhoods",
        Category2 == "King County regions" ~ "Regions",
        Category2 == "Region" ~ "Regions",
        Tab == "Subgroups" ~ "Overall",
        TRUE ~ Category2),
      Cat1varname = case_when(
        Cat1varname == 'mrace' ~ "race3",
        TRUE ~ Cat1varname),
      Cat2varname = case_when(
        Cat2varname == 'mrace' ~ "race3",
        Tab == "Subgroups" ~ "Overall",
        TRUE ~ Cat2varname),
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
        grepl(' NH', Group) == TRUE ~ sub(' NH', '-NH', Group),
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
        grepl(" NH", Subgroup) == TRUE ~ sub(' NH', '-NH', Subgroup),
        TRUE ~ Subgroup),
      caution = ifelse(rse>30, "!", ""),
      Comparisonwith.KC = ifelse(Comparisonwith.KC == 'nodiff', 'no different', Comparisonwith.KC),
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
        "suppression" = "Suppress",
        "run_date" = "runid") %>% 
  select(data_source, indicator_key, tab, year, cat1, cat1_group, cat1_group_alias, cat1_varname, cat2, cat2_group, cat2_group_alias, cat2_varname, result, lower_bound, upper_bound, se, rse, comparison_with_kc, time_trends, significance, caution, numerator, denominator, chi, run_date) %>% 
  bind_rows(., brfss_new) 
  
# metadata
brfss_metadata <- read.xlsx(file.path(chi_path, "BRFSS_combined_tableau_suppressed.xlsx"), sheet=2)

# 
# ecig_meta <-  read.xlsx(file.path(brfss_path, "ECIGA_suppress_2019-0516 (new format).xlsm"), sheet="metadata")
# smoker_meta <- read.xlsx(file.path(brfss_path, "CIGA_suppress.xlsm"), sheet="metadata")
# firearm_meta <- read.xlsx(file.path(brfss_path, "GUNHOME_suppress.xlsm"), sheet="metadata") 
# obese_meta <- read.xlsx(file.path(brfss_path, "OBEA_suppress.xlsm"), sheet="metadata") 
# physact_meta <- read.xlsx(file.path(brfss_path, "PHYSAMET_suppress.xlsm"), sheet="metadata") 
# physact1824_meta <- read.xlsx(file.path(brfss_path, "PHYSAMET1824_suppress.xlsm"), sheet="metadata") 
# mjuse_meta <- read.xlsx(file.path(brfss_path, "MJUSE_suppress.xlsm"), sheet="metadata")
# 
# brfss_meta_new <-bind_rows(ecig_meta, smoker_meta, firearm_meta, obese_meta, physact_meta,physact1824_meta, mjuse_meta) %>% 

brfss_meta <- lapply(file.path(brfss_path, files), function(x){
  a = read_excel(x, sheet = 'metadata')
  return(a)
})

brfss_meta_new = bind_rows(brfss_meta) %>% 
  filter(!is.na(indicator_key)) %>% 
  mutate(latest_year_result = as.numeric(gsub('%', "", latest_year_result))/100,
         latest_year_kc_pop = as.numeric(gsub(",", "", latest_year_kc_pop)),
         latest_year_count = as.numeric(gsub(",", "", latest_year_count)),
         chi = as.numeric(chi)) %>% 
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
         unit = "adults",
         chi = 1) %>% 
  rename("indicator_key" = "Indicator",
         "short_name" = "title",
         "latest_year_result" = "latest_rate",
         "run_date" = "runid") %>% 
  select(-latest_data, -trendsummary, -short_name, -varname, -Topic_new, -Domain_new, -savname, -sav_abbrev, -task_id) %>% 
  select (data_source, indicator_key, result_type, valence, latest_year, latest_year_result, latest_year_kc_pop, latest_year_count, map_type, unit, valid_years, chi, run_date) %>% 
  bind_rows(., brfss_meta_new)

list_of_datasets <- list("results" = brfss_updated, "metadata" = brfss_meta_updated)
currentDate <- Sys.Date()
xlsxFileName <- paste("//Phshare01/epe_share/WORK/CHI Visualizations/BRFSS Indicators (all)/BRFSS (all)/BRFSS_combined_tableau_suppressed_", currentDate, ".xlsx")
write.xlsx(list_of_datasets, file = xlsxFileName)
