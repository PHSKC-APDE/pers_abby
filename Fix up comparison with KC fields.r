#########
# Fix up time trend data in death and birth CHIs
#
# Alastair Matheson
# 2018-10
#########

#### Set up global parameters and call in libraries ####
options(max.print = 350, tibble.print_max = 30, scipen = 999)

library(tidyverse) # Manipulate data
library(openxlsx) # Read and write Excel files

chi_path <- "//Phshare01/epe_share/WORK/CHI Visualizations"


#### BRING IN DATA ####
firearm_death <- read.xlsx(file.path(chi_path, "Death Indicators (all)/Firearm Deaths/2013-2017 update/",
                              "SQL ready_CHI Firearm Deaths 2013-17.xlsx")) %>% 
  mutate(Cat1varname = ifelse(Category1 == "King County regions", "ccreg", Cat1varname),
         # Cat1varname = ifelse(grepl("-NH", Group, fixed=TRUE), "race4", 
         #                      ifelse(grepl("- NH", Group, fixed=TRUE), "race4", 
         #                             ifelse(Group == "Hispanic as Race", "race4", 
         #                                    ifelse(Group == "Hispanic", "race4", 
         #                                           ifelse(Category1 == "Race/Ethnicity", "race3", Cat1varname))))), 
         # Group = recode(Group, "American Indian/Alaskan Native Only-NH" = "AIAN - NH",
         #                "American Indian/Alaskan Native Only" = "AIAN",
         #                "Black Only" = "Black",
         #                "Black Only-NH" = "Black - NH",
         #                "Asian Only" = "Asian",
         #                "Asian Only-NH" = "Asian - NH",
         #                "Hispanic as Race" = "Hispanic",
         #                "Hispanic - any race" = "Hispanic",
         #                "Pacific Islander Only-NH" = "NHPI - NH",
         #                "Pacific Islander Only" = "NHPI",
         #                "White Only-NH" = "White - NH"), 
         Tab = ifelse(Cat1varname == "race3" & Tab != "Trends" & is.na(Cat2varname), "Subgroups", Tab),
         Cat1varname = (ifelse(Group=="White - NH", "race4", Cat1varname ))) %>%
  select(-Comparison_with_KC, -Significance_level) %>% 
  filter(!is.na(indicator_key))


#birth <- read.xlsx(file.path(chi_path, "Birth Indicators (all)",   "BIRTH_combined_suppress.xlsx"))

#acs <- read.xlsx(file.path(chi_path, "ACS Indicators (all)",   "ACS combined_2016 updated data.xlsx"))

#### DEATH DATA ####
death_kc <- firearm_death %>%
  filter(Category1 == "King County" & !is.na(indicator_key)) %>%
  select(indicator_key, Year, Lower_bound, Upper_bound, Tab) %>%
  mutate(Tab = ifelse(Tab=="_King County", "Subgroups", Tab),
         Lower_bound = round(Lower_bound, digits=1),
         Upper_bound = round(Upper_bound, digits=1)) %>% 
  rename(lb_kc = Lower_bound, ub_kc = Upper_bound)

death_trend <- firearm_death %>%
  filter(!is.na(indicator_key)) %>%
  select(indicator_key, Year, Tab, Category1,Cat1varname, Group, Lower_bound, Upper_bound, Suppress) %>%
  mutate(Lower_bound = round(Lower_bound, digits=1),
         Upper_bound = round(Upper_bound, digits=1)) %>% 
  left_join(., death_kc, by = c("indicator_key", "Year", "Tab")) %>%
  
  mutate(Comparison_with_KC = case_when(Suppress == "Y" ~ NA_character_,
                                        Lower_bound > ub_kc ~ "higher",
                                        Upper_bound < lb_kc ~ "lower",
                                        Lower_bound < ub_kc | Upper_bound > lb_kc ~ "no different",
                                        TRUE ~ NA_character_),
         Significance_level = ifelse(Comparison_with_KC == "higher", "*", ifelse(Comparison_with_KC == "lower", "*", NA))) %>% 
  select(indicator_key, Year, Tab, Category1, Cat1varname, Group, Comparison_with_KC, Significance_level)

firearm_death_combined <- left_join(firearm_death, death_trend, by = c("indicator_key", "Year", "Tab", "Category1", "Cat1varname", "Group")) %>%
    select(data_source:rse, Comparison_with_KC, Significance_level, Timetrends:Suppress)


write.xlsx(firearm_death_combined, file = file.path(chi_path, "Death Indicators (all)",
                                            "DEATH_firearm_related_corrected_KC_comp_2013-2017.xlsx"))

death <- read.xlsx(file.path(chi_path, "Death Indicators (all)",
                                     "DEATH_combined_suppress.xlsx"))

death_updated <- death %>% 
    filter(indicator_key != "dth801000") %>% 
    rbind(firearm_death_combined) %>% 
        mutate (Rate = round(Rate, digits=1),
           Lower_bound = round(Lower_bound, digits=1),
           Upper_bound = round(Upper_bound, digits=1))

# QA new indicator
death_QA <- death %>% 
  filter(indicator_key == "dth801000") %>% 
  select(indicator_key:Rate) %>% 
  rename("old_rate" = "Rate") 

# compare new vs. updated indicators for QA using 3-point threshold to flag changes
QA_groups <- left_join(death_QA, firearm_death_combined, by = c("indicator_key", "Tab", "Category1", "Group", "Category2", "Subgroup")) %>% 
  select(indicator_key:Rate) %>% 
  filter(Tab !="Trends") %>% 
  mutate(difference = (Rate-old_rate),
         flag = ifelse(abs(difference) >= 3, 'flag',NA_character_))

#compare trends to make sure rates are same for previous years
QA_trends <- left_join(death_QA, firearm_death_combined, by = c("indicator_key", "Tab", "Year", "Category1", "Group", "Category2", "Subgroup")) %>% 
  filter(Tab=="Trends")  %>% 
  select(indicator_key:Rate) %>% 
  mutate(difference = (Rate-old_rate),
         flag = ifelse(difference != 0, 'flag', NA_character_)) %>% 
  filter(flag=='flag')



list_of_datasets <- list("results" = death_updated, "QA" = QA_groups, "QA trends" = QA_trends)
currentDate <- Sys.Date()
xlsxFileName <- paste0(file.path(chi_path),"/Death Indicators (all)/", "DEATH_combined_suppress_",  currentDate, ".xlsx")
write.xlsx(list_of_datasets, file = xlsxFileName)


# #### BIRTH DATA ####
# #birth_kc <- birth %>%
# #  filter(Tab == "Trends" & Category1 == "King County") %>%
# #  select(indicator_key, Year, Lower_bound, Upper_bound) %>%
#   rename(lb_kc = Lower_bound, ub_kc = Upper_bound)
# 
# birth_trend <- birth %>%
#   filter(Tab == "Trends") %>%
#   select(indicator_key, Year, Category1, Group, Lower_bound, Upper_bound, Suppress) %>%
#   left_join(., birth_kc, by = c("indicator_key", "Year")) %>%
#   mutate(Comparison_with_KC = case_when(Suppress == "Y" ~ NA_character_,
#                                         Lower_bound > ub_kc ~ "higher",
#                                         Upper_bound < lb_kc ~ "lower",
#                                         Lower_bound < ub_kc | Upper_bound > lb_kc ~ "no different",
#                                         TRUE ~ NA_character_)) %>%
#   select(indicator_key, Year, Category1, Group, Comparison_with_KC)
# 
# birth_combined <- left_join(birth, birth_trend, by = c("indicator_key", "Year", "Category1", "Group")) %>%
#   mutate(Comparison_with_KC = ifelse(is.na(Comparison_with_KC.y),
#                                      Comparison_with_KC.x,
#                                      Comparison_with_KC.y),
#          Cat1varname = ifelse(Category1 == "King County regions", "ccreg", Cat1varname)) %>%
#   select(-Comparison_with_KC.x, -Comparison_with_KC.y) %>%
#   select(data_source:rse, Comparison_with_KC, Significance_level:Suppress)
# 
# 
# write.xlsx(birth_combined, file = file.path(chi_path, "birth Indicators (all)",
#                                             "birth_combined_suppress_corrected_KC_comp.xlsx"))
# 
# 
# #### ACS DATA ####
# acs_kc <- acs %>%
#   filter(Tab == "Trends" & Group == "King County") %>%
#   select(indicator, house_type, Year, Lower.bound, Upper.bound) %>%
#   rename(lb_kc = Lower.bound, ub_kc = Upper.bound)
# 
# acs_trend <- acs %>%
#   filter(Tab == "Trends") %>%
#   select(indicator, Year, house_type, Category1, Group, Lower.bound, Upper.bound, Suppression.label) %>%
#   left_join(., acs_kc, by = c("indicator", "Year", "house_type")) %>%
#   mutate(Comparisonwith.KC = case_when(Suppression.label == "^" ~ NA_character_,
#                                         Lower.bound > ub_kc ~ "higher",
#                                         Upper.bound < lb_kc ~ "lower",
#                                         Lower.bound < ub_kc | Upper.bound > lb_kc ~ "no different",
#                                         TRUE ~ NA_character_)) %>%
#   select(indicator, Year, house_type, Category1, Group, Comparisonwith.KC)
# 
# acs_combined <- left_join(acs, acs_trend, by = c("indicator", "Year", "house_type", "Category1", "Group")) %>%
#   mutate(Comparisonwith.KC = ifelse(is.na(Comparisonwith.KC.y),
#                                      Comparisonwith.KC.x,
#                                      Comparisonwith.KC.y)) %>%
#   select(-Comparisonwith.KC.x, -Comparisonwith.KC.y) %>%
#   select(data_source:rse, Comparisonwith.KC, Trend:Denominator) %>%
#   mutate_at(vars(Percent, Lower.bound, Upper.bound),
#             funs(ifelse(Percent > 1, round(., 0), round(., 3)))) %>%
#   mutate(se = ifelse(Percent > 1, round(se, 0), round(se, 4)),
#          rse = round(rse, 1))
# 
# 
# write.xlsx(acs_combined, file = file.path(chi_path, "ACS Indicators (all)",
#                                           "ACS combined_2016 updated data_corrected_KC_comp.xlsx"))
