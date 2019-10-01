# Firearm death data
# Written by: Abby Schachter
# September 2019

# Combine raw CHAT output and convert to Tableau-ready format for Gun Violence viz

# packages
library(tidyverse)
library(openxlsx)

# set filepath for CHAT raw output
chat.path <- "//Phshare01/epe_share/WORK/Firearms/GV Data Workplan & Materials 2018-2019/Chart Pack 3/CHAT Raw Output/CHAT Firearm Deaths 2013-2017"
temp <- list.files(path=chat.path, pattern= '*.csv')

#import raw CHAT files as list; remove blank rows and query details
chat <- lapply(file.path(chat.path, temp), function(x){
  a = read.csv(x, header=T, skip=3, sep=",", as.is=T, na.strings=c(""), blank.lines.skip=T)
  a<-a[!is.na(a[,2]),]
  return(a)
})

# convert from list to dataframe
chat_combined <- bind_rows(chat) %>%
  rename(., chat_geo = Geography) %>% 
  mutate(#rename geography column in order to create new column
         chat_geo = trimws(chat_geo),
         
         #make rows unique for Seattle vs. King County geographies
         #chat_geo = ifelse(chat_geo != "King" & !grepl("HRA", chat_geo), make.unique(chat_geo), chat_geo),
         
         #combine rate and CI columns
         Rate = if_else(is.na(Rate), Age.Adjusted.Rate, Rate),
         Lower_bound = if_else(is.na(Lower.CI), Age.Adjusted.Lower.CI, Lower.CI),
         Upper_bound = if_else(is.na(Upper.CI), Age.Adjusted.Upper.CI, Upper.CI),
         
         # remove text from Age column
         Age=gsub("Age Group=", "", Age),
         
         # add columns for data source and indicator key
         data_source = "death",
         indicator_key = "dth801000",
         
         # Suppress counts and rates if count <5 (need to seek exemption from new suppression rules from DOH)
         # Suppress = ifelse(Count %in% (1:4), "Y", "N"),
         # Rate = ifelse(Suppress == "Y", NA_real_, Rate),
         # Count = ifelse(Suppress == "Y", NA_real_, Count),
         # Lower_bound = ifelse(Suppress == "Y", NA_real_, Lower_bound),
         # Upper_bound = ifelse(Suppress == "Y", NA_real_, Upper_bound),
         
         # assign geography
         Geography = case_when(
           chat_geo == "King" ~ "King County",
           #tease out KCSeattleHRA regions vs. Seattle alone
           chat_geo == "KCSeattleHRA" & lag(chat_geo,1) != "KCSeattleHRA" & lead(chat_geo) != "KCSeattleHRA" ~ "King County",
           chat_geo == "KCSeattleHRA" & (lag(chat_geo,1) == "KCSeattleHRA" | lead(chat_geo) == "KCSeattleHRA") ~ "Seattle",
           grepl( "HRA", chat_geo) ~ "King County",
           #Poverty groupings
           grepl("PovCT", chat_geo) ~ "Seattle",
           grepl("KCPov", chat_geo) ~ "King County",
           #tease out Seattle HRAs
           startsWith(chat_geo,"2") & Injury.Intent == "All" ~ "Seattle",
           Injury.Intent %in% c("Suicide","Homicide") ~ "Seattle",
           Injury.Intent %in% c("Assault","Self-Inflicted") ~ "King County",
           TRUE ~ NA_character_),
         
         # rename injury intents
         Injury.Intent = case_when(
           Injury.Intent == "Assault" ~ "Homicide",
           Injury.Intent == "Self-Inflicted" ~ "Suicide",
           TRUE ~ Injury.Intent),
         
         # create category column
         Category1 = case_when(
           Age != 'All' ~ "Age",
           Race != 'All' | (Ethnicity!= "All" & !is.na(Ethnicity)) ~ "Race/Ethnicity",
           Gender != 'All' ~ "Gender",
           grepl("Pov",chat_geo) ~ "Neighborhood Poverty",
           (chat_geo=="KCSeattleHRA" & Age == "All" & Race == "All" & Gender == "All" & Ethnicity == "All" & Geography == "Seattle") ~ "Seattle",
           (grepl("HRA", chat_geo) & Age == "All" & Race == "All" & Gender == "All" & Ethnicity == "All" & Geography == "King County") ~ "King County regions",
           (chat_geo=="King" & Age == "All" & Race == "All" & Gender == "All" & Ethnicity == "All") ~ "King County",
           chat_geo != "King" & !grepl("HRA", chat_geo) ~ "Cities/Neighborhoods",
           TRUE ~ NA_character_), 
         
         # create Tab column
         Tab = case_when(
           Category1 == "King County" ~ "_King County",
           Category1 == "Seattle" ~ "_Seattle",
           TRUE ~ "Subgroups"),
         
         #create Group column
         Group = case_when(
           Category1 == "Age" ~ Age,
           grepl("American Indian", Race) ~ "AIAN",
           grepl("Pacific Islander", Race) ~ "NHPI",
           Category1 == "Race/Ethnicity" & Race != 'All' & grepl(" Only-NH", Race) ~ gsub(" Only-NH", "", Race),
           Category1 == "Race/Ethnicity" & Race != 'All' & grepl("Only", Race) ~ gsub(" Only", "", Race),
           Category1 == "Race/Ethnicity" & Race == "Hispanic as Race" ~ "Hispanic",
           Category1 == "Race/Ethnicity" & Race == "All" ~ Ethnicity,
           Category1 == "Gender" ~ Gender,
           Category1 == "King County" ~ Category1,
           Category1 == "Seattle" ~ Category1,
           Category1 == "King County regions" ~ gsub("KC", "", gsub("HRA","",chat_geo)),
           chat_geo == "Vashon" ~ "Vashon Island",
           Category1 == "Cities/Neighborhoods" ~ trimws(gsub("[[:digit:]]","", chat_geo)),
           Category1 == "Neighborhood Poverty" & grepl("Med", chat_geo) ~ "Medium",
           Category1 == "Neighborhood Poverty" & grepl("High", chat_geo) ~ "High",
           Category1 == "Neighborhood Poverty" & grepl("Low", chat_geo) ~ "Low",
           TRUE ~ NA_character_),
         
         #Group = ifelse(endsWith(Group, "."), gsub(".","", Group), Group),
         Cat1varname = case_when(
           Race == "Hispanic as Race" ~ "race4",
           Category1 == "Race/Ethnicity" & grepl("-NH", Race) ~ "race4",
           Ethnicity == "Hispanic" ~ "race3",
           Category1 == "Race/Ethnicity" ~ "race3",
           Category1 == "Age" ~ "age5",
           Category1 == "Gender" ~ "sex",
           Category1 == "Neighborhood Poverty" ~ "povgrp",
           Category1 == "King County regions" ~ "ccreg",
           Category1 == "Cities/Neighborhoods" ~ "hracode",
           Tab == "_King County" ~ "kingco",
           Tab == "_Seattle" ~ "seattle"
         )
         ) %>% 
  #Remove old rate and CI columns
  select(-Age, -Gender, -Race, -Ethnicity, -chat_geo, -Age.Adjusted.Rate,-Age.Adjusted.Lower.CI,-Age.Adjusted.Upper.CI, -Lower.CI, -Upper.CI, -Injury.Mechanism)

# read in firearm death data (all intents) from CHI
death_chi <- read.xlsx("S:/WORK/CHI Visualizations/Death Indicators (all)/DEATH_combined_suppress_updateddth801000.xlsx", sheet = "results") %>% 
    filter(indicator_key == "dth801000" & Tab != "SmallGroups") %>% 
  select(-Category2, -Subgroup, -Cat2varname) %>% 
  rename(Comparison = Comparison_with_KC,
         Significance = Significance_level) %>% 
  mutate(Geography = "King County",
         Injury.Intent = "All")

# combine CHAT data with CHI data and update HRA names  
death_combined <- bind_rows(chat_combined, death_chi) %>% 
  mutate(Category1 = case_when(
          Category1 %in% c("Race","Ethnicity") ~ "Race/Ethnicity",
          Category1 == "Health Reporting Areas" ~ "Cities/Neighborhoods",
          TRUE ~ Category1),
         Group = case_when(
           grepl("Auburn ", Group) ~ gsub("Auburn ", "Auburn-", Group), 
           grepl("Beacon", Group) ~ "Beacon/Gtown/S.Park",
           Group == "Bear Creek Carnation Duvall" ~ "Bear Creek/Carnation/Duvall",
           grepl("Bellevue ", Group) ~ gsub("Bellevue ", "Bellevue-", Group),
           Group == "Black Diamond Enumclaw SE County" ~ "Black Diamond/Enumclaw/SE County",
           Group == "Bothell Woodinville" ~ "Bothell/Woodinville",
           Group == "Capitol Hill E lake" ~ "Capitol Hill/E.lake",
           Group == "Central" ~ "Central Seattle",
           Group == "Covington Maple Valley" ~  'Covington/Maple Valley',
           Group == "Des Moines Normandy Park" ~ 'Des Moines/Normandy Park',
           grepl("Dash Point", Group)  ~ "Fed Way-Dash Pt",
           Group == "Fed Way Central Military Rd" ~ "Fed Way-Central/Military Rd",
           grepl("Kent ", Group) ~ gsub("Kent ", "Kent-", Group),
           Group == "Fremont Greenlake" ~ "Fremont/Greenlake",
           Group == "Kenmore LFP" ~ "Kenmore/LFP",
           Group == "Mercer Isle Pt Cities" ~ "Mercer Isle/Pt Cities",
           Group == "Newcastle Four Creeks"~ "Newcastle/Four Creeks",
           Group == "QA Magnolia" ~ "QA/Magnolia",
           grepl("Renton ", Group) ~ gsub("Renton ", "Renton-", Group),
           Group == "SeaTac Tukwila" ~ "SeaTac/Tukwila",
           Group == "Snoqualmie North Bend Skykomish" ~ "Snoqualmie/North Bend/Skykomish",
           grepl("Snoqalmie", Group) ~ gsub("Snoqalmie", "Snoqualmie", Group),
           TRUE ~ Group))%>% 
  distinct(.)
  
# bring in city/neighborhood aliases
#library(odbc)
# db_extract51 <- odbc::dbConnect(odbc(), "PHExtractStore51")
# 
# chi_alias <- DBI::dbReadTable(db_extract51, 
#                                        DBI::Id(schema = "APDE_WIP", table = "brfss_results")) 
# 
# chi_alias <- chi_alias %>% 
#   filter(tab == 'demgroups' & cat1_varname == 'hracode') %>% 
#   mutate(cat1=ifelse(cat1 == "Cities/neighborhoods", "Cities/neighborhoods", cat1)) %>% 
#   select(cat1, cat1_group, cat1_group_alias) %>% 
#   unique()
# 
# death_combined <- left_join(death_combined, chi_alias, by=c("Category1"="cat1", "Group" = "cat1_group"))

# compare confidence intervals for significance test



#### DEATH DATA ####
death_kcsea <- death_combined %>%
  filter((Category1 == "King County" & Group == "King County") | (Category1 == "Seattle" & Group == "Seattle")) %>%
  select(Geography, Year, Injury.Intent, Lower_bound, Upper_bound) %>%
  rename(lb = Lower_bound, ub = Upper_bound)


death_comparison <- death_combined %>%
  left_join(., death_kcsea, by = c("Year", "Geography", "Injury.Intent")) %>%  # join death with death_kcsea
  #compare confidence intervals for overlap and create "Comparison" column
  mutate(
    Comparison = case_when(
    Suppress == "^" ~ NA_character_, # DELETE THIS IF WE DECIDE NOT TO USE SUPPRESSION
    Category1 %in% c("King County", "Seattle") & Lower_bound > ub ~ "higher",  # compare to the overall KC or Seattle rate
    !(Category1 %in% c("King County", "Seattle")) & Lower_bound > ub ~ "higher",  # compare subgroup to intent-specific KC or Seattle rate
    Category1 %in% c("King County", "Seattle") & Upper_bound < lb ~ "lower",
    !(Category1 %in% c("King County", "Seattle")) & Upper_bound < lb ~ "lower",
    
    (Category1 %in% c("King County", "Seattle")) & Lower_bound <= ub| Upper_bound >= lb ~ "no different",
    !(Category1 %in% c("King County", "Seattle")) & Lower_bound <= ub | Upper_bound >= lb ~ "no different",
    TRUE ~ NA_character_),
  Significance = ifelse(Comparison == "higher", "*", ifelse(Comparison == "lower", "*", NA))) %>% 
  select(-ub, -lb) %>% 
  distinct(.)




#### QA 2013-2017 data ####

#import 2012-2016 data
death_QA <- read.xlsx("//Phshare01/epe_share/WORK/Firearms/GV Data Workplan & Materials 2018-2019/Chart Pack 1/Analysis - Chart Pack 1/Firearm Mortality Data for Chart Pack 1_comparison.xlsx") %>% 
  select(indicator_key:Suppress) %>% 
  filter(Tab!="Trends") %>% 
  rename("old_rate" = "Rate",
         "Injury.Intent" = "Intent") %>% 
  mutate(Cat1varname = ifelse(Cat1varname == 'race3', 'race4', Cat1varname))


# compare new vs. updated indicators for QA using 3-point threshold to flag changes
QA_groups <- left_join(death_QA, death_comparison, 
                       by = c("indicator_key", "Tab", "Injury.Intent",
                              "Geography", "Category1", "Group", "Cat1varname")) %>% 
  mutate(difference = (Rate-old_rate),
         flag = ifelse(abs(difference) >= 3, 'flag',NA_character_))%>% 
  filter(flag=="flag")

view(QA_groups)


# write output to excel
list_of_datasets <- list("results" = death_comparison, "QA" = QA_groups)
currentDate <- Sys.Date()
xlsxFileName <- paste0("S:/WORK/Firearms/GV Data Workplan & Materials 2018-2019/Chart Pack 1/Analysis - Chart Pack 1/FirearmDeaths_2013-2017_nosuppress.xlsx")
write.xlsx(list_of_datasets, file = xlsxFileName , overwrite = T)


#write.xlsx(death_comparison, "S:/WORK/Firearms/GV Data Workplan & Materials 2018-2019/Chart Pack 1/Analysis - Chart Pack 1/FirearmDeaths_2013-2017.xlsx")
