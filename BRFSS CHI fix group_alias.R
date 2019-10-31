library(haven) # Read in data
library(tidyverse) # Manipulate data
library(odbc) # Read to and write from SQL
library(glue) # Safely combine SQL code
library(data.table) # Manipulate data
library(srvyr) # Survey data
library(rlang) # work with quosures

db_extract51 <- dbConnect(odbc(), "PHExtractStore51")
db_extract50 <- dbConnect(odbc(), "PHExtractStore50")

brfss_results <- DBI::dbReadTable(db_extract51, 
                                     DBI::Id(schema = "APDE_WIP", table = "brfss_results"))

aliases <- brfss_results %>% filter(indicator_key != "cholchk5" & cat2 == "Cities/neighborhoods") %>% select(cat2_group, cat2_group_alias) %>% unique


brfss_fixed <-  brfss_results %>% 
  mutate(cat2_group_alias = case_when(
      indicator_key == "cholchk5" ~ aliases$cat2_group_alias[match(cat2_group, aliases$cat2_group)],
      TRUE ~ cat2_group_alias),
    cat1_group_alias = case_when(
      cat1_group == "Kirkland" ~ "Kirkland",
      cat1_group == "Kirkland North" ~ "Kirkland North", 
      TRUE ~ cat1_group_alias
    )
  )

check1 <- brfss_fixed %>% filter(indicator_key == "cholchk5" & cat2 == "Cities/neighborhoods") %>% select(cat2_group, cat2_group_alias) %>% unique

view(check1)

check2 <- brfss_fixed %>% filter(cat1_group %in% c("Kirkland", "Kirkland North")) %>% select(cat1_group, cat1_group_alias) %>% unique
view(check2)

# dbGetQuery(db_extract51,
#            "DELETE APDE_WIP.brfss_results 
#            WHERE indicator_key = 'cholchk5' AND  cat2 = 'Cities/neighborhoods'")


dbWriteTable(db_extract51,
             name = DBI::Id(schema = "APDE_WIP", table = "brfss_results"),
             value = brfss_fixed,
             overwrite = T, append = F)


