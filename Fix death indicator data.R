library(odbc)

db_store51 <- dbConnect(odbc(), "PHExtractStore51")

# db_store <- dbConnect(odbc::odbc(),
#                          .connection_string = "Driver={SQL Server};Server={KCITSQLUTPDBH51};
#                          Database={PHExtractStore51};Trusted_Connection=True")


tbl_id_results<- DBI::Id(schema = "APDE_WIP", table = "death_results")
tbl_id_meta<-  DBI::Id(schema = "APDE_WIP", table = "death_metadata")
  
death_results <- DBI::dbReadTable(db_store51, tbl_id_results)
death_meta <-DBI::dbReadTable(db_store51, tbl_id_meta)
death_results <- death_results %>% 
  #select(-Caution)
  # mutate(Cat1varname = ifelse(Group=="Hispanic" & indicator_key != 'dth801000', 'race3', Cat1varname)) %>% 
  # select(indicator)
  mutate(rse = ifelse(indicator_key== 'dth801000', 1/sqrt(Count) *100, rse),
         #Caution = ifelse(rse>=.25, "!", NA_character_),
         se = ifelse(indicator_key=='dth801000' & is.na(se), (Upper_bound-Rate)/1.96, se))


dbWriteTable(db_store51, tbl_id_results, death_results, overwrite = T)


chi_path<- "S:/WORK/CHI Visualizations/Death Indicators (all)"

dfs <- list("results" = death_results, "metadata" = death_meta)

write.xlsx(dfs, file.path(chi_path, "/DEATH_combined_suppress_updateddth801000.xlsx"))
