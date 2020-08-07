rm(list=ls()) # clear memory
library(data.table)
library(rads)
library(tidyverse)
library(openxlsx)
library(DescTools)

demogs <- read.xlsx("J:/CDC COVID19 impacts eval/SNAP data/EMAPS 5015_New Basic Food Clients Demographic Analysis_External_V8.xlsx", sheet = "King County (3)", startRow = 7, fillMergedCells = T) %>% 
  mutate(Newly.Approved.Clients = as.numeric(Newly.Approved.Clients),
         Ongoing.Clients = as.numeric(Ongoing.Clients))

bf_gender <- demogs %>% filter(Category == 'Gender') 
bf_race <- demogs %>% filter(Category == 'Ethnicity and Race')
bf_age <- demogs %>% filter(Category == 'Age')
bf_marital <- demogs %>% filter(Category == 'Marital Status') 
bf_educ <- demogs %>% filter(Category == 'Education Status')
bf_cit <- demogs %>% filter(Category == 'Citizenship Status')
bf_lang <- demogs %>% filter(Category == 'Primary Language') 
bf_disability <- demogs %>% filter(Category == 'Disability') 
bf_housing <- demogs %>% filter(Category == 'Housing Status') 
bf_prior <- demogs %>% filter(Category == 'Prior SNAP/FAP Receipt7') 

tbls <- mget(ls(pattern = "bf_"))

demogs_ci <- bind_rows(lapply(tbls, function(x){
  tbl <- cbind(x, as.data.frame(
    MultinomCI(x$Newly.Approved.Clients, 
               conf.level=0.95, 
               method="wald"))) %>% 
    rename(est_new = est, lwr.ci_new = lwr.ci, upr.ci_new = upr.ci) 
  
  tbl <- cbind(tbl, as.data.frame(
    MultinomCI(tbl$Ongoing.Clients, 
               conf.level=0.95, 
               method="wald"))) %>% 
    rename(est_old = est, lwr.ci_old = lwr.ci, upr.ci_old = upr.ci) 
  
  tbl <- tbl %>% mutate(significance = case_when(lwr.ci_new > upr.ci_old ~ 'higher',
                                                upr.ci_new < lwr.ci_old ~ 'lower',
                                                TRUE ~ 'no sig difference'))
                        
  
  return(tbl)
}))

write.xlsx(demogs_ci, "J:/CDC COVID19 impacts eval/SNAP data/SNAP_demographics_significance.xlsx", overwrite = T)
