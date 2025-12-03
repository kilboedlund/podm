library('tidyverse')

# Importing JEMs ---------------------------------------------------------------
jem_syss <- haven::read_sas('JEM/syssjem.sas7bdat') %>% 
  rename('jem_syss' = 'syss') %>% 
  rename_with(.cols = -jem_syss, ~paste0('jem_', .x))
jem_ynuv <- haven::read_sas('JEM/ynuvjem1.sas7bdat') %>% 
  rename('jem_ynuv' = 'ynuv') %>% 
  rename_with(.cols = -jem_ynuv, ~paste0('jem_', .x))


