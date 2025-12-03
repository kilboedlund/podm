library('tidyverse')
setwd('/safe/data/Research projects/PODM')

# Load base data file ----------------------------------------------------------
podm_base <- read_rds('Working data/derived/podm_base.rds')

# Attach JEM and calculate combined exposures ----------------------------------
source('Working data/scripts/012_data_import_JEM.R')

podm_1 <- bind_rows(
  podm_base %>% 
  filter(!is.na(base_ynuv)) %>% 
  left_join(jem_ynuv %>% select(jem_ynuv, jem_mmmf, jem_asb, jem_dies, jem_svet, jem_asf, jem_kva, jem_woo, jem_cem, jem_bet), 
            by = c('base_ynuv' = 'jem_ynuv')),
  podm_base %>% 
  filter(is.na(base_ynuv)) %>% 
  left_join(jem_syss %>% select(jem_syss, jem_mmmf, jem_asb, jem_dies, jem_svet, jem_asf, jem_kva, jem_woo, jem_cem, jem_bet), 
            by = c('base_syss' = 'jem_syss'))
) %>% 
  mutate(dusts = if_else(jem_dies + jem_svet + jem_kva + jem_cem + jem_bet > 0, 1, 0),
         wood = if_else(jem_woo > 0, 1, 0),
         fibre = if_else(jem_mmmf + jem_asb > 0, 1, 0)) %>% 
  mutate(dusts_x = if_else(dusts + wood + fibre >= 2, NA_integer_, dusts),
         wood_x = if_else(dusts + wood + fibre >= 2, NA_integer_, wood),
         fibre_x = if_else(dusts + wood + fibre >= 2, NA_integer_, fibre)) %>% 
  mutate(across(c(jem_mmmf, jem_asb, jem_dies, jem_svet, jem_asf, jem_kva, jem_woo, jem_cem, jem_bet),
                \(x) if_else(x > 0, 1, 0),
                .names = '{.col}_bin')) %>% 
  rename_with(.cols = contains('_bin'), \(x) str_remove(x, 'jem_'))

# Attach smoking, residence and emigration data --------------------------------
podm_2 <- podm_1 %>% 
  left_join(read_rds('Working data/derived/podm_smoking.rds') %>% 
              select(LopNr, smoke_status, smoke_intensity)) %>% 
  left_join(read_rds('Working data/derived/podm_municipality.rds')) %>% 
  left_join(read_rds('Working data/derived/podm_sei.rds')) %>% 
  left_join(read_rds('Working data/derived/podm_emig.rds'))

# Attach outcome data ----------------------------------------------------------
podm_3 <- podm_2 %>% 
  left_join(read_rds('Working data/derived/podm_dors.rds')) %>% 
  left_join(read_rds('Working data/derived/podm_par.rds')) %>% 
  left_join(read_rds('Working data/derived/podm_lmed.rds'))%>% 
  left_join(read_rds('Working data/derived/podm_ndr.rds')) %>% 
  mutate(dm_year = pmin(year(par_dm_date), 
                        year(lmed_dm_date), 
                        ndr_dm_year, 
                        na.rm = T))

saveRDS(podm_3, 'Working data/derived/podm_3.rds')
