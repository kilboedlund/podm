library('tidyverse')
setwd('/safe/data/Research projects/PODM')

podm_3 <- read_rds('Working data/derived/podm_3.rds')

# Perform exclusions -----------------------------------------------------------
podm_4 <- podm_3 %>% 
  filter(base_sex == 1,
         year(base_exam_date) - base_birth_year > 15,
         year(base_exam_date) - base_birth_year < 65,
         !is.na(smoke_status),
         !is.na(base_bmi),
         !base_officeworker,
         !is.na(dusts),
         !LopNr %in% read_rds('Working data/derived/podm_reused_pnr.rds')$LopNr,
         is.na(ndr_dm_type) | ndr_dm_type == 'type 2',
         is.na(dm_year) | dm_year >= 1987)

# Transform to long format and prepare an event variable -----------------------
# also attach the civil status and occupational data
podm_5 <- podm_4 %>% 
  expand(LopNr, year = 1971:2021) %>% 
  left_join(podm_4, by = 'LopNr') %>% 
  left_join(readRDS('Working data/derived/podm_civ.rds') %>% 
              mutate(year = as.integer(year)), 
            by = c('LopNr', 'year')) %>% 
  left_join(readRDS('Working data/derived/podm_occ.rds') %>%
              mutate(year = as.integer(year)),
            by = c('LopNr', 'year')) %>% 
  mutate(age = year - base_birth_year) %>% 
  filter(age >= 20, 
         age < 90,
         year > year(base_exam_date),
         year >= 1987,
         year < year(dors_death_date) | is.na(dors_death_date),
         year < year(base_emig_date) | is.na(base_emig_date),
         year <= dm_year | is.na(dm_year),
         year <= 2021) %>% 
  mutate(event = if_else(year == dm_year, 1, 0, missing = 0))

saveRDS(podm_5, 'Working data/derived/podm_5.rds')
