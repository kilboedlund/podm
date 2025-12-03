library('tidyverse')
setwd('/safe/data/Research projects/PODM')
source('Working data/scripts/00_functions.R')

# Import cause-of-death register (only for date of death) ----------------------
dors <- haven::read_sas('Data from SoS/36620_2021_Lev1/ut_r_dors_36620_2021.sas7bdat') %>% 
  transmute(LopNr, 
            dors_death_date = as.Date.character(DODSDAT, format = "%Y%m%d"))
write_rds(dors, 'Working data/derived/podm_dors.rds')

# Importing patient register ---------------------------------------------------
par_sv <- haven::read_sas('Data from SoS/36620_2021_Lev1/ut_r_par_sv_36620_2021.sas7bdat') %>% 
  unite('DIA', contains('DIA'), sep = ' ') %>% 
  mutate(DIA = DIA %>% str_remove_all('_atc') %>% str_squish) %>% 
  ICD_version() %>% 
  mutate(DIA = str_split(DIA, ' '),
         INDATUMA = as.Date.character(as.character(INDATUMA), format = '%Y%m%d')) %>% 
  unnest(DIA) %>% 
  select(LopNr, INDATUMA, DIA, ICD)

par_ov <- haven::read_sas('Data from SoS/36620_2021_Lev1/ut_r_par_ov_36620_2021.sas7bdat') %>% 
  unite('DIA', contains('DIA'), sep = ' ') %>% 
  mutate(DIA = DIA %>% str_remove_all('_atc') %>% str_squish) %>% 
  ICD_version() %>% 
  mutate(DIA = str_split(DIA, ' '),
         INDATUMA = as.Date.character(as.character(INDATUMA), format = '%Y%m%d')) %>% 
  unnest(DIA) %>% 
  select(LopNr, INDATUMA, DIA, ICD)

# Combine in- and outpatient registers, identify diagnoses
par <- bind_rows(par_sv, par_ov) %>%
  par_diag() %>% 
  group_by(LopNr) %>% 
  summarise(across(c(DM, HPT), ~min(.x, na.rm = T))) %>%
  rename('par_dm_date' = 'DM',
         'par_hpt_date' = 'HPT')

write_rds(par, 'Working data/derived/podm_par.rds')

# Diabetes register ------------------------------------------------------------
ndr <- haven::read_sas('Data from SoS/47043_2024_Lev1/sos_dnr_47043_2024.sas7bdat') %>% 
  select(LopNr, 
         ndr_dm_type = R_DiabetesType, 
         ndr_dm_year = R_YearOfOnset, 
         ndr_dm_date = R_ContactDate) %>% 
  mutate(ndr_dm_type = case_when(ndr_dm_type == 'Typ 2' ~ 'type 2',
                                 ndr_dm_type == 'Diabetestyp Typ 1 (inkl LADA)' ~ 'type 1',
                                 TRUE ~ NA_character_),
         ndr_dm_year = as.integer(ndr_dm_year)) %>% 
  group_by(LopNr) %>% 
  arrange(ndr_dm_date) %>% 
  summarise(ndr_dm_type = min(ndr_dm_type, na.rm = T), 
            ndr_dm_year = min(ndr_dm_year, na.rm = T),
            ndr_dm_date = min(ndr_dm_date, na.rm = T))

write_rds(ndr, 'Working data/derived/podm_ndr.rds')

# Prescribed drug register -----------------------------------------------------
lmed <- haven::read_sas('Data from SoS/47043_2024_Lev1/ut_r_lmed_47043_2024.sas7bdat') %>% 
  group_by(LopNr) %>% 
  summarise(lmed_dm_date = min(FDATUM))

write_rds(lmed, 'Working data/derived/podm_lmed.rds')

# Compare register data --------------------------------------------------------
podm_diag_comp <- read_rds('Working data/derived/podm_base.rds') %>%
  select(LopNr, base_sex, base_exam_date, base_birth_year) %>%
  left_join(read_rds('Working data/derived/podm_par.rds') %>% 
              select(LopNr, par_dm_date)) %>%
  left_join(read_rds('Working data/derived/podm_ndr.rds')) %>%
  left_join(read_rds('Working data/derived/podm_lmed.rds'))

podm_diag_comp_1 <- podm_diag_comp %>% 
  filter(is.na(ndr_dm_type) | ndr_dm_type == 'type 2') %>% 
  transmute(LopNr, 
         par = year(par_dm_date) %>% as.integer, 
         lmed = year(lmed_dm_date) %>% as.integer, 
         ndr = ndr_dm_year) %>% 
  pivot_longer(cols = -LopNr,
               names_to = 'register',
               values_to = 'year') %>%
  group_by(LopNr) %>% 
  filter(!is.na(year)) %>% 
  arrange(year) %>% 
  summarise(order = paste(register, collapse = '-')) %>% 
  filter(!is.na(order))

podm_diag_comp_1 %>%
  group_by(reg_first = str_split_i(order, '-', 1) %>% str_to_upper,
           reg_second = str_split_i(order, '-', 2) %>% str_to_upper) %>% 
  count() %>% 
  ggplot(aes(x = reg_first, y = n, fill = reg_second)) +
  geom_col(colour = 'black') +
  geom_text(aes(label = n), position = position_stack(vjust = .5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 30000)) +
  labs(x = 'First register of appearance', y = 'Number of cases', fill = 'Second register\nof appearance') +
  theme_classic() +
  theme(text = element_text(size = 14))
ggsave(paste0('Output/figures/', Sys.Date(), '_register.png'), width = 8, height = 5)

podm_diag_comp_1 %>%
  mutate(reg = str_split_i(order, '-', 1) %>% str_to_upper()) %>%
  left_join(podm_diag_comp %>% transmute(LopNr, dm_year = pmin(year(par_dm_date), ndr_dm_year, year(lmed_dm_date), na.rm = T))) %>% 
  group_by(dm_year, reg) %>% 
  count() %>% 
  ggplot(aes(x = dm_year, y = n, fill = reg)) +
  geom_col(colour = 'black') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3000)) +
  scale_x_continuous(expand = c(0,0), limits= c(1970.5,2021.5)) +
  labs(x = 'Year of diagnosis', y = 'Number of cases', fill = 'First register\nof appearance') +
  #guide_legend(position = )
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = c(.2, .7))
ggsave(paste0('Output/figures/', Sys.Date(), '_register_time.png'), width = 8, height = 5)

podm_diag_comp_1 %>%
  mutate(reg = str_split_i(order, '-', 1) %>% str_to_upper()) %>%
  left_join(podm_diag_comp %>% transmute(LopNr, dm_year = pmin(year(par_dm_date), ndr_dm_year, year(lmed_dm_date), na.rm = T))) %>%
  left_join(read_rds('Working data/derived/podm_base.rds') %>% select(LopNr, base_birth_year)) %>% 
  mutate(dm_age = dm_year - base_birth_year) %>%
  group_by(dm_age, reg) %>% 
  count() %>% 
  ggplot(aes(x = dm_age, y = n, fill = reg)) +
  geom_col(colour = 'black') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 3000)) +
  scale_x_continuous(expand = c(0,0), limits= c(19.5, 99.5)) +
  labs(x = 'Age at diagnosis', y = 'Number of cases', fill = 'First register\nof appearance') +
  #guide_legend(position = )
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = c(.2, .7))
ggsave(paste0('Output/figures/', Sys.Date(), '_register_age.png'), width = 8, height = 5)



         