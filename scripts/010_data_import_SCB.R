library('tidyverse')
setwd('/safe/data/Research projects/PODM')
source('Working data/scripts/00_functions.R')

# Importing basis file ---------------------------------------------------------
ls_lev_basfil <-
  haven::read_sas('Data from SCB/258694_899789-2/ls_lev_basfil.sas7bdat') %>%
  mutate(across(
    c('YNUV', 'SYSS', 'YGRNUV', 'SYSSGR'),
    ~ if_else(. %in% c('99', '999'), NA_real_, .)
  )) %>%
  transmute(
    LopNr,
    base_sex = SEX,
    base_reg = UREG,
    base_exam_date = as.Date(as.character(UDAT), format = '%y%m%d'),
    base_height = if_else(LENGTH != 999, LENGTH, NA_real_),
    base_weight = if_else(WEIGHT != 999, WEIGHT, NA_real_),
    base_bmi = base_weight / (base_height / 100) ^ 2,
    base_bmi_cat = case_when(base_bmi < 18.5 ~ 'underweight',
                             base_bmi < 25 ~ '_normal weight',
                             base_bmi < 30 ~ 'overweight',
                             base_bmi < 35 ~ 'obese cl i',
                             base_bmi < 40 ~ 'obese cl ii',
                             base_bmi >= 40 ~ 'obese cl iii'),
    base_ynuv = YNUV,
    base_syss = SYSS,
    base_title = if_else(!is.na(SYSSGR), SYSSGR, YGRNUV) %>% 
      factor(
        levels = 1:22,
        labels = c(
          'Asphalt pavers',
          'Miners',
          'Concrete workers',
          'Carpenters',
          'Bricklayers',
          'Flooring installers',
          'Machine operators',
          'Crane operators',
          'Chauffeurs',
          'Glass workers',
          'Insulators',
          'Refrigeration installers',
          'Pipe workers',
          'Painters',
          'Sheet metal workers',
          'Electricians',
          'Foremen',
          'Office workers',
          'Repairers',
          'Ground workers', 
          'Roofers', 
          'Other'), 
        ordered = T), 
    base_birth_year = fod,
    base_officeworker = if_else(YGRNUV == 18 |
                                  SYSSGR == 18, T, F, missing = F)
  )

write_rds(ls_lev_basfil, 'Working data/derived/podm_base.rds')
#podm_base <- read_rds('Working data/derived/podm_base.rds')

# Importing smoking data files -------------------------------------------------
# import and format smoke data from roktot (new) and Q40 (old) files,
# combine and pick out the first recorded smoking data for each participant
ls_lev_roktot <- haven::read_sas('Data from SCB/258694_899789-2/ls_lev_roktot.sas7bdat') %>% 
  mutate(udat = as.Date(as.character(udat), format = '%y%m%d'),
         across(-'udat', 
                ~if_else(. %in% c('99', '999'), NA_real_, .)),
         smoke_status = case_when(cigyr == 0 & cigst == 0 & cgaryr == 0 & cgast == 0 & pipyr == 0 & pipst == 0 ~ '_never',
                         ((cigyr != 0 & cigst != 0) | (cigyr == 0 & cigst == 0)) &
                          ((cgaryr != 0 & cgast != 0) | (cgaryr == 0 & cgast == 0)) &
                          ((pipyr != 0 & pipst != 0) | (pipyr == 0 & pipst == 0)) ~ 'former',
                         (cigyr != 0 | cgaryr != 0 | pipyr != 0) & cigst == 0 & cgast == 0 & pipst == 0 ~ 'current'),
         rok_cig = case_when(cigant == 0 ~ 0,
                             cigant >= 1 & cigant <= 4 ~ 1,
                             cigant >= 5 & cigant <= 14 ~ 2,
                             cigant >= 15 & cigant <= 24 ~ 3,
                             cigant >= 25 ~ 4),
         rok_cga = case_when(cgant == 0 ~ 0,
                             cgant >= 1 & cgant <= 2 ~ 1,
                             cgant >= 3 & cgant <= 7 ~ 2,
                             cgant >= 8 & cgant <= 12 ~ 3,
                             cgant >= 13 ~ 4),
         rok_pip = case_when(pipvol == 0 ~ 0,
                             pipvol > 0 & pipvol < 30 ~ 1,
                             pipvol >= 30 & pipvol < 100 ~ 2,
                             pipvol >= 100 ~ 3),
         smoke_intensity = case_when(rok_cig + rok_cga + rok_pip == 0 ~ '_none',
                                     rok_cig + rok_cga + rok_pip <= 2 ~ 'light',
                                     rok_cig + rok_cga + rok_pip <= 3 ~ 'medium',
                                     rok_cig + rok_cga + rok_pip >= 4 ~ 'heavy')) %>% 
  select(LopNr, unr, udat, smoke_status, smoke_intensity)

ls_lev_bygghalsan_q40 <- haven::read_sas('Data from SCB/258694_899789-2/ls_lev_bygghalsan_q40.sas7bdat') %>%
  mutate(across(-'udat', as.integer),
         udat = as.Date.character(udat, format = '%y%m%d'),
         unr = if_else(unr > 100, unr-100, unr)) %>%
  rename(rok_dagl = F055,
         rok_tidg = F056,
         rok_hals = F057,
         rok_cig_1_4 = F058,
         rok_cig_5_14 = F059,
         rok_cig_15_24 = F060,
         rok_cig_25 = F061,
         rok_pip_30 = F062,
         rok_pip_30_100 = F063,
         rok_pip_100 = F064,
         rok_cga_1_2 = F065,
         rok_cga_3_7 = F066,
         rok_cga_8_12 = F067,
         rok_cga_13 = F068,
         snus = F071) %>%
  mutate(smoke_status = case_when(rok_dagl == 1 ~ 'current',
                                  rok_tidg == 1 ~ 'former',
                                  rok_dagl == 0 & rok_tidg == 0 ~ '_never'),
         rok_cig = case_when(rok_cig_1_4 == 0 & rok_cig_5_14 == 0 & rok_cig_15_24 == 0 & rok_cig_25 == 0 ~ 0,
                             rok_cig_1_4 == 1 ~ 1,
                             rok_cig_5_14 == 1 ~ 2,
                             rok_cig_15_24 == 1 ~ 3,
                             rok_cig_25 == 1 ~ 4),
         rok_cga = case_when(rok_cga_1_2 == 0 & rok_cga_3_7 == 0 & rok_cga_8_12 == 0 & rok_cga_13 == 0 ~ 0,
                             rok_cga_1_2 == 1 ~ 1,
                             rok_cga_3_7 == 1 ~ 2,
                             rok_cga_8_12 == 1 ~ 3,
                             rok_cga_13 == 1 ~ 4),
         rok_pip = case_when(rok_pip_30 == 0 & rok_pip_30_100 == 0 & rok_pip_100 == 0 ~ 0,
                             rok_pip_30 == 1 ~ 1,
                             rok_pip_30_100 == 1 ~ 2,
                             rok_pip_100 == 1 ~ 3),
         smoke_intensity = case_when(rok_cig + rok_cga + rok_pip == 0 ~ '_none',
                                     rok_cig + rok_cga + rok_pip <= 2 ~ 'light',
                                     rok_cig + rok_cga + rok_pip == 3 ~ 'medium',
                                     rok_cig + rok_cga + rok_pip >= 4 ~ 'heavy')) %>% 
  select(LopNr, unr, udat, smoke_status, smoke_intensity)

podm_smoking <- bind_rows(ls_lev_bygghalsan_q40, ls_lev_roktot) %>% 
  group_by(LopNr) %>% 
  arrange(unr) %>% 
  summarise(across(everything(), ~first(na.omit(.x))))
write_rds(podm_smoking, 'Working data/derived/podm_smoking.rds')

#podm_smoking <- read_rds('Working data/derived/podm_smoking.rds')

# graph smoke habit change among workers with repeated smoking data
smoke_change <- bind_rows(ls_lev_bygghalsan_q40, ls_lev_roktot) %>%  
  filter(!is.na(smoke_status)) %>% 
  group_by(LopNr) %>% 
  filter(n() > 1) %>% 
  group_by(LopNr) %>% 
  arrange(unr) %>% 
  summarise(first = first(smoke_status), 
            last = last(smoke_status), 
            time = as.numeric(last(udat)-first(udat))) %>%
  mutate(change = case_when(first == last ~ 'unchanged', 
                            first == 'current' & last == 'former' ~ 'stopped', 
                            first != '_never' & last == '_never' ~ 'forgot', 
                            first == '_never' & last != '_never' ~ 'started', 
                            first == 'former' & last == 'current' ~ 'relapsed'))

smoke_change %>% 
  ggplot(aes(x = change, fill = first)) + 
  geom_bar() + 
  scale_y_continuous(expand = c(0,0), limits = c(0,175000)) + 
  labs(y = 'Count', x = 'Change in smoking status from\nfirst to last occ health exam', fill = 'Smoking status\nat first visit') + 
  theme_classic() +
  guides(fill = guide_legend(position = 'inside')) +
  theme(legend.position.inside = c(.2, .5))
  
smoke_change %>% 
  ggplot(aes(x = change, y = time/365.25)) +
  geom_violin(fill = 'lightgrey') +
  scale_y_continuous(expand = c(0,0), limits = c(0,23)) + 
  labs(y = 'Years between exams', x = 'Change in smoking status from\nfirst to last occ health exam') +
  theme_classic()

# Importing socioeconomic data -------------------------------------------------
ls_lev_fob <- full_join(
  haven::read_sas('Data from SCB/258694_899789-2/ls_lev_fob_1980.sas7bdat') %>% 
    select(LopNr, SEI) %>% 
    group_by(LopNr) %>% 
    summarise(SEI_1980 = first(SEI)),
  haven::read_sas('Data from SCB/258694_899789-2/ls_lev_fob_1985.sas7bdat') %>% 
    select(LopNr, SEI) %>% 
    group_by(LopNr) %>% 
    summarise(SEI_1985 = first(SEI)),
  by = 'LopNr'
) %>% 
  transmute(LopNr,
            base_sei = if_else(SEI_1985 == '', SEI_1980, SEI_1985) %>% 
              as.integer) %>% 
  mutate(base_sei = case_when(base_sei %in% c(11, 12) ~ '_manual worker, unskilled',
                              base_sei %in% c(21, 22) ~ 'manual worker, skilled',
                              base_sei %in% c(33, 36, 46, 56, 57) ~ 'non-manual worker',
                              base_sei == 60 ~ 'self-employed academic',
                              base_sei == 79 ~ 'self-employed',
                              base_sei == 89 ~ 'farmer',
                              TRUE ~ NA_character_))
write_rds(ls_lev_fob, 'Working data/derived/podm_sei.rds')

# Importing residential and civil status history -------------------------------
# and calculate year of first complete register data
ls_lev_rtb <- multi_read_sas('Data from SCB/258694_899789-2//ls_lev_rtb_', 
                             1971:1989, 
                             '.sas7bdat') %>%
  lapply(function(x) transmute(x,
                            LopNr = as.integer(LopNr),
                            munic = as.integer(Kommun),
                            civil = case_when(
                              Civil %in% c(1, 8, 9) ~ 'unmarried',
                              Civil %in% c(2, 3, 7) ~ '_married',
                              Civil == 4            ~ 'separated',
                              Civil == 5            ~ 'widow(er)',
                              Civil == 6            ~ NA_character_)
                            )) %>%
  imap(\(x, y) rename_with(x, ~paste(., y, sep = '_'), -LopNr)) %>%
  reduce(left_join, by = 'LopNr')

ls_lev_lisa <- multi_read_sas('Data from SCB/258694_899789-2//ls_lev_lisa_',
                              1990:2020,
                              '.sas7bdat') %>%
  lapply(function(x)
    transmute(
      x,
      LopNr = as.integer(LopNr),
      munic = as.integer(Kommun),
      civil = case_when(
        Civil == '\u00c4' | Civil == 'EP' ~ 'widow(er)',
        Civil == 'G' | Civil == 'RP'      ~ '_married',
        Civil == 'OG'                     ~ 'unmarried',
        Civil == 'S' | Civil == 'SP'      ~ 'separated'
      )
    )) %>%
  imap(\(x, y) rename_with(x, ~ paste(., y, sep = '_'), -LopNr)) %>%
  reduce(left_join, by = 'LopNr')

podm_municipality <- full_join(
  ls_lev_rtb %>% select(LopNr, contains('munic')) %>% 
    filter(!(LopNr %in% c(287402, 35048, 35217, 321226, 81598))),
  ls_lev_lisa %>% select(LopNr, contains('munic')),
  by = 'LopNr') %>%
  pivot_longer(contains('munic_'),
               names_to = 'year',
               names_prefix = 'munic_',
               values_to = 'municipality') %>%
  mutate(
    year_par = case_when(
      floor(municipality / 100) == 1 ~ 1972,      # Stockholms län
      floor(municipality / 100) == 3 ~ 1964,      # Uppsala län
      floor(municipality / 100) == 4 ~ 1976,      # Södermanlands län
      floor(municipality / 100) == 5 ~ 1981,      # Östergötlands län
      floor(municipality / 100) == 6 ~ 1982,      # Jönköpings län
      floor(municipality / 100) == 7 ~ 1987,      # Kronobergs län
      floor(municipality / 100) == 8 ~ 1974,      # Kalmar län
      floor(municipality / 100) == 9 ~ 1974,      # Gotlands län
      floor(municipality / 100) == 10 ~ 1984,     # Blekinge län
      floor(municipality / 100) == 11 ~ 1975,     # Kristianstad län
      floor(municipality / 100) == 12 ~ 1970,     # Malmöhus län
      floor(municipality / 100) == 13 ~ 1974,     # Hallands län
      municipality == 1480 ~ 1977,                # Göteborgs stad (sep health region)
      floor(municipality / 100) == 14 ~ 1986,     # Bohus sjukvårdsområde
      floor(municipality / 100) == 15 ~ 1977,     # Älvsborgs län
      floor(municipality / 100) == 16 ~ 1970,     # Skaraborgs län
      floor(municipality / 100) == 17 ~ 1984,     # Värmlands län
      floor(municipality / 100) == 18 ~ 1975,     # Örebro län
      floor(municipality / 100) == 19 ~ 1985,     # Västmanlands län
      floor(municipality / 100) == 20 ~ 1985,     # Dalarnas län
      floor(municipality / 100) == 21 ~ 1964,     # Gävleborgs län
      floor(municipality / 100) == 22 ~ 1984,     # Västernorrlands län
      floor(municipality / 100) == 23 ~ 1985,     # Jämtlands län
      floor(municipality / 100) == 24 ~ 1984,     # Västerbottens län
      floor(municipality / 100) == 25 ~ 1984,     # Norrbottens län
      TRUE ~ 1987)) %>%
  group_by(LopNr) %>%
  summarise(year_par = max(year_par, na.rm = T)) %>%
  ungroup()
write_rds(podm_municipality, 'Working data/derived/podm_municipality.rds')

podm_civ <- full_join(
  ls_lev_rtb %>% select(LopNr, contains('civil')),
  ls_lev_lisa %>% select(LopNr, contains('civil')),
  by = 'LopNr') %>%
  pivot_longer(contains('civil_'),
               names_to = 'year',
               names_prefix = 'civil_',
               values_to = 'base_civil')
saveRDS(podm_civ, 'Working data/derived/podm_civ.rds')

# graph civil status over time
options(scipen = 1000)
podm_civ %>% 
  filter(!is.na(base_civil)) %>% 
  group_by(year, base_civil) %>% 
  count() %>% 
  ggplot(aes(x = as.integer(year), y = n, fill = base_civil)) +
  geom_area(stat = 'identity', colour = 'black') +
  labs(x = 'Year', y = NULL, fill = 'Civil status') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 400000)) +
  theme_classic()

podm_civ %>% 
  group_by(year, base_civil) %>% 
  count() %>% 
  ggplot(aes(x = as.integer(year), y = n, colour = base_civil)) +
  geom_line(linewidth = 2) +
  labs(x = 'Year', y = NULL, colour = 'Civil status') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 400000)) +
  theme_classic()

# Use LISA data to determine work in- or outside construction sector -----------
ls_lev_syssreg <- multi_read_sas(
  'Data from SCB/258694_899789-2//ls_lev_syssreg_', 
  1985:1989, 
  '.sas7bdat') %>% 
  imap(\(x, y) rename_with(x, ~paste(., y, sep = '_'), -LopNr)) %>% 
  reduce(left_join, by = 'LopNr')

ls_lev_lisa <- multi_read_sas(
  'Data from SCB/258694_899789-2//ls_lev_lisa_',
  1990:2020,
  '.sas7bdat') %>% 
  imap(\(x, y) rename_with(x, ~paste(., y, sep = '_'), -LopNr)) %>% 
  reduce(left_join, by = 'LopNr')

SNI_keys <- readxl::read_xlsx(path = 'Data from SCB/SNI keys/nyckel-sni69-sni92.xlsx') %>%
  select('SNI 69', 'SNI 92') %>%
  rename(SNI69 = 'SNI 69', SNI92 = 'SNI 92') %>%
  mutate(across(everything(), as.character)) %>%
  full_join(readxl::read_xlsx(path = 'Data from SCB/SNI keys/nyckel-sni92-sni2002.xlsx') %>%
              select('SNI 92', 'SNI 2002') %>%
              rename(SNI92 = 'SNI 92', SNI02 = 'SNI 2002') %>%
              mutate(across(everything(), as.character)),
            by = 'SNI92') %>%
  full_join(readxl::read_xlsx(path = 'Data from SCB/SNI keys/nyckel-sni2002-sni2007.xlsx') %>%
              select('SNI 2002', 'SNI 2007') %>%
              rename(SNI02 = 'SNI 2002', SNI07 = 'SNI 2007') %>%
              mutate(across(everything(), as.character)),
            by = 'SNI02') %>%
  mutate(across(everything(), as.integer)) %>%
  filter(SNI69 %/% 10000 == 5) #SNI69 %/% 10000 == 2 | SNI69 %/% 10000 == 3 | SNI69 %/% 10000 == 4 | SNI69 %in% c(61113, 61114, 61115, 61116, 61117, 61119, 61120, 61413, 61414, 61415, 61416, 61417))

podm_AstSNI69 <- full_join(ls_lev_syssreg %>% select(LopNr, contains('AstSNI69')),
                           ls_lev_lisa %>% select(LopNr, contains('AstSNI69')),
                           by = 'LopNr') %>%
  pivot_longer(-LopNr, 
               names_to = 'year', 
               names_prefix = 'AstSNI69_', 
               values_to = 'SNI69') %>%
  transmute(LopNr,
            year = as.integer(year),
            base_occ = case_when(as.integer(SNI69) %in% SNI_keys$SNI69 ~ 'construction ind',
                                 is.na(SNI69) | as.integer(SNI69) %in% c(0, 99000) ~ 'non-working',
                                 TRUE ~ 'other ind'))

podm_AstSNI92 <- full_join(ls_lev_syssreg %>% select(LopNr, contains('AstSNI92_')),
                           ls_lev_lisa %>% select(LopNr, contains('AstSNI92_')),
                           by = 'LopNr') %>%
  pivot_longer(-LopNr, names_to = 'year', names_prefix = 'AstSNI92_', values_to = 'SNI92') %>%
  transmute(LopNr,
            year = as.integer(year),
            base_occ = case_when(as.integer(SNI92) %in% SNI_keys$SNI92 ~ 'construction ind',
                               is.na(SNI92) | as.integer(SNI92) %in% c(0, 99000) ~ 'non-working',
                               TRUE ~ 'other ind'))

podm_AstSNI02 <- select(ls_lev_lisa, LopNr, contains('AstSNI2002_')) %>%
  pivot_longer(-LopNr, names_to = 'year', names_prefix = 'AstSNI2002_', values_to = 'SNI02') %>%
  transmute(LopNr,
            year = as.integer(year),
            base_occ = case_when(as.integer(SNI02) %in% SNI_keys$SNI02 ~ 'construction ind',
                               is.na(SNI02) | as.integer(SNI02) %in% c(0, 99000) ~ 'non-working',
                               TRUE ~ 'other ind'))

podm_AstSNI07 <- select(ls_lev_lisa, LopNr, contains('AstSNI2007_')) %>%
  pivot_longer(-LopNr, names_to = 'year', names_prefix = 'AstSNI2007_', values_to = 'SNI07') %>%
  transmute(LopNr,
            year = as.integer(year),
            base_occ = case_when(as.integer(SNI07) %in% SNI_keys$SNI07 ~ 'construction ind',
                            is.na(SNI07) | as.integer(SNI07) %in% c(0, 99000) ~ 'non-working',
                            TRUE ~ 'other ind'))

podm_occ <- bind_rows(podm_AstSNI69 %>% filter(year %in% 1985:1991),
                      podm_AstSNI92 %>% filter(year %in% 1992:2001),
                      podm_AstSNI02 %>% filter(year %in% 2002:2006),
                      podm_AstSNI07 %>% filter(year %in% 2007:2022)) %>%
  arrange(LopNr, year) %>% 
  mutate(base_occ2 = if_else(base_occ %in% c('construction ind', 'other ind'), 'working', base_occ))

saveRDS(podm_occ, 'Working data/derived/podm_occ.rds')

# A failed attempt at mapping the SNI codes
a <- full_join(ls_lev_syssreg %>% 
                 select(LopNr, paste('AstSNI69', 1985:1989, sep = '_')),
               ls_lev_lisa %>% 
                 select(LopNr,
                        paste('AstSNI69', 1990:1993, sep = '_'),
                        paste('AstSNI92', 1992:2001, sep = '_'),
                        paste('AstSNI2002', 2002:2006, sep = '_'),
                        paste('AstSNI2007', 2007:2020, sep = '_')),
               by = 'LopNr') %>% 
  pivot_longer(cols = -LopNr, 
               names_to = c('codebook', 'year'), 
               names_sep = '_', 
               values_to = 'code') %>% 
  mutate(codebook = str_remove(codebook, 'Ast') %>% str_remove('20'), 
         year = as.integer(year),
         code = as.integer(code))

b <- list(a %>% 
            filter(codebook == 'SNI69') %>% 
            mutate(SNI69 = code),
          a %>% 
            filter(codebook == 'SNI92') %>% 
            left_join(SNI_keys %>% 
                        select(SNI92, SNI69) %>% 
                        mutate(SNI69 = trunc(SNI69/10000)) %>% 
                        distinct(),
                      by = c('code' = 'SNI92')),
          a %>% 
            filter(codebook == 'SNI02') %>% 
            left_join(SNI_keys %>% select(SNI02, SNI69) %>% 
                        mutate(SNI69 = trunc(SNI69/10000)) %>% 
                        distinct(),
                      by = c('code' = 'SNI02')),
          a %>% 
            filter(codebook == 'SNI07') %>% 
            left_join(SNI_keys %>% select(SNI07, SNI69) %>% 
                        mutate(SNI69 = trunc(SNI69/10000)) %>% 
                        distinct() %>% view,
                      by = c('code' = 'SNI07'))
          ) %>% 
  bind_rows()

# Import immigration and emigration data ---------------------------------------
ls_lev_emig <- haven::read_sas('Data from SCB/258694_899789-2//ls_lev_migrationer.sas7bdat') %>% 
  filter(Posttyp == 'Utv') %>% 
  transmute(LopNr,
            base_emig_date = as.Date.character(Datum, format = '%Y%m%d')) %>% 
  group_by(LopNr) %>% 
  summarise(base_emig_date = min(base_emig_date))
write_rds(ls_lev_emig, 'Working data/derived/podm_emig.rds')


ls_lev_immig <- haven::read_sas('Data from SCB/258694_899789-2//ls_lev_migrationer.sas7bdat') %>%
  transmute(LopNr,
            Posttyp,
            base_immig_date = as.Date.character(Datum, format = '%Y%m%d'),
            base_mig_origin = Varldsdelnamn_EU27_2020) %>% 
  group_by(LopNr) %>% 
  arrange(base_immig_date) %>% 
  filter(first(Posttyp) == 'Inv') %>% 
  slice_head(n = 1)
saveRDS(ls_lev_immig, 'Working data/derived/podm_immig.rds')

# Import data on reused personal numbers ---------------------------------------
ls_lev_aterpnr <- haven::read_sas('Data from SCB/258694_899789-2/ls_lev_aterpnr.sas7bdat') %>% 
  write_rds('Working data/derived/podm_reused_pnr.rds')
  
