library(tidyverse)
setwd('/safe/data/Research projects/PODM')

source('Working data/scripts/00_functions.R')

podm_3 <- read_rds('Working data/derived/podm_3.rds')
podm_5 <- read_rds('Working data/derived/podm_5.rds')

# Create table 1 ---------------------------------------------------------------
t1_df <- podm_3 %>% 
  filter(LopNr %in% podm_5$LopNr) %>% 
  left_join(podm_5 %>% filter(age == 50) %>% select(LopNr, base_civil), 
            by = 'LopNr')

bind_rows(
  t1_df %>% 
    table1_podm() %>% 
    mutate(expo = 'all'),
  
  t1_df %>% 
    filter(dusts == 0, wood == 0, fibre == 0) %>% 
    table1_podm() %>% 
    mutate(expo = 'unexposed'),
  
  t1_df %>% 
    filter(dusts == 1)  %>% 
    table1_podm() %>% 
    mutate(expo = 'dusts'),
  
  t1_df %>% 
    filter(wood == 1) %>% 
    table1_podm() %>% 
    mutate(expo = 'wood'),
  
  t1_df %>% 
    filter(fibre == 1)  %>% 
    table1_podm() %>% 
    mutate(expo = 'fibre')) %>% 
  data.table::transpose(keep.names = 'var', make.names = 'expo') %>% 
  writexl::write_xlsx(paste0('Output/', Sys.Date(), '_table1.xlsx'))
  

# Create table 2 ---------------------------------------------------------------

source('Working data/scripts/012_data_import_JEM.R')

t2_df <- bind_rows(
  read_rds('Working data/derived/podm_base.rds') %>%
    filter(LopNr %in% podm_5$LopNr) %>%
    filter(!is.na(base_ynuv)) %>%
    left_join(
      jem_ynuv %>% select(
        jem_ynuv,
        jem_mmmf,
        jem_asb,
        jem_dies,
        jem_svet,
        jem_asf,
        jem_kva,
        jem_woo,
        jem_cem,
        jem_bet
      ),
      by = c('base_ynuv' = 'jem_ynuv')
    ),
  read_rds('Working data/derived/podm_base.rds') %>%
    filter(LopNr %in% podm_5$LopNr)  %>%
    filter(is.na(base_ynuv)) %>%
    left_join(
      jem_syss %>% select(
        jem_syss,
        jem_mmmf,
        jem_asb,
        jem_dies,
        jem_svet,
        jem_asf,
        jem_kva,
        jem_woo,
        jem_cem,
        jem_bet
      ),
      by = c('base_syss' = 'jem_syss')
    )
) %>%
  select(contains('jem')) %>%
  summarise(
    across(
      -jem_dies,
      .fns = list(
        none = ~ sum(.x == 0),
        low  = ~ sum(.x == 10),
        high = ~ sum(.x >= 20)
      ),
      .names = '{.col} {.fn}'
    ),
    across(
      jem_dies,
      .fns = list(
        none = ~ sum(.x == 0),
        low  = ~ sum(.x == 5),
        high = ~ sum(.x >= 10)
      ),
      .names = '{.col} {.fn}'
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c('exposure', 'grade'),
    names_sep = ' ',
    values_to = 'number'
  ) %>% 
  pivot_wider(id_cols = 'exposure',
              names_from = 'grade',
              values_from = 'number') %>%
  mutate(exposure = factor(
    exposure,
    levels = paste(
      'jem',
      list('cem', 'bet', 'kva', 'svet', 'dies', 'asf', 'woo', 'asb', 'mmmf'),
      sep = '_'
    ),
    ordered = T
  )) %>%
  arrange(exposure)

writexl::write_xlsx(t2_df, paste0('Output/', Sys.Date(), '_table2.xlsx'))

# Loose figures in the Results text --------------------------------------------
# Number excluded
podm_3 %>% 
  ungroup() %>% 
  summarise(women = sum(base_sex == 2, na.rm = T),
            under15 = sum(year(base_exam_date) - base_birth_year <= 15),
            over65 = sum(year(base_exam_date) - base_birth_year >= 65),
            nosmokedata = sum(is.na(smoke_status)),
            noanthrodata = sum(is.na(base_bmi)),
            officeworker = sum(base_officeworker),
            noexpodata = sum(is.na(dusts)),
            reusedpnr = sum(LopNr %in% read_rds('Working data/derived/podm_reused_pnr.rds')$LopNr),
            type1dm = sum(ndr_dm_type == 'type 1', na.rm = T),
            earlydm = sum(dm_year < 1987, na.rm = T)) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(proportion = 100 * value / 389132)

# Number included
length(unique(podm_4$LopNr))
length(unique(podm_4$LopNr))/389132

length(unique(podm_5$LopNr))
length(unique(podm_5$LopNr))/389132

length(unique(podm_4$LopNr)) - length(unique(podm_5$LopNr))
(length(unique(podm_4$LopNr)) - length(unique(podm_5$LopNr))) / 389132

# Number or workers in >1 exposure group
podm_3 %>% 
  filter(LopNr %in% podm_5$LopNr) %>% 
  summarise(sum(dusts + wood + fibre > 1, na.rm = T))

# Share of exposed workers in >1 exposure group
podm_3 %>% 
  filter(LopNr %in% podm_5$LopNr,
         dusts + wood + fibre > 0) %>% 
  summarise(mean(dusts + wood + fibre > 1, na.rm = T))

# Share of exposed workers grade >= 3
podm_3 %>%
  filter(LopNr %in% podm_5$LopNr,
         dusts + wood + fibre > 0) %>%
  summarise(mean(
      jem_mmmf >= 30 |
      jem_asf >= 30 |
      jem_dies >= 30 |
      jem_svet >= 30 |
      jem_asf >= 30 |
      jem_kva >= 30 |
      jem_woo >= 30 |
      jem_cem >= 30 |
      jem_bet >= 30,
    na.rm = T
  ))

# Mean follow-up time and incident cases ---------------------------------------
podm_5 %>% 
  group_by(LopNr) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(median(n))

# Wherefrom come the DM diagnoses? ---------------------------------------------
podm_5 %>% 
  select(LopNr, base_exam_date, base_birth_year, par_dm_date, ndr_dm_year, lmed_dm_date, dm_year) %>% 
  distinct() %>% 
  filter(dm_year <= 2021, 
         dm_year - base_birth_year < 90, 
         dm_year - base_birth_year >= 20) %>%   
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
  filter(!is.na(order)) %>%
  group_by(reg_first = str_split_i(order, '-', 1) %>% str_to_upper,
           reg_second = str_split_i(order, '-', 2) %>% str_to_upper) %>% 
  count() %>% 
  ggplot(aes(x = reg_first, y = n, fill = reg_second)) +
  geom_col(colour = 'black') +
  geom_text(aes(label = n), position = position_stack(vjust = .5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 20000)) +
  labs(x = 'First register of appearance', y = 'Number of cases', fill = 'Second register\nof appearance') +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = 'none',
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))
ggsave(paste0('Output/figures/', Sys.Date(), '_register.svg'), width = 8, height = 5)

figS1A <- podm_5 %>% 
  select(LopNr, base_exam_date, base_birth_year, par_dm_date, ndr_dm_year, lmed_dm_date, dm_year) %>% 
  distinct() %>% 
  filter(dm_year <= 2021, 
         dm_year - base_birth_year < 90, 
         dm_year - base_birth_year >= 20) %>% 
  transmute(LopNr, 
            dm_year,
            par = year(par_dm_date) %>% as.integer, 
            lmed = year(lmed_dm_date) %>% as.integer, 
            ndr = ndr_dm_year) %>% 
  pivot_longer(cols = -c(LopNr, dm_year),
               names_to = 'register',
               values_to = 'year') %>%
  group_by(LopNr) %>% 
  filter(!is.na(year)) %>% 
  arrange(year) %>% 
  summarise(order = paste(register, collapse = '-'), dm_year = first(dm_year)) %>%
  filter(!is.na(order)) %>%
  mutate(reg = str_split_i(order, '-', 1) %>% str_to_upper()) %>%
  group_by(dm_year, reg) %>% 
  count() %>% 
  ggplot(aes(x = dm_year, y = n, fill = reg)) +
  geom_col(colour = 'black') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2500)) +
  scale_x_continuous(expand = c(0,0), limits= c(1986.5,2021.5)) +
  scale_fill_manual(values = c('grey90', 'grey50', 'grey10')) +
  labs(x = 'Year of diagnosis', y = 'Number of cases', fill = 'Register') +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = 'none',
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))
ggsave(paste0('Output/figures/', Sys.Date(), '_register_time.svg'), width = 8, height = 5)

figS1B <- podm_5 %>% 
  select(LopNr, base_exam_date, base_birth_year, par_dm_date, ndr_dm_year, lmed_dm_date, dm_year) %>% 
  distinct() %>% 
  filter(dm_year <= 2021, 
         dm_year - base_birth_year < 90, 
         dm_year - base_birth_year >= 20) %>% 
  transmute(LopNr,
            dm_age = dm_year - base_birth_year,
            par = year(par_dm_date) %>% as.integer, 
            lmed = year(lmed_dm_date) %>% as.integer, 
            ndr = ndr_dm_year) %>% 
  pivot_longer(cols = -c(LopNr, dm_age),
               names_to = 'register',
               values_to = 'year') %>%
  group_by(LopNr) %>% 
  filter(!is.na(year)) %>% 
  arrange(year) %>% 
  summarise(order = paste(register, collapse = '-'), dm_age = first(dm_age)) %>%
  filter(!is.na(order)) %>% 
  mutate(reg = str_split_i(order, '-', 1) %>% str_to_upper()) %>%
  group_by(dm_age, reg) %>% 
  count() %>% 
  ggplot(aes(x = dm_age, y = n, fill = reg)) +
  geom_col(colour = 'black') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1500)) +
  scale_x_continuous(expand = c(0,0), limits= c(19.5, 89.5)) +
  scale_fill_manual(values = c('grey90', 'grey50', 'grey10')) +
  labs(x = 'Age at diagnosis', y = 'Number of cases', fill = 'Register') +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = 'right',
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))
ggsave(paste0('Output/figures/', Sys.Date(), '_register_age.svg'), width = 8, height = 5)

cowplot::plot_grid(fig1a, fig1b, nrow = 1, rel_widths = c(.4, .6), labels = 'AUTO') %>% 
  ggsave(filename = paste0('Output/figures/', Sys.Date(), '_register_comb.pdf'), width = 12, height = 5)

# Crude incidence data ---------------------------------------------------------
podm_5 %>% summarise(cases = sum(event, na.rm = T))
podm_5 %>% summarise(py = n())
podm_5 %>% summarise(inc = sum(event, na.rm = T)/n())
podm_5 %>% filter(dusts + wood + fibre == 0) %>% 
  summarise(inc = sum(event, na.rm = T)/n(),
            cas = sum(event, na.rm = T))
podm_5 %>% filter(dusts == 1) %>% 
  summarise(inc = sum(event, na.rm = T)/n(),
            cas = sum(event, na.rm = T))
podm_5 %>% filter(wood == 1) %>% 
  summarise(inc = sum(event, na.rm = T)/n(),
            cas = sum(event, na.rm = T))
podm_5 %>% filter(fibre == 1) %>% 
  summarise(inc = sum(event, na.rm = T)/n(),
            cas = sum(event, na.rm = T))
podm_5 %>% filter(event == 1) %>% summarise(age = median(age))
podm_5 %>% filter(event == 1, dusts + wood + fibre == 0) %>% summarise(age = median(age))
podm_5 %>% filter(event == 1, dusts == 1) %>% summarise(age = median(age))
podm_5 %>% filter(event == 1, wood == 1) %>% summarise(age = median(age))
podm_5 %>% filter(event == 1, fibre == 1) %>% summarise(age = median(age))

# Immigration ------------------------------------------------------------------
podm_5 %>% 
  select(LopNr, base_birth_year) %>% 
  distinct %>% 
  left_join(readRDS('Working data/derived/podm_immig.rds'), by = 'LopNr') %>% 
  filter(!is.na(base_immig_date)) %>% 
  mutate(base_mig_origin = case_when(base_mig_origin == 'Norden utom Sverige' ~ 'Nordic countries',
                                     base_mig_origin == 'EU27 utom Norden' |
                                       base_mig_origin == 'Europa utom EU27 och Norden' |
                                       base_mig_origin == 'Sovjetunionen' ~ 'Rest of Europe',
                                     base_mig_origin == '' | base_mig_origin == 'Ok\u00e4nt' ~ 'Unknown',
                                     TRUE ~ 'Rest of the world')) %>% 
  group_by(base_mig_origin) %>% 
  count() %>%
  ungroup() %>% 
  mutate(p = 100 * n/sum(n)) %>% 
  ggplot(aes(x = base_mig_origin, y = n)) + 
  geom_col(colour = 'black', fill = 'grey90') +
  geom_text(aes(label = paste0(sprintf(p, fmt = "%.0f"), '%')), vjust = -.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 7500)) +
  labs(x = 'Region of origin', y = 'Number of immigrants') +
  theme_classic() +
  theme(text = element_text(size = 14),
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))
ggsave(paste0('Output/figures/', Sys.Date(), '_immigration.pdf'), width = 7, height = 4)


# Occupation -------------------------------------------------------------------
options(scipen = 1000)
occ_p1 <- podm_5 %>% 
  select(age, base_occ2) %>% 
  filter(!is.na(base_occ2)) %>% 
  mutate(base_occ2 = case_when(
    base_occ2 == 'working' ~ 'Working', 
    base_occ2 == 'non-working' ~ 'Retired, unemployed\nor unknown')) %>% 
  ggplot(aes(x = age, fill = base_occ2)) + 
  geom_bar(colour = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 200000)) +
  scale_fill_manual(values = c('grey25', 'grey75')) +
  labs(x = 'Age', y = 'Person-years', fill = 'Occupation') +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = 'none', #,
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))
#ggsave(paste0('Output/figures/', Sys.Date(), '_occ_age.pdf'), width = 8, height = 5)

occ_p2 <- podm_5 %>% 
  select(year, base_exam_date, base_occ2) %>% 
  filter(!is.na(base_occ2)) %>% 
  mutate(base_occ2 = case_when(
    base_occ2 == 'working' ~ 'Working', 
    base_occ2 == 'non-working' ~ 'Retired, unemployed\nor unknown')) %>% 
  mutate(y_after_baseline = year - year(base_exam_date)) %>% 
  ggplot(aes(x = y_after_baseline, fill = base_occ2)) + 
  geom_bar(colour = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 300000)) +
  scale_fill_manual(values = c('grey25', 'grey75')) +
  labs(x = 'Years after first examination', y = 'Person-years', fill = 'Occupation') +
  theme_classic() +
  theme(text = element_text(size = 14),
        #legend.position = c(0.8, .85),
        legend.position = 'none',
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))

occ_p3 <- podm_5 %>% 
  #filter(!is.na(base_occ)) %>% 
  select(base_occ2, contains('jem')) %>% 
  mutate(unexp = (jem_cem + jem_bet + jem_kva + jem_svet + jem_dies + jem_asf + 
                    jem_woo + jem_asb + jem_mmmf) == 0) %>% 
  pivot_longer(cols = c(unexp, contains('jem')), 
               names_to = 'expo_gr', 
               values_to = 'value') %>% 
  mutate(expo_gr = factor(expo_gr, 
                          levels = c('unexp', 'jem_cem', 'jem_bet', 'jem_kva', 'jem_svet', 'jem_dies', 'jem_asf', 
                                     'jem_woo', 'jem_asb', 'jem_mmmf'),
                          labels = c('Unexposed', 
                                     'Cement\ndust',
                                     'Concrete\ndust',
                                     'Quartz\ndust',
                                     'Welding\nfumes',
                                     'Diesel\nexhaust',
                                     'Asphalt\nfumes',
                                     'Wood dust',
                                     'Asbestos',
                                     'MMMF'),
                          ordered = T)) %>% 
  mutate(base_occ2 = case_when(
    base_occ2 == 'working' ~ 'Working', 
    base_occ2 == 'non-working' ~ 'Retired, unemployed\nor unknown')) %>% 
  group_by(base_occ2, expo_gr) %>% 
  summarise(n = sum(value > 0)) %>% 
  ggplot(aes(x = expo_gr, y = n, fill = base_occ2)) +
  geom_col(colour = 'black', position = 'dodge') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2500000)) +
  scale_fill_manual(values = c('grey25', 'grey75')) +
  labs(x = 'Exposure group', y = 'Person-years', fill = 'Occupation') +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = c(0.8, .7),
        panel.grid.major.y = element_line(colour = 'lightgrey', linewidth = .25))

cowplot::plot_grid(
  cowplot::plot_grid(occ_p1, occ_p2, nrow = 1, labels = 'AUTO', rel_widths = c(.57, .43)),
  occ_p3, labels = c(' ', 'C'), nrow = 2, rel_heights = c(.625, .375)) %>% 
  ggsave(filename = paste0('Output/figures/', Sys.Date(), '_occ.pdf'), width = 12, height = 8)

podm_occ <- read_rds('Working data/derived/podm_occ.rds')

podm_3 %>% 
  full_join(podm_occ, by = 'LopNr') %>% 
  filter(year == year(base_exam_date),
         base_title != 'Office workers') %>%
  group_by(base_occ2, base_title) %>% 
  summarise(n = n(), p = mean(dusts)) %>% 
  mutate(base_occ2 = case_when(
    base_occ2 == 'working' ~ 'Working, employed', 
    base_occ2 == 'non-working' ~ 'Retired, unemployed\nor unknown')) %>% 
  ggplot(aes(x = base_occ2, y = base_title, size = n, fill = p)) +
  geom_point(shape = 21, colour = 'black') +
  geom_text(aes(label = paste0(n, ' (', round(p*100), '%)')), size = 4, hjust = 0, nudge_x = .11) +
  scale_y_discrete(limits = rev) +
  scale_x_discrete(expand = expansion(mult = c(.2, 0.75))) +
  scale_size_continuous(range = c(0, 10)) +
  scale_fill_gradient2(high = 'darkred', low = 'white', labels = scales::percent) +
  labs(x = 'Workplace occupational group\n(Statics Sweden employment register, year of recruitment)', 
       y = 'Occupational title\n(Bygghalsokohorten data)',
       size = 'Number of workers',
       fill = 'Percent exposed to\ninorganic dusts or fumes') +
  theme_classic() +
  theme(text = element_text(size = 14))
ggsave(filename = paste0('Output/figures/', Sys.Date(), '_title_occ_expo.pdf'), width = 9, height = 8)
