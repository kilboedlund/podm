library('tidyverse')
library('magrittr')
library('survival')
library('splines')
setwd('/safe/data/Research projects/PODM')

podm_5 <- read_rds('Working data/derived/podm_5.rds')

# Set up the survival object ---------------------------------------------------
surv <- podm_5 %$%
  Surv(
    time = .$year,
    time2 = .$year + 1,
    event = .$event,
    type = 'counting'
  )

# Primary analyses -------------------------------------------------------------
crude <- coxph(
  surv ~ ns(age, 4) +
    dusts + wood + fibre,
  data = podm_5
)

main <- coxph(
  surv ~ ns(age, 4) +
    smoke_status + smoke_intensity +
    ns(base_bmi, 4) +
    dusts + wood + fibre,
  data = podm_5
)

extend <- coxph(
  surv ~ ns(age, 4) +
    smoke_status + smoke_intensity +
    ns(base_bmi, 4) +
    base_civil + base_sei +
    dusts + wood + fibre,
  data = podm_5
)

## Visually inspect the influence of various terms -----------------------------
newdata <- bind_rows(
  data.frame(age = 80, year = 2015, smoke_status = '_never', smoke_intensity = '_none',
             base_bmi = 15:45, dusts = 0, wood = 0, fibre = 0),
  data.frame(age = 80, year = 2015, smoke_status = 'current', smoke_intensity = '_none',
             base_bmi = 15:45, dusts = 0, wood = 0, fibre = 0))
predict(main, newdata = newdata, type = 'risk') %>% 
  as.data.frame() %>%
  rename('fit' = '.') %>%
  mutate(base_bmi = c(15:45, 15:45), 
         smoke_status = c(rep('_never', 31), rep('current', 31))) %>% 
  ggplot(aes(x = base_bmi, y = fit, colour = smoke_status)) +
  scale_x_continuous(breaks = 4:9*5, expand = c(0,0)) +
  geom_line() +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = 'lightgrey')) +
  ggtitle('Prediceted risk of smoking and BMI')
ggsave('Output/figures/pred_smo_bmi.png', width = 5, height = 5)

newdata <- expand.grid(age = 30:90, year = 1985:2015) %>% 
  mutate(smoke_status = '_never', smoke_intensity = '_none',
         base_bmi = 25, dusts = 0, wood = 0, fibre = 0)
predict(main, newdata = newdata, type = 'risk') %>% 
  as.data.frame() %>%
  rename('fit' = '.') %>%
  bind_cols(newdata) %>% 
  ggplot(aes(x = year, y = age, fill = fit)) +
  geom_tile() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = 'lightgrey'),
        panel.border = element_rect(colour = 'black', fill = NA)) +
  ggtitle('Predicted risk of age and calendar time')
ggsave('Output/figures/pred_age_time_heatmap.png', width = 5, height = 5)
predict(main, newdata = newdata, type = 'risk') %>% 
  as.data.frame() %>%
  rename('fit' = '.') %>%
  bind_cols(newdata) %>%
  filter(year %in% c(1980 + 0:7*5)) %>% 
  mutate(year = factor(year, ordered = T)) %>% 
  ggplot(aes(x = age, y = fit, colour = year)) +
  geom_line() +
  geom_label(data = . %>% filter(age == 75), aes(x = age, y = fit, label = year)) +
  scale_x_continuous(breaks = 4:18*5, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5)) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = 'lightgrey')) +
  ggtitle('Predicted risk of age and calendar time')
ggsave('Output/figures/pred_age_time_linegraph.png', width = 5, height = 5)

podm_5 %>% 
  group_by(year, age = 10*round((age+5)/10)) %>% 
  summarise(count_event = sum(event), perc_event = mean(event)) %>% 
  mutate(perc_event = perc_event*100) %>% 
  pivot_longer(-c(age,year)) %>% 
  ggplot(aes(x = year, y = value, fill = as.factor(age))) + 
  geom_col(colour = 'black') +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(name~., scales = 'free_y') +
  theme_classic() +
  ggtitle('Events per time and age')
ggsave('Output/figures/pred_age_time_event.png', width = 5, height = 5)

newdata <- expand.grid(age = 30:90, smoke_status = c('_never', 'former', 'current')) %>% 
  mutate(year = 2015,  smoke_intensity = '_none',
         base_bmi = 25, dusts = 0, wood = 0, fibre = 0)
predict(main, newdata = newdata, type = 'risk') %>% 
  as.data.frame() %>%
  rename('fit' = '.') %>%
  bind_cols(newdata) %>%
  ggplot(aes(x = age, y = fit, colour = smoke_status)) +
  geom_line() +
  scale_x_continuous(breaks = 4:18*5, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 6)) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = 'lightgrey')) +
  ggtitle('Predicted risk of smoke and age')
ggsave('Output/figures/pred_age_time_event.png', width = 5, height = 5)

## Evaluate linearity in Schoenfeld residuals ----------------------------------
crude_zph <- cox.zph(crude)
main_zph <- cox.zph(main)
extend_zph <- cox.zph(extend)

## Bind and transport results --------------------------------------------------
crude_res <-
  full_join(
    coef(crude)[c('dusts', 'wood', 'fibre')] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(crude)[c('dusts', 'wood', 'fibre'), ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'crude') %>% 
  full_join(
    crude_zph$table[c('dusts', 'wood', 'fibre'), 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

main_res <-
  full_join(
    coef(main)[c('dusts', 'wood', 'fibre')] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(main)[c('dusts', 'wood', 'fibre'), ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'main') %>% 
  full_join(
    main_zph$table[c('dusts', 'wood', 'fibre'), 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

extend_res <-
  full_join(
    coef(extend)[c('dusts', 'wood', 'fibre')] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(extend)[c('dusts', 'wood', 'fibre'), ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'extend') %>% 
  full_join(
    extend_zph$table[c('dusts', 'wood', 'fibre'), 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

res <- bind_rows(crude_res, main_res, extend_res) %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = c("dusts", "wood", "fibre"), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res, paste0('Output/', Sys.Date(), '_results.xlsx'))

# Secondary analysis but for separate particle exposures -------------------------
expo_sep <- paste0(list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'),
                   '_bin')

crude_sep <- coxph(as.formula(paste(
  'surv ~',
  paste(expo_sep, collapse = ' + '),
  '+ ns(age, 4)'
)),
data = podm_5)

main_sep <-  coxph(as.formula(
  paste(
    'surv ~',
    paste(expo_sep, collapse = ' + '),
    '+ ns(age, 4) + smoke_status + smoke_intensity + ns(base_bmi, 4)'
  )
),
data = podm_5)

extend_sep <-  coxph(as.formula(
  paste(
    'surv ~',
    paste(expo_sep, collapse = ' + '),
    '+ ns(age, 4) + smoke_status + smoke_intensity + ns(base_bmi, 4) + base_civil + base_sei'
  )
),
data = podm_5)

## Evaluate linearity in Schoenfeld residuals ----------------------------------
crude_zph_sep <- cox.zph(crude_sep)
main_zph_sep <- cox.zph(main_sep)
extend_zph_sep <- cox.zph(extend_sep)

## Bind and transport results --------------------------------------------------
crude_res_sep <-
  full_join(
    coef(crude_sep)[expo_sep] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(crude_sep)[expo_sep, ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'crude') %>% 
  full_join(
    crude_zph_sep$table[expo_sep, 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

main_res_sep <-
  full_join(
    coef(main_sep)[expo_sep] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(main_sep)[expo_sep, ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'main') %>% 
  full_join(
    main_zph_sep$table[expo_sep, 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

extend_res_sep <-
  full_join(
    coef(extend_sep)[expo_sep] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(extend_sep)[expo_sep, ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'extend') %>% 
  full_join(
    extend_zph_sep$table[expo_sep, 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

res_sep <- bind_rows(crude_res_sep, main_res_sep, extend_res_sep) %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = paste0(c('cem', 'bet', 'kva', 'svet', 'dies', 'asf', 'woo', 'asb', 'mmmf'), '_bin'), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res_sep, paste0('Output/', Sys.Date(), '_results_sep.xlsx'))

# Exposure-response analyses ---------------------------------------------------
expo_er <- paste0('jem_',
                   list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'))

podm_6 <- podm_5 %>% 
  mutate(jem_dies = case_when(jem_dies == 0 ~ '_none',
                              jem_dies == 5 ~ 'low',
                              jem_dies >= 10 ~ 'high') %>% 
           as.factor() %>% recode_factor(ref = 'none'),
         across(.cols = select(., contains('jem'), -jem_dies) %>% names(),
                .fns = \(x) case_when(x == 0 ~ '_none',
                               x == 10 ~ 'low',
                               x >= 20 ~ 'high')))

main_er <- coxph(as.formula(
                  paste(
                    'surv ~',
                    paste(expo_er, collapse = ' + '),
                    '+ ns(age, 4) + smoke_status + smoke_intensity + ns(base_bmi, 4)')),
                data = podm_6)

## Evaluate linearity in Schoenfeld residuals ----------------------------------
main_zph_er <- cox.zph(main_er)

## Bind and transport results --------------------------------------------------
res_er <-
        coef(main_er) %>% 
          as.data.frame %>% 
          rownames_to_column('expo') %>% 
          left_join(confint(main_er) %>% 
                      as.data.frame %>% 
                      rownames_to_column('expo')) %>%
          filter(str_detect(expo, paste(expo_er, collapse = '|'))) %>% 
          rename(c('est' = '.', 'lcl' = '2.5 %', 'ucl' = '97.5 %')) %>% 
  mutate(across(-expo, exp), 
         level = str_extract(expo, pattern = 'low|high'),
         expo = str_remove(expo, pattern = 'low|high')) %>% 
  mutate(
         expo = factor(expo, 
                       levels = expo_er, 
                       ordered = T),
         model = 'main',
         formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')')) %>%
  pivot_wider(id_cols = c('expo', 'model'), names_from = 'level', values_from = 'formatted') %>%  
  arrange(expo, model) %>% 
  left_join(main_zph_er$table %>% 
              as.data.frame() %>%
              rownames_to_column('expo') %>% 
              select(expo, p))

writexl::write_xlsx(res_er, paste0('Output/', Sys.Date(), '_results_er.xlsx'))

# Separate working and non-working time ----------------------------------------
## For three main exposure groups ----------------------------------------------
expo_ind <- list('dusts', 'wood', 'fibre')

main_ind <- coxph(surv ~ ns(age, 4) + 
                    dusts:base_occ2 + wood:base_occ2 + fibre:base_occ2 + base_occ2 + 
                    smoke_status + smoke_intensity + ns(base_bmi, 4),
                   data = podm_5)

res_ind <-
  coef(main_ind) %>% 
  as.data.frame %>% 
  rownames_to_column('expo') %>% 
  left_join(confint(main_ind) %>% 
              as.data.frame %>% 
              rownames_to_column('expo')) %>%
  filter(str_detect(expo, paste(expo_ind, collapse = '|'))) %>% 
  rename(c('est' = '.', 'lcl' = '2.5 %', 'ucl' = '97.5 %')) %>% 
  mutate(across(-expo, exp), 
         ind = str_extract(expo, pattern = 'non-working|working'),
         expo = str_remove_all(expo, pattern = 'base_occ2|:|non-working|working')) %>% 
  mutate(
    expo = factor(expo, 
                  levels = expo_ind, 
                  ordered = T),
    model = 'crude',
    formatted = paste0(sprintf("%.2f", est), ' (', 
                       sprintf("%.2f", lcl), '-',
                       sprintf("%.2f", ucl), ')')) %>%
  pivot_wider(id_cols = c('expo', 'model'), names_from = 'ind', values_from = 'formatted') %>%  
  arrange(expo, model)

writexl::write_xlsx(res_ind, paste0('Output/', Sys.Date(), '_results_ind.xlsx'))

## For each particle form ------------------------------------------------------
expo_sep <- paste0(list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'),
                   '_bin')

main_sep_ind <- coxph(as.formula(
                     paste0(
                       'surv ~ ',
                       paste(paste0(expo_sep, ':base_occ2'), collapse = ' + '), '+ base_occ2 + ns(age, 4):ns(year, 4) + smoke_status + smoke_intensity + ns(base_bmi, 4)'
                     )
                   ),
                   data = podm_5)

res_sep_ind <- 
  coef(main_sep_ind) %>% 
  as.data.frame %>% 
  rownames_to_column('expo') %>% 
  left_join(confint(main_sep_ind) %>% 
              as.data.frame %>% 
              rownames_to_column('expo')) %>%
  filter(str_detect(expo, paste(expo_sep, collapse = '|'))) %>% 
  rename(c('est' = '.', 'lcl' = '2.5 %', 'ucl' = '97.5 %')) %>% 
  mutate(across(-expo, exp), 
         ind = str_extract(expo, pattern = 'non-working|working'),
         expo = str_remove_all(expo, pattern = 'base_occ2|:|non-working|working')) %>% 
  mutate(
    expo = factor(expo, 
                  levels = expo_sep, 
                  ordered = T),
    model = 'main',
    formatted = paste0(sprintf("%.2f", est), ' (', 
                       sprintf("%.2f", lcl), '-',
                       sprintf("%.2f", ucl), ')')) %>%
  pivot_wider(id_cols = c('expo', 'model'), names_from = 'ind', values_from = 'formatted') %>%  
  arrange(expo, model)
writexl::write_xlsx(res_sep_ind, paste0('Output/', Sys.Date(), '_results_sep_ind.xlsx'))

# Additional sensitivity analyses ----------------------------------------------
## Remove double-exposed workers -----------------------------------------------
main_x <- coxph(
  surv ~ ns(age, 4) +
    smoke_status + smoke_intensity +
    ns(base_bmi, 4) +
    dusts_x + wood_x + fibre_x,
  data = podm_5
)

main_zph_x <- cox.zph(main_x)

res_x <-
  full_join(
    coef(main_x)[c('dusts_x', 'wood_x', 'fibre_x')] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(main_x)[c('dusts_x', 'wood_x', 'fibre_x'), ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'main_x') %>% 
  full_join(
    main_zph_x$table[c('dusts_x', 'wood_x', 'fibre_x'), 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

writexl::write_xlsx(res_x, paste0('Output/', Sys.Date(), '_results_x.xlsx'))

## Adjust for region of examination --------------------------------------------
main_reg <- coxph(
  surv ~ ns(age, 4) +
    smoke_status + smoke_intensity +
    ns(base_bmi, 4) + base_reg +
    dusts + wood + fibre,
  data = podm_5 %>%
    left_join(
      read_rds('Working data/derived/podm_base.rds') %>%
        select(LopNr, base_reg)
    )
)

main_zph_reg <- cox.zph(main_reg)

res_reg <-
  full_join(
    coef(main_reg)[c('dusts', 'wood', 'fibre')] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(main_reg)[c('dusts', 'wood', 'fibre'), ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'main') %>% 
  full_join(
    main_zph_reg$table[c('dusts', 'wood', 'fibre'), 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

writexl::write_xlsx(res_reg, paste0('Output/', Sys.Date(), '_results_reg.xlsx'))

## Adjust for SEI but not BMI --------------------------------------------------
extend_noBMI <- coxph(
  surv ~ ns(age, 4) +
    smoke_status + smoke_intensity +
    base_civil + base_sei +
    dusts + wood + fibre,
  data = podm_5
)

extend_zph_noBMI <- cox.zph(extend_noBMI)

res_noBMI <-
  full_join(
    coef(extend_noBMI)[c('dusts', 'wood', 'fibre')] %>% 
      as.data.frame() %>% 
      rename('est' = '.') %>% 
      rownames_to_column('expo'),
    confint(extend_noBMI)[c('dusts', 'wood', 'fibre'), ] %>% 
      as.data.frame() %>% 
      rename('lcl' = '2.5 %', 'ucl' = '97.5 %') %>% 
      rownames_to_column('expo')) %>% 
  mutate(across(-expo, exp),
         model = 'extend') %>% 
  full_join(
    extend_zph_noBMI$table[c('dusts', 'wood', 'fibre'), 'p'] %>% 
      as.data.frame() %>% 
      rename('zph' = '.') %>% 
      rownames_to_column('expo'))

writexl::write_xlsx(res_noBMI, paste0('Output/', Sys.Date(), '_results_noBMI.xlsx'))
