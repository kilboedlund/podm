library('tidyverse')
library('magrittr')
library('survival')
library('splines')
setwd('/safe/data/Research projects/PODM')

podm_5 <- read_rds('Working data/derived/podm_5.rds')

# Setting up the survival object -----------------------------------------------
surv <- podm_5 %$%
  Surv(
    time = .$year,
    time2 = .$year + 1,
    event = .$event,
    type = 'counting'
  )

# Running Cox regressions with three different covariate models ----------------
# combined regression models

crude <- coxph(surv ~ ns(age, 4):ns(year, 4) + 
                  smoke_status + smoke_intensity + 
                  dusts + wood + fibre,
                data = podm_5)

main <- coxph(surv ~ ns(age, 4):ns(year, 4) + 
                   smoke_status + smoke_intensity + 
                   base_bmi_cat + 
                   dusts + wood + fibre,
                 data = podm_5)

extend <- coxph(surv ~ ns(age, 4):ns(year, 4) + 
                smoke_status + smoke_intensity + 
                base_bmi_cat + 
                base_civil + base_sei + 
                dusts + wood + fibre,
              data = podm_5)

# separate regression models
#expo <- list('dusts', 'wood', 'fibre')

#crude <- map(expo,
#             \(x) coxph(as.formula(
#               paste('surv ~', x , '+ ns(age, 4):ns(year, 4)')
#             ),
#             data = podm_5)) %>% 
#  set_names(expo)

#main <- map(expo,
#            \(x) coxph(as.formula(
#              paste(
#                'surv ~',
#                x,
#                '+ ns(age, 4):ns(year, 4) +
#                smoke_status + smoke_intensity + ns(base_bmi, 4)'
#              )
#            ),
#            data = podm_5)) %>% 
#  set_names(expo)

#extend <- map(expo,
#            \(x) coxph(as.formula(
#              paste(
#                'surv ~',
#                x,
#                '+ ns(age, 4):ns(year, 4) +
#                smoke_status + smoke_intensity + ns(base_bmi, 4) + 
#                base_civil + base_sei'
#              )
#            ),
#            data = podm_5)) %>% 
#  set_names(expo)

# Evaluate linearity in Schoenfeld residuals -----------------------------------
crude_zph <- map(crude, cox.zph)
main_zph <- map(main, cox.zph)
extend_zph <- map(extend, cox.zph)

# Binding and transporting results ---------------------------------------------
crude_res <-
  bind_rows(map2(expo, crude, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
    set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'crude',
    zph = crude_zph %>% map2(expo, \(x, y) x$table[y, 'p']) %>% unlist()
  )

main_res <-
  bind_rows(map2(expo, main, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
      set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'main',
    zph = main_zph %>% map2(expo, \(x, y) x$table[y, 'p']) %>% unlist()
  )

extend_res <-
  bind_rows(map2(expo, extend, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
      set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'extend',
   zph = extend_zph %>% map2(expo, \(x, y) x$table[y, 'p']) %>% unlist()
  )

res <- bind_rows(crude_res, main_res, extend_res) %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = c("dusts", "wood", "fibre"), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res, paste0('Output/', Sys.Date(), '_results.xlsx'))

# Running Cox regressions for each particle exposure ---------------------------
expo_sep <- paste0(list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'),
                   '_bin')

crude_sep <- map(expo_sep,
                \(x) coxph(as.formula(
                  paste(
                    'surv ~',
                    x,
                    '+ ns(age, 4):ns(year, 4)'
                  )
                ),
                data = podm_5)) %>% 
  set_names(expo_sep)

main_sep <- map(expo_sep,
                \(x) coxph(as.formula(
                  paste(
                    'surv ~',
                    x,
                    '+ ns(age, 4):ns(year, 4) +
                smoke_status + smoke_intensity + base_bmi'
                  )
                ),
                data = podm_5)) %>% 
  set_names(expo_sep)

extend_sep <- map(expo_sep,
            \(x) coxph(as.formula(
              paste(
                'surv ~',
                x,
                '+ ns(age, 4):ns(year, 4) +
                smoke_status + smoke_intensity + base_bmi + 
                base_civil + base_sei'
              )
            ),
            data = podm_5)) %>% 
  set_names(expo_sep)

crude_zph_sep <- map(crude_sep, cox.zph)
main_zph_sep <- map(main_sep, cox.zph)
extend_zph_sep <- map(extend_sep, cox.zph)

crude_res_sep <-
  bind_rows(map(crude_sep, \(x) exp(c(
    coef(x)[1], confint(x)[1, ]
  )) %>%
    set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_sep),
    model = 'crude',
    zph = crude_zph_sep %>% map(~ .x$table[1, 'p']) %>% unlist()
  )

main_res_sep <-
  bind_rows(map(main_sep, \(x) exp(c(
    coef(x)[1], confint(x)[1, ]
  )) %>%
    set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_sep),
    model = 'main',
    zph = main_zph_sep %>% map(~ .x$table[1, 'p']) %>% unlist()
  )

extend_res_sep <-
  bind_rows(map(extend_sep, \(x) exp(c(
    coef(x)[1], confint(x)[1, ]
  )) %>%
  set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_sep),
    model = 'extend',
    zph = extend_zph_sep %>% map(~ .x$table[1, 'p']) %>% unlist()
  )

res_sep <- bind_rows(crude_res_sep, main_res_sep, extend_res_sep) %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = paste('jem',
                                            list('cem', 'bet', 'kva', 'svet', 'dies', 'asf', 'woo', 'asb', 'mmmf'),
                                            sep = '_'), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res_sep, paste0('Output/', Sys.Date(), '_results_sep.xlsx'))

# Running Cox regressions particle exposure as exposure-response ---------------
expo_sep <- paste0('jem_',
                   list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'))

podm_6 <- podm_5 %>% 
  mutate(jem_dies = case_when(jem_dies == 0 ~ 'none',
                              jem_dies == 5 ~ 'low',
                              jem_dies >= 10 ~ 'high') %>% 
           as.factor() %>% recode_factor(ref = 'none'),
         across(.cols = select(., contains('jem'), -jem_dies) %>% names(),
                .fns = \(x) case_when(x == 0 ~ 'none',
                               x == 10 ~ 'low',
                               x >= 20 ~ 'high') %>% 
           as.factor() %>% recode_factor(ref = 'none')))

main_er <- map(expo_sep,
                \(x) coxph(as.formula(
                  paste(
                    'surv ~',
                    x,
                    '+ ns(age, 4):ns(year, 4) +
                smoke_status + smoke_intensity + base_bmi'
                  )
                ),
                data = podm_6)) %>% 
  set_names(expo_sep)

main_zph_er <- map(main_er, cox.zph)

main_res_er <-
  bind_rows(
    map2(
      main_er, 
      expo_sep, 
      \(x, y)
        coef(x) %>% 
          as.data.frame %>% 
          rownames_to_column(var = 'var') %>% 
          left_join(confint(x) %>% 
                      as.data.frame %>% 
                      rownames_to_column(var = 'var'), 
                    by = 'var') %>%
          filter(str_detect(var, y)) %>% 
          rename(c('exposure' = 'var', 'est' = '.', 'lcl' = '2.5 %', 'ucl' = '97.5 %')))) %>% 
  transmute(model = 'main',
            est = exp(est),
            lcl = exp(lcl),
            ucl = exp(ucl),
         formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         level = case_when(str_detect(exposure, 'high') ~ 'high',
                           str_detect(exposure, 'low') ~ 'low'),
         expo = str_remove(exposure, 'low|high') %>% 
           factor(levels = paste('jem',
                                 list('cem', 'bet', 'kva', 'svet', 'dies', 'asf', 'woo', 'asb', 'mmmf'),
                                 sep = '_'), ordered = T)) %>% 
  arrange(expo, model)

res_er <- main_res_er %>% 
  select(expo, model, level, formatted) %>% 
  pivot_wider(id_cols = c('expo', 'model'), names_from = 'level', values_from = 'formatted') %>% 
  select(expo, model, low, high)
writexl::write_xlsx(res_er, paste0('Output/', Sys.Date(), '_results_er.xlsx'))

# Joint Cox regressions for all particle exposures -----------------------------
expo_sep <- paste0(list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'),
                   '_bin')

crude_multi <- coxph(as.formula(
                   paste(
                     'surv ~',
                     paste(expo_sep, collapse = ' + '),
                     '+ ns(age, 4):ns(year, 4)'
                   )
                 ),
                 data = podm_5)

main_multi <-  coxph(as.formula(
                    paste(
                      'surv ~',
                      paste(expo_sep, collapse = ' + '),
                      '+ ns(age, 4):ns(year, 4) +
                      smoke_status + smoke_intensity + base_bmi'
                      )
                    ),
                    data = podm_5)

extend_multi <-  coxph(as.formula(
                      paste(
                        'surv ~',
                        paste(expo_sep, collapse = ' + '),
                        '+ ns(age, 4):ns(year, 4) +
                        smoke_status + smoke_intensity + base_bmi + 
                        base_civil + base_sei')
                      ), 
                      data = podm_5)

crude_zph_multi <- cox.zph(crude_multi)
main_zph_multi <- cox.zph(main_multi)
extend_zph_multi <- cox.zph(extend_multi)

crude_res_multi <-
  bind_rows(map(crude_multi, \(x) exp(c(
    coef(x)[1], confint(x)[1, ]
  )) %>%
    set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_multi),
    model = 'crude',
    zph = crude_zph_multi %>% map(~ .x$table[1, 'p']) %>% unlist()
  )

main_res_multi <-
  bind_rows(map(main_multi, \(x) exp(c(
    coef(x)[1], confint(x)[1, ]
  )) %>%
    set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_multi),
    model = 'main',
    zph = main_zph_multi %>% map(~ .x$table[1, 'p']) %>% unlist()
  )

extend_res_multi <-
  bind_rows(map(extend_multi, \(x) exp(c(
    coef(x)[1], confint(x)[1, ]
  )) %>%
    set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_multi),
    model = 'extend',
    zph = extend_zph_multi %>% map(~ .x$table[1, 'p']) %>% unlist()
  )

res_multi <- bind_rows(crude_res_multi, main_res_multi, extend_res_multi) %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = paste('jem',
                                            list('cem', 'bet', 'kva', 'svet', 'dies', 'asf', 'woo', 'asb', 'mmmf'),
                                            sep = '_'), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res_multi, paste0('Output/', Sys.Date(), '_results_multi.xlsx'))

# Separately for inside or outside construction ind ----------------------------
## For three main exposure groups ----------------------------------------------
expo <- list('dusts', 'wood', 'fibre')

crude_ind <- map(expo,
                \(x) coxph(as.formula(
                  paste0(
                    'surv ~ ',
                    x, ':base_occ + base_occ + ns(age, 4):ns(year, 4)'
                  )
                ),
                data = podm_5)) %>% 
  set_names(expo)


main_ind <- map(expo,
            \(x) coxph(as.formula(
              paste0(
                'surv ~ ',
                x, ':base_occ + base_occ + ns(age, 4):ns(year, 4) + 
                smoke_status + smoke_intensity + base_bmi'
              )
            ),
            data = podm_5)) %>% 
  set_names(expo)

extend_ind <- map(expo,
                  \(x) coxph(as.formula(
                    paste0(
                      'surv ~ ',
                      x, ':base_occ + base_occ + ns(age, 4):ns(year, 4) + 
                      smoke_status + smoke_intensity + base_bmi + 
                      base_civil + base_sei'
                    )
                  ),
                  data = podm_5)) %>% 
  set_names(expo)

crude_zph_ind <- map(crude_ind, cox.zph)
main_zph_ind <- map(main_ind, cox.zph)
extend_zph_ind <- map(extend_ind, cox.zph)

crude_ind_res <-
  bind_rows(map2(expo, crude_ind, \(x, y) exp(c(
    coef(y)[paste0(x, ':base_occconstruction ind')], 
    confint(y)[paste0(x, ':base_occconstruction ind'), ],
    coef(y)[paste0(x, ':base_occnon-working')], 
    confint(y)[paste0(x, ':base_occnon-working'), ],
    coef(y)[paste0(x, ':base_occother ind')], 
    confint(y)[paste0(x, ':base_occother ind'), ])) %>%
      set_names(c('est_constr', 'lcl_constr', 'ucl_constr',
                  'est_nonwor', 'lcl_nonwor', 'ucl_nonwor',
                  'est_other', 'lcl_other', 'ucl_other')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'crude'
  )

main_ind_res <-
  bind_rows(map2(expo, main_ind, \(x, y) exp(c(
    coef(y)[paste0(x, ':base_occconstruction ind')], 
    confint(y)[paste0(x, ':base_occconstruction ind'), ],
    coef(y)[paste0(x, ':base_occnon-working')], 
    confint(y)[paste0(x, ':base_occnon-working'), ],
    coef(y)[paste0(x, ':base_occother ind')], 
    confint(y)[paste0(x, ':base_occother ind'), ])) %>%
      set_names(c('est_constr', 'lcl_constr', 'ucl_constr',
                  'est_nonwor', 'lcl_nonwor', 'ucl_nonwor',
                  'est_other', 'lcl_other', 'ucl_other')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'main'
  )

extend_ind_res <-
  bind_rows(map2(expo, extend_ind, \(x, y) exp(c(
    coef(y)[paste0(x, ':base_occconstruction ind')], 
    confint(y)[paste0(x, ':base_occconstruction ind'), ],
    coef(y)[paste0(x, ':base_occnon-working')], 
    confint(y)[paste0(x, ':base_occnon-working'), ],
    coef(y)[paste0(x, ':base_occother ind')], 
    confint(y)[paste0(x, ':base_occother ind'), ])) %>%
      set_names(c('est_constr', 'lcl_constr', 'ucl_constr',
                  'est_nonwor', 'lcl_nonwor', 'ucl_nonwor',
                  'est_other', 'lcl_other', 'ucl_other')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'extend'
  )

res_ind <- bind_rows(crude_ind_res, main_ind_res, extend_ind_res) %>% 
  mutate(formatted_constr = paste0(sprintf("%.2f", est_constr), ' (', 
                                   sprintf("%.2f", lcl_constr), '-',
                                   sprintf("%.2f", ucl_constr), ')'),
         formatted_other = paste0(sprintf("%.2f", est_other), ' (', 
                                   sprintf("%.2f", lcl_other), '-',
                                   sprintf("%.2f", ucl_other), ')'),
         formatted_nonwor = paste0(sprintf("%.2f", est_nonwor), ' (', 
                                   sprintf("%.2f", lcl_nonwor), '-',
                                   sprintf("%.2f", ucl_nonwor), ')'),
         expo = factor(expo, levels = c("dusts", "wood", "fibre"), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res_ind, paste0('Output/', Sys.Date(), '_main_results_ind.xlsx'))

## Separately for each particle form -------------------------------------------
expo_sep <- paste('jem',
                  list('mmmf', 'asb', 'dies', 'svet', 'asf', 'kva', 'woo', 'cem', 'bet'),
                  sep = '_')

crude_sep_ind <- map(expo_sep,
                   \(x) coxph(as.formula(
                     paste0(
                       'surv ~ ',
                       x, ':base_occ + base_occ + 
                       ns(age, 4):ns(year, 4)'
                     )
                   ),
                   data = podm_5)) %>%
  set_names(expo_sep)

main_sep_ind <- map(expo_sep,
                   \(x) coxph(as.formula(
                     paste0(
                       'surv ~ ',
                       x, ':base_occ + base_occ + 
                       ns(age, 4):ns(year, 4) + 
                       smoke_status + smoke_intensity + base_bmi'
                     )
                   ),
                   data = podm_5)) %>%
  set_names(expo_sep)

extend_sep_ind <- map(expo_sep,
                   \(x) coxph(as.formula(
                     paste0(
                       'surv ~ ',
                       x, ':base_occ + base_occ + 
                       ns(age, 4):ns(year, 4) + 
                       smoke_status + smoke_intensity + base_bmi +
                       + base_civil + base_sei'
                     )
                   ),
                   data = podm_5)) %>%
  set_names(expo_sep)

crude_zph_sep_ind <- map(crude_sep_ind, cox.zph)
main_zph_sep_ind <- map(main_sep_ind, cox.zph)
extend_zph_sep_ind <- map(extend_sep_ind, cox.zph)

crude_sep_ind_res <-
  bind_rows(map2(expo_sep, crude_sep_ind, \(x, y) exp(c(
    coef(y)[paste0(x, ':base_occconstruction ind')], 
    confint(y)[paste0(x, ':base_occconstruction ind'), ],
    coef(y)[paste0(x, ':base_occnon-working')], 
    confint(y)[paste0(x, ':base_occnon-working'), ],
    coef(y)[paste0(x, ':base_occother ind')], 
    confint(y)[paste0(x, ':base_occother ind'), ])) %>%
      set_names(c('est_constr', 'lcl_constr', 'ucl_constr',
                  'est_nonwor', 'lcl_nonwor', 'ucl_nonwor',
                  'est_other', 'lcl_other', 'ucl_other')))) %>%
  mutate(
    expo = unlist(expo_sep),
    model = 'crude'
  )

main_sep_ind_res <-
  bind_rows(map2(expo_sep, main_sep_ind, \(x, y) exp(c(
    coef(y)[paste0(x, ':base_occconstruction ind')], 
    confint(y)[paste0(x, ':base_occconstruction ind'), ],
    coef(y)[paste0(x, ':base_occnon-working')], 
    confint(y)[paste0(x, ':base_occnon-working'), ],
    coef(y)[paste0(x, ':base_occother ind')], 
    confint(y)[paste0(x, ':base_occother ind'), ])) %>%
      set_names(c('est_constr', 'lcl_constr', 'ucl_constr',
                  'est_nonwor', 'lcl_nonwor', 'ucl_nonwor',
                  'est_other', 'lcl_other', 'ucl_other')))) %>%
  mutate(
    expo = unlist(expo_sep),
    model = 'main'
  )

extend_sep_ind_res <-
  bind_rows(map2(expo_sep, extend_sep_ind, \(x, y) exp(c(
    coef(y)[paste0(x, ':base_occconstruction ind')], 
    confint(y)[paste0(x, ':base_occconstruction ind'), ],
    coef(y)[paste0(x, ':base_occnon-working')], 
    confint(y)[paste0(x, ':base_occnon-working'), ],
    coef(y)[paste0(x, ':base_occother ind')], 
    confint(y)[paste0(x, ':base_occother ind'), ])) %>%
      set_names(c('est_constr', 'lcl_constr', 'ucl_constr',
                  'est_nonwor', 'lcl_nonwor', 'ucl_nonwor',
                  'est_other', 'lcl_other', 'ucl_other')))) %>%
  mutate(
    expo = unlist(expo_sep),
    model = 'extend'
  )

res_sep_ind <- bind_rows(crude_sep_ind_res, main_sep_ind_res, extend_sep_ind_res) %>% 
  mutate(formatted_constr = paste0(sprintf("%.2f", est_constr), ' (', 
                                   sprintf("%.2f", lcl_constr), '-',
                                   sprintf("%.2f", ucl_constr), ')'),
         formatted_other = paste0(sprintf("%.2f", est_other), ' (', 
                                  sprintf("%.2f", lcl_other), '-',
                                  sprintf("%.2f", ucl_other), ')'),
         formatted_nonwor = paste0(sprintf("%.2f", est_nonwor), ' (', 
                                   sprintf("%.2f", lcl_nonwor), '-',
                                   sprintf("%.2f", ucl_nonwor), ')'),
         expo = factor(expo, levels = paste('jem',
                                            list('cem', 'bet', 'kva', 'svet', 'dies', 'asf', 'woo', 'asb', 'mmmf'),
                                            sep = '_'), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res_sep_ind, paste0('Output/', Sys.Date(), '_results_sep_ind.xlsx'))


# Exclusive exposure -----------------------------------------------------------
expo_x <- paste(list('dusts', 'wood', 'fibre'), 'x', sep = '_')

crude_x <- map(expo_x,
              \(x) coxph(as.formula(
                paste(
                  'surv ~',
                  x,
                  '+ ns(age, 4):ns(year, 4)'
                )
              ),
              data = podm_5)) %>% 
  set_names(expo_x)

main_x <- map(expo_x,
            \(x) coxph(as.formula(
              paste(
                'surv ~',
                x,
                '+ ns(age, 4):ns(year, 4) +
                smoke_status + smoke_intensity + base_bmi'
              )
            ),
            data = podm_5)) %>% 
  set_names(expo_x)

extend_x <- map(expo_x,
              \(x) coxph(as.formula(
                paste(
                  'surv ~',
                  x,
                  '+ ns(age, 4):ns(year, 4) +
                smoke_status + smoke_intensity + base_bmi + 
                base_civil + base_sei'
                )
              ),
              data = podm_5)) %>% 
  set_names(expo_x)

crude_zph_x <- map(crude_x, cox.zph)
main_zph_x <- map(main_x, cox.zph)
extend_zph_x <- map(extend_x, cox.zph)

crude_res_x <-
  bind_rows(map2(expo_x, crude_x, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
      set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_x),
    model = 'crude',
    zph = crude_zph_x %>% map2(expo_x, \(x, y) x$table[y, 'p']) %>% unlist()
  )

main_res_x <-
  bind_rows(map2(expo_x, main_x, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
      set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_x),
    model = 'main',
    zph = main_zph_x %>% map2(expo_x, \(x, y) x$table[y, 'p']) %>% unlist()
  )

extend_res_x <-
  bind_rows(map2(expo_x, extend_x, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
      set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo_x),
    model = 'extend',
    zph = extend_zph_x %>% map2(expo_x, \(x, y) x$table[y, 'p']) %>% unlist()
  )

res_x <- bind_rows(crude_res_x, main_res_x, extend_res_x) %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = c("dusts_x", "wood_x", "fibre_x"), ordered = T),
         model = factor(model, levels = c("crude", "main", "extend"), ordered = T)) %>% 
  arrange(expo, model)
writexl::write_xlsx(res_x, paste0('Output/', Sys.Date(), '_results_x.xlsx'))


# Adjustment for region of examination -----------------------------------------
expo <- list('dusts', 'wood', 'fibre')

main <- map(expo,
            \(x) coxph(as.formula(
              paste(
                'surv ~',
                x,
                '+ ns(age, 4):ns(year, 4) + base_reg +
                smoke_status + smoke_intensity + base_bmi'
              )
            ),
            data = podm_5 %>% 
              left_join(read_rds('Working data/derived/podm_base.rds') %>% 
                          select(LopNr, base_reg)))) %>% 
  set_names(expo)

main_res <-
  bind_rows(map2(expo, main, \(x, y) exp(c(
    coef(y)[x], confint(y)[x, ])) %>%
      set_names(c('est', 'lcl', 'ucl')))) %>%
  mutate(
    expo = unlist(expo),
    model = 'main_reg')

res <- main_res %>% 
  mutate(formatted = paste0(sprintf("%.2f", est), ' (', 
                            sprintf("%.2f", lcl), '-',
                            sprintf("%.2f", ucl), ')'),
         expo = factor(expo, levels = c("dusts", "wood", "fibre"), ordered = T)) %>% 
  arrange(expo)
writexl::write_xlsx(res, paste0('Output/', Sys.Date(), '_results_reg.xlsx'))
