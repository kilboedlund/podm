# This function reads multiple sas7bdat files with a certain name pattern ------
# consisting of prefix + year + suffix
multi_read_sas <- function(prefix, years, suffix) {
  purrr::map(years, ~haven::read_sas(paste0(prefix, .x, suffix))) %>%
    set_names(paste0(years))
}

# This function identifies which ICD version is used for each patient visit ----
ICD_version <- function(x) {
  x %>%
    mutate(ICD = case_when(AR <= 1986 ~ 'ICD8',
                           str_detect(str_sub(DIA, 1, 1), "[:digit:]") & AR >= 1987 & AR <= 1997 ~ 'ICD9',
                           str_detect(str_sub(DIA, 1, 1), "V") & AR >= 1987 & AR <= 1997 ~ 'ICD9',
                           str_detect(str_sub(DIA, 1, 1), "[:alpha:]") & AR >= 1997 ~ 'ICD10',
                           TRUE ~ NA_character_))
}

# This function identifies cases of DM and HPT from PAR files ------------------
par_diag <- function(x, pattern) {
  x_ICD10 <- x %>%
    filter(ICD == 'ICD10') %>% 
    mutate(#DM1 = 'E10',
           DM = 'E11 E14',
           #DMx = 'E12 E13',
           #DMu = 'E14',
           HPT = 'I10 I11 I12 I13 I14')
  x_ICD9 <- x %>%
    filter(ICD == 'ICD9') %>% 
    mutate(DM = '250',
           HPT = '401 402 403 404')
  x_ICD8 <- x %>% 
    filter(ICD == 'ICD8') %>% 
    mutate(DM = '250',
           HPT = '400 401 402 403 404') 
  bind_rows(x_ICD10, x_ICD9, x_ICD8) %>% 
    mutate(across(DM:HPT, ~if_else(str_detect(.x, pattern = DIA), INDATUMA, NA_Date_)))
  }

# This function combines beta coefficients from two Cox covariates -------------
calculate_combined_HR_CI <- function(model, expo, add) {
  mat <- vcov(model)
  v1 <- expo
  v2 <- paste0(expo, ':', add)
  var_b1 <- mat[v1, v1]
  var_b2 <- mat[v2, v2]
  cov <- mat[v1, v2]
  var_b1_b2 <- var_b1 + var_b2 + 2 * cov
  b1 <- model$coefficients[v1]
  HR_b1 <- exp(b1)
  LCL_b1 <- exp(b1 - 1.96 * sqrt(var_b1))
  UCL_b1 <- exp(b1 + 1.96 * sqrt(var_b1))
  b1_b2 <- model$coefficients[v1] + model$coefficients[v2]
  HR_b1_b2 <- exp(b1_b2)
  LCL_b1_b2 <- exp(b1_b2 - 1.96 * sqrt(var_b1_b2))
  UCL_b1_b2 <- exp(b1_b2 + 1.96 * sqrt(var_b1_b2))
  return(data.frame(
    HR = HR_b1, 
    HR_65 = HR_b1_b2,
    LCL = LCL_b1, 
    LCL_65 = LCL_b1_b2,
    UCL = UCL_b1,
    UCL_65 = UCL_b1_b2)
  )
}

# This function makes a custom table 1 summary of a data set -------------------
table1_podm <- function(x) {
  x %>%
    summarise(
      n = length(unique(LopNr)),
      age_at_recr = paste0(
        mean(year(base_exam_date) - base_birth_year, na.rm = T) %>% round(1),
        ' (',
        sd(year(base_exam_date) - base_birth_year, na.rm = T) %>% round(1),
        ')'
      ),
      current_smokers = paste0((
        mean(smoke_status == 'current', na.rm = T) * 100
      ) %>% round(), '%'),
      heavy_smokers = paste0((
        mean(smoke_intensity == 'heavy', na.rm = T) * 100
      ) %>% round(), '%'),
      year_of_recr = paste0(
        mean(year(base_exam_date), na.rm = T) %>% round(),
        ' (',
        sd(year(base_exam_date), na.rm = T) %>% round(),
        ')'
      ),
      mean_BMI = paste0(
        mean(base_bmi, na.rm = T) %>% round(1),
        ' (',
        sd(base_bmi, na.rm = T) %>% round(1),
        ')'
      ),
      overweight = paste0(
        (mean(base_bmi>=25 & base_bmi <30, na.rm = T)*100) %>% round(1),
        '%'
      ),
      obese = paste0(
        (mean(base_bmi>=30)*100) %>% round(1),
        '%'
      ),
      obese_I = paste0(
        (mean(base_bmi>=30 & base_bmi <35, na.rm = T)*100) %>% round(1),
        '%'
      ),
      obese_II = paste0(
        (mean(base_bmi>=35 & base_bmi <40, na.rm = T)*100) %>% round(1),
        '%'
      ),
      obese_III = paste0(
        (mean(base_bmi>=40, na.rm = T)*100) %>% round(1),
        '%'
      ),
      manual_worker = paste0((
        mean(base_sei == '_manual worker, unskilled', na.rm = T) * 100
      ) %>% round(),
      '%'),
      unmarried = paste0(
        (mean(base_civil == 'unmarried', na.rm = T) * 100) %>% round(),
        '%')
    )
}
