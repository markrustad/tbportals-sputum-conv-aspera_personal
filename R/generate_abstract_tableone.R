##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_init_gr
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_abstract_tableone <- function(df_init_gr) {

  # join cohort table with other covariates of interest
  
  df <- df_init_gr$df_init_pos
  df %<>%
    left_join(df_init_gr$xl_files$`TB Portals Patient Cases_20210202.csv`, by = c("patient_id", "condition_id")) %>%
    mutate(type_of_resistance_2 = factor(case_when(type_of_resistance %in% c("Sensitive", "Mono DR") ~ "Sensitive/Mono DR",
                                            type_of_resistance %in% c("MDR non XDR", "Poly DR") ~ "MDR non XDR/Poly DR",
                                            TRUE ~ as.character(type_of_resistance)),
                                         levels = c("Sensitive/Mono DR", "MDR non XDR/Poly DR", "XDR"))) %>%
    ungroup()
  
  #Create df with var of interest for table one 
  tab1_vars <- df %>%
    select(condition_id, age_of_onset, bmi, gender, country, case_definition, registration_date, 
           type_of_resistance_2, lung_localization, outcome, x_ray_exists, ct_exists, genomic_data_exists) %>%
    distinct()
  
  #Set controls
  mycontrols  <- tableby.control(test=FALSE, total=TRUE,
                                 #numeric.test="kwt", cat.test="chisq",
                                 numeric.simplify = TRUE, cat.simplify = TRUE,
                                 #numeric.stats = c("medianiqr", "range", "Nmiss"),
                                 digits = 1, digits.count = 0, digits.pct = 1, digits.n = 0,
                                 numeric.stats=c("median", "q1q3", "range", "Nmiss"),
                                 cat.stats=c("countpct", "Nmiss"),
                                 stats.labels=list(median='Median', q1q3='IQR', range= "Range", Nmiss= "Nmiss"))
  #Create table
  TB_tabOne <- tableby(type_of_resistance_2 ~ registration_date + age_of_onset + bmi + 
                         gender + country + case_definition + outcome + lung_localization +
                         x_ray_exists + ct_exists + genomic_data_exists, data=tab1_vars, 
                       control = mycontrols)
  
  #Create labels 
  labels(TB_tabOne) <- c(condition_id = "Case identifier", type_of_resistance_2 = 'Type of Resistance',  registration_date = "Registration Date", age_of_onset = "Age of onset (yrs)", bmi = "BMI", gender = "Gender", country = "Country", case_definition = "Case definition", 
                         outcome = "Outcome", lung_localization = "Lung localization", x_ray_exists = "Radiographs in portal", ct_exists = "CT in portal", genomic_data_exists = "Genomic data in portal")

  # return table as data frame
  return(summary(TB_tabOne, text = T) %>% as.data.frame())

}
