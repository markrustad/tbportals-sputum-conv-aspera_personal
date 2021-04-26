tar_load(df_init_gr)

attach(df_init_gr)

case_metadata <- df_init_gr$xl_files$`TB Portals Patient Cases_20210202.csv` %>%
  select(c("patient_id", "identifier", "registration_date", "condition_id", "type_of_resistance",
           "period_span","regimen_count", "outcome", "treatment_status", "comorbidity", 
           "social_risk_factors", "age_of_onset", "gender", "country", "education", "employment", 
           "number_of_children", "number_of_daily_contacts", "case_definition", 
           "diagnosis_code", "type_of_resistance", "bmi", "lung_localization", 
           "x_ray_count", "status", "organization","outcome"))

case_metadata %<>%
  mutate(type_of_resistance2 = factor(case_when(type_of_resistance %in% c("Sensitive", "Mono DR") ~ "Sensitive/Mono DR",
                                                type_of_resistance %in% c("MDR non XDR", "Poly DR") ~ "MDR non XDR/Poly DR",
                                                type_of_resistance == "XDR" ~ "XDR"), levels = c("Sensitive/Mono DR",
                                                                                                 "MDR non XDR/Poly DR",
                                                                                                 "XDR")))

tab1 <- tableby(type_of_resistance2 ~ country + case_definition, data = case_metadata)

summary(tab1, text = TRUE)

df %>% distinct()
