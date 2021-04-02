generate_df_init <- function(files, data) {
  
  xl_files <-  data
  names(xl_files) <- basename(files)
  
  # dataframes
  patient <- xl_files$`TB Portals Patient Cases_20210202.csv`
  dst <- xl_files$`TB Portals DST_20210202.csv`
  reg <- xl_files$`TB Portals Regimens_20210202.csv` %>%
    rename(regimen_drug_short = regimen_drug)
  
  # determine intersection of variables and what to drop when joining
  var_patient <- names(patient)
  var_dst <- names(select(dst, -condition_id))
  var_reg <- names(select(reg, -condition_id))
  dst_rmv <- intersect(var_patient,var_dst)
  reg_rmv <- intersect(var_patient,var_reg)
  
  # create master dataframe from joining on condition_id
  
  df <- patient %>% full_join(select(dst, -(all_of(dst_rmv))), by = "condition_id") %>%
    full_join(select(reg, -(all_of(reg_rmv))), by = "condition_id") %>%
    group_by(condition_id) %>%
    mutate(min_period_start = min(period_start)) %>%
    select(condition_id, specimen_id, observation_id, specimen_collection_date,
           test_date, cultureresults, microscopyresults, min_period_start,
           period_start:treatment_status,regimen_drug_short, comorbidity,
           social_risk_factors, patient_id:reinfusioned) %>%
    select(condition_id:genexpert_test,
           (starts_with("bactec_") & !("bactec_test")),
           (starts_with("le_") & !("le_test")),
           (starts_with("hain_") & !("hain_test")),
           (starts_with("lpaother_") & !("lpaother_test")),
           (starts_with("genexpert_") & !("genexpert_test")),
           regimen_drug:reinfusioned) %>%
    arrange(specimen_collection_date, test_date, .by_group = TRUE) %>%
    arrange(case_definition, type_of_resistance, country, age_of_onset,
            desc(registration_date)) %>%
    mutate(activities_period_end = as.integer(activities_period_end))
  
  result_classifier <- function(result_string) {
    
    # three classes of results
    pos_outcomes <- c("Positive", "20 to 100", "100 to 200", "More than 200",
                      "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)",
                      "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", "More than 99 in 1 (4+)")
    und_outcomes <- c("NULL", "Nonspecific microflora", "Study in progress",
                      "Unknown result", "Not done","MOTT", "Saliva", "Unknown data")
    nonpos_outcomes <- append(und_outcomes, "Negative")
    
    if (result_string %in% pos_outcomes) result_class <- "POSITIVE"
    if (result_string %in% und_outcomes) result_class <- "und"
    if (result_string == "Negative") result_class <- "NEGATIVE"
    
    return(result_class)
  }
  
  # add simplified culture/microscopy results columns
  df %<>% rowwise() %>%
    mutate(
      cultureresults_class = result_classifier(cultureresults),
      microscopyresults_class = result_classifier(microscopyresults)
    )
  df %<>% rowwise() %>%
    mutate(results_class = switch(cultureresults_class,
                                  "POSITIVE" = cultureresults_class,
                                  "NEGATIVE" = cultureresults_class,
                                  "und" = microscopyresults_class))
  
  # re-arrange column order
  df %<>% select(condition_id:test_date,
                 results_class, cultureresults_class, microscopyresults_class,
                 cultureresults:reinfusioned)
  
  
  # NORMALIZE DATES TO START OF TREATMENT----
  # columns to transform
  cols <- c("specimen_collection_date", "test_date", "period_start", "period_end",
            "activities_period_start", "activities_period_end", "min_period_start")
  cols_t <- c("specimen_collection_date_t", "test_date_t", "period_start_t", "period_end_t",
              "activities_period_start_t", "activities_period_end_t", "min_period_start_t")
  
  # HOW MANY ARE MISSING TEST RESULTS AFTER DAY 0 INITIAL TREATMENT
  
  # re-scale dates to day 0 on period_start date
  df %<>% ungroup() %>%
    mutate(across(.cols = all_of(cols),
                  .fns = ~(.-min_period_start),
                  .names = "{.col}_t")) %>%
    group_by(condition_id)
  
  # columns <- df %>% names() %>% as_tibble()
  
  # create columns giving collection/observation dates as regimen completion percentage
  df %<>% ungroup() %>%
    mutate(specimen_collection_date_percent = ((specimen_collection_date_t/period_span)*100),
           test_date_percent = ((test_date_t/period_span)*100)) %>%
    group_by(condition_id)
  
  df %<>% ungroup() %>%
    select(condition_id:observation_id, specimen_collection_date_t,
           test_date_t, results_class, cultureresults_class,
           microscopyresults_class, cultureresults, microscopyresults,
           period_span, period_start_t, period_end_t,
           specimen_collection_date_percent, test_date_percent,
           activities_period_start_t, activities_period_end_t,
           regimen_count:reinfusioned, min_period_start_t, all_of(cols)) %>%
    group_by(condition_id)
  
  df %<>% arrange(specimen_collection_date_t, test_date_t, .by_group = TRUE) %>%
    arrange(case_definition, type_of_resistance, country, age_of_onset,
            desc(registration_date))
  
  # for culture/microscopy: column showing a conversion event
  generate_conversion_df <- function(.df, .col) {
    temp <- .df
    col <- sym(.col)
    
    temp %<>% select(condition_id:test_date_t, !!col, cultureresults, microscopyresults) %>%
      distinct() %>% filter(!!col != "und") %>%
      mutate(deltaT = specimen_collection_date_t - lag(specimen_collection_date_t)) %>%
      mutate(previous_result := lag(!!col)) %>%
      mutate(conversion = 0)
    
    obs <- temp %>% filter(!!col != previous_result) %>%
      transmute(observation_id = as.character(observation_id)) %>% deframe()
    
    temp[temp$observation_id %in% obs, "conversion"] <- 1
    
    temp %<>% ungroup() %>%
      select(observation_id, deltaT, conversion)
    
    return(temp)
  }
  
  df_m <- generate_conversion_df(.df = df, .col = "microscopyresults_class") %>%
    rename(deltaT_m = deltaT, conversion_m = conversion)
  df_c <- generate_conversion_df(.df = df, .col = "cultureresults_class") %>%
    rename(deltaT_c = deltaT, conversion_c = conversion)
  
  df <- full_join(df, df_m, by = "observation_id")
  df <- full_join(df, df_c, by = "observation_id")
  
  df %<>%
    select(condition_id:test_date_t,
           microscopyresults_class, deltaT_m, conversion_m, microscopyresults,
           cultureresults_class, deltaT_c, conversion_c, cultureresults,
           results_class,
           period_span:min_period_start)
  
  
  # combine "Poly DR" and "MDR non XDR" levels for type_of_resistance
  # _1: Poly DR with MDR non XDR
  df %<>% mutate(type_of_resistance_1 = type_of_resistance, .before = period_span)
  df[df$type_of_resistance_1 == "Poly DR","type_of_resistance_1"] <- "MDR non XDR"
  
  # _2: Mono DR with Sensitive, and Poly DR with MDR non XDR
  df %<>% mutate(type_of_resistance_2 = type_of_resistance_1, .before = period_span)
  df[df$type_of_resistance_2 == "Mono DR","type_of_resistance_2"] <- "Sensitive"
  
  
  return(df)
}
