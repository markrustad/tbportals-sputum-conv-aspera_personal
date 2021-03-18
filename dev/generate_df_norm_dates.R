##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df
generate_df_norm_dates <- function(initial_df) {
  
  df <- initial_df
  
  # columns to transform
  cols <- c("specimen_collection_date", "test_date", "period_start", "period_end",
            "activities_period_start", "activities_period_end", "min_period_start")
  cols_t <- c("specimen_collection_date_t", "test_date_t", "period_start_t", "period_end_t",
            "activities_period_start_t", "activities_period_end_t", "min_period_start_t")
  
  # HOW MANY ARE MISSING TEST RESULTS AFTER DAY 0 INITIAL TREATMENT
  
  # re-scale dates to day 0 on period_start date
  df %<>% ungroup() %>%
    mutate(across(.cols = cols,
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
    select(condition_id:observation_id,
           specimen_collection_date_t, specimen_collection_date_percent,
           test_date_t, test_date_percent,
           results_class, cultureresults_class, microscopyresults_class,
           cultureresults, microscopyresults,
           period_span, period_start_t, period_end_t, activities_period_start_t,
           activities_period_end_t,
           regimen_count:reinfusioned,
           min_period_start_t, cols) %>% 
    group_by(condition_id)
  
  df %<>% arrange(specimen_collection_date_t, test_date_t, .by_group = TRUE) %>% 
    arrange(case_definition, type_of_resistance, country, age_of_onset,
            desc(registration_date))
  
  return(df)
  
}
