##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param files
##' @param data
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_df_init_gr <- function(files, data) {

  xl_files <-  data
  names(xl_files) <- basename(files)
  
  # dataframes
  patient <- xl_files$`TB Portals Patient Cases_20210202.csv`
  dst <- xl_files$`TB Portals DST_20210202.csv`
  reg <- xl_files$`TB Portals Regimens_20210202.csv` %>%
    rename(regimen_drug_short = regimen_drug)
  
  # Get only cases of TB with pulmonary involvement
  pulm_conds <- patient %>%
    filter(lung_localization != "Extrapulmonary") %>%
    select(condition_id) %>% unlist()
  
  # Get initial treatment start date
  reg_starts <- reg %>% 
    group_by(patient_id, condition_id) %>%
    summarise(treatment_start = min(period_start)) %>%
    filter(condition_id %in% pulm_conds)
  
  # Get all sputum sample dates and calculate derived sputum values
  sputum <- dst %>%
    filter(specimen_collection_site == "sputum" & condition_id %in% pulm_conds) %>%
    mutate(derived_culture_result = case_when(cultureresults %in% c("Positive", "20 to 100", "100 to 200", "More than 200",
                                                                    "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)",
                                                                    "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", "More than 99 in 1 (4+)") ~ "POSITIVE",
                                              cultureresults %in% c("Negative") ~ "NEGATIVE",
                                              cultureresults %in% c("Nonspecific microflora", "Study in progress",
                                                                    "Unknown result", "Not done","MOTT", "Saliva", "Unknown data") ~ "und",
                                              TRUE ~ as.character(NA)),
           derived_microscopy_result = case_when(microscopyresults %in% c("Positive", "20 to 100", "100 to 200", "More than 200",
                                                                       "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)",
                                                                       "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", "More than 99 in 1 (4+)") ~ "POSITIVE",
                                                 microscopyresults %in% c("Negative") ~ "NEGATIVE",
                                                 microscopyresults %in% c("Nonspecific microflora", "Study in progress",
                                                                       "Unknown result", "Not done","MOTT", "Saliva", "Unknown data") ~ "und",
                                                 TRUE ~ as.character(NA))) %>%
    mutate(derived_result = case_when(derived_culture_result == "POSITIVE" | derived_microscopy_result == "POSITIVE" ~ "POSITIVE",
                                      derived_culture_result == "NEGATIVE" | derived_microscopy_result == "NEGATIVE" ~ "NEGATIVE",
                                      derived_culture_result == "und" | derived_microscopy_result == "und" ~ "und",
                                      TRUE ~ as.character(NA)))
  
  # identify initial positive cohort
  
  df <- reg_starts %>%
    inner_join(sputum) %>%
    select(patient_id:microscopytype, derived_culture_result, derived_microscopy_result, derived_result)
  
  df_init_pos <- df %>%
    rowwise() %>%
    filter(between(specimen_collection_date, treatment_start - 14, treatment_start)) %>% #Change treatment_start - # to change the range
    filter(derived_result == "POSITIVE")
  
  # Generate test results relative to day 0 treatment start
  df_status_over_time <- df %>%
    filter(condition_id %in% df_init_pos$condition_id & !is.na(derived_result)) %>%
    group_by(patient_id, condition_id) %>%
    mutate(treatment_start_relative = treatment_start - treatment_start,
           specimen_collection_date_relative = specimen_collection_date - treatment_start,
           test_date_relative = test_date - treatment_start) 

  return(list("df" = df, "df_init_pos" = df_init_pos, "df_status_over_time" = df_status_over_time, "xl_files" = xl_files))
}
