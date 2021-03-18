##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df
##' @param t1
##' @param t2
generate_df_positive_initial <- function(initial_df, t1, t2) {
  
  df <- initial_df; time1 <- t1; time2 <- t2
  
  cids <- df %>% filter(microscopyresults_class == "POSITIVE" | cultureresults_class == "POSITIVE") %>%
    rowwise() %>% 
    filter(between(specimen_collection_date_t, period_start_t - time1, period_start_t + time2)) %>% 
    select(condition_id) %>% distinct()
  
  df_positive_initial <- df %>% ungroup() %>% 
    filter(condition_id %in% deframe(cids)) %>% 
    group_by(condition_id)

  return(df_positive_initial)

}

# cur_data()
# 
# temporal_cases %<>%
#   mutate(relative_change_overall_timika = log2(overall_timika / overall_timika[which.min(imaging_date)]),
#          diff_dates = abs(imaging_date - imaging_date[which.min(imaging_date)]))