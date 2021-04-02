generate_df_pos_init <- function(df_init, t1 = 14, t2 = 0) {
  
  df <- df_init; time1 <- t1; time2 <- t2
  
  cids <- df %>% filter(microscopyresults_class == "POSITIVE" | cultureresults_class == "POSITIVE") %>%
    rowwise() %>% 
    filter(between(specimen_collection_date_t, period_start_t - time1, period_start_t + time2)) %>% 
    select(condition_id) %>% distinct()
  
  df_pos_init <- df %>% ungroup() %>% 
    filter(condition_id %in% deframe(cids)) %>% 
    group_by(condition_id)
  
  return(df_pos_init)
  
}
