generate_df_discard <- function(df_init, df_pos_init) {
  
  df_main <- df_init
  df_subset <- df_pos_init
  
  # get set difference of condition_id's
  cids_discard <- setdiff(df_main$condition_id, df_subset$condition_id)
  
  df_discard <- df_main %>% filter(condition_id %in% cids_discard)
  
  return(df_discard)
}
