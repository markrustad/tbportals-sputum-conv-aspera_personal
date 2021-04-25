tar_load(c(df_init_gr, df_init))

# Gabe's targets
df_init_gabe <- df_init_gr$df
df_init_pos_gabe <- df_init_gr$df_init_pos
df_status_over_time <- df_init_gr$df_status_over_time
xl_files <- df_init_gr$xl_files; names(xl_files) <- basename(files)

# dataframes
patient <- xl_files$`TB Portals Patient Cases_20210202.csv` %>% arrange(condition_id)
dst <- xl_files$`TB Portals DST_20210202.csv` %>% group_by(specimen_id, condition_id) %>%
  arrange(condition_id, test_date, .by_group = TRUE)
reg <- xl_files$`TB Portals Regimens_20210202.csv` %>% group_by(patient_id, condition_id) %>%
  rename(regimen_drug_short = regimen_drug)

# subset of MDR df_init to match features of Gabe's df_init (for comparison)
df_mdr <- df_init %>% select(patient_id, condition_id, min_period_start, specimen_id,
                   observation_id, test_date, specimen_collection_site,
                   specimen_collection_date, cultureresults, firstcultureresults,
                   culturetype, microscopyresults, firstmicroscopyresults,
                   microscopytype, cultureresults_class, microscopyresults_class,
                   results_class) %>% 
  distinct()

# compare ids
cond_gabe <- ungroup(df_init_gabe) %>% select(condition_id) %>% distinct() %>% unlist()
cond_mdr <- ungroup(df_mdr) %>% select(condition_id) %>% distinct() %>% unlist()

# observations in my df but not in Gabe's
df_diff <- df_init %>% filter(condition_id %in% setdiff(cond_mdr, cond_gabe)) %>% arrange(test_date, .by_group = TRUE); View(df_diff)
df_diff_short <- df_diff %>% select(names(df_mdr)) %>% distinct() %>% arrange(test_date, .by_group = TRUE); View(df_diff_short)

patient_diff <- patient %>% filter(condition_id %in% setdiff(cond_mdr, cond_gabe)); View(patient_diff) # kept extra extrapulmonary results
dst_diff <- dst %>% filter(condition_id %in% setdiff(cond_mdr, cond_gabe)); View(dst_diff)
reg_diff <- reg %>% filter(condition_id %in% setdiff(cond_mdr, cond_gabe)); View(reg_diff)


# df_diff characteristics
for (i in 1:ncol(df_mdr)) {
  column <- sym(names(df_mdr)[i])
  
  temp <- df_diff %>% group_by(!!column) %>% count(sort = TRUE)
  
  print(temp)
}
