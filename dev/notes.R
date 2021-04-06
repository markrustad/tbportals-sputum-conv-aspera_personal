# initialize
tar_make()
tar_load(c(files, data, df_init, df_pos_init, plot1))



# condensed df_init of non-"und" results
temp <- df_init %>% select(condition_id:test_date_t,
                              microscopyresults_class:conversion_m,
                              cultureresults_class:conversion_c,results_class) %>%
  distinct() %>%
  filter(!(microscopyresults_class == "und" & cultureresults_class == "und")) %>% 
  group_by(condition_id, specimen_id)


  
# dfs of non-"und" results for microscopy and culture, separately
temp %>% ungroup() %>% select(specimen_id, microscopyresults_class) %>%
  filter(microscopyresults_class != "und") -> m
temp %>% ungroup() %>% select(specimen_id, cultureresults_class) %>%
  filter(cultureresults_class != "und") -> c



# some specimen_id's have multiple POSITIVE/NEGATIVE observations----
# take multiple xrays/cultures of the same sample (could be on different days)
m %>% group_by(specimen_id) %>% count(sort = TRUE)
c %>% group_by(specimen_id) %>% count(sort = TRUE)

df_init %>% filter(specimen_id == "b3d049a0-d741-4ddd-90ce-bf914c6b39ec") %>% View()
df_init %>% filter(condition_id == "0aa8ef6f-98b0-420b-89ce-3d47f8ca4f72") %>% View()

df_init %>% filter(specimen_id == "f9f7198b-59f4-46eb-8771-b1f19db2b107") %>% View()
df_init %>% filter(condition_id == "be50322c-e76d-4b05-8b3b-620d693ad60d") %>%  View()



# multiple rows of the same observation_id (from regimen count > 1)----
df_init %>% group_by(observation_id) %>% count(sort = TRUE) %>%
  ungroup() %>% select(n) %>% group_by(n) %>% count(sort = TRUE)
df_init %>% group_by(observation_id) %>% count(sort = TRUE) %>%
  ungroup() %>% select(n) %>% transmute(n = as.integer(n)) %>%
  deframe() %>% hist(main = "Multiple rows of single observation_id's",
                     xlab = "n() distinct rows of observation_id")
# histogram of regimen count
df_init %>% group_by(regimen_count) %>% count(sort = TRUE) -> x; x$transformed <- x$n / x$regimen_count; x



# n() missing results after initial positive----
# cids with one definitive observation_id 
# 52 condition_id's with just one observation
# condensed df_init of non-"und" results
temp2 <- df_pos_init %>% select(condition_id:test_date_t,
                              microscopyresults_class:conversion_m,
                              cultureresults_class:conversion_c,results_class) %>%
  distinct() %>%
  filter(!(microscopyresults_class == "und" & cultureresults_class == "und")) %>% 
  group_by(condition_id)

cids <- temp2 %>% group_by(condition_id) %>% summarise(n = n()) %>% filter(n == 1) %>%
  transmute(condition_id = as.character(condition_id)) %>% 
  distinct() %>% deframe()
df_one_obs <- df_init %>% 
  filter(condition_id %in% cids) %>% distinct()


temp2 %>% filter(conversion_m == 1 | conversion_c == 1) %>% View()




# cases excluded from df_pos_init----
cids <- df_pos_init %>% transmute(condition_id = as.character(condition_id)) %>% 
  distinct() %>% deframe()
df_init %>% filter(!(condition_id %in% cids)) %>% n_groups() # 1780
df_pos_init %>% n_groups() # 1954
df_init %>% n_groups() # 3734

# activities_period_end date earlier than activities_period_start---- 
df_init %>% filter(activities_period_end_t < activities_period_start_t) %>% View() # 1427 rows, 74 condition_ids

df_pos_init %>% filter(conversion_m == 1 | conversion_c == 1) %>%
  select(condition_id) %>% distinct() %>% 
  transmute(condition_id = as.character(condition_id)) %>% deframe() -> cids

df_init %>% filter(condition_id %in% cids) %>% select(condition_id,type_of_resistance_2) %>% distinct() %>% group_by(type_of_resistance_2) %>% count()

cids %>% writeClipboard()
