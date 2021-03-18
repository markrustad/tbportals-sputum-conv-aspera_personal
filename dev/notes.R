# initialize
drake::r_make()
drake::loadd()



# condensed initial_df of non-"und" results
temp <- initial_df %>% select(condition_id:test_date_t,
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

initial_df %>% filter(specimen_id == "b3d049a0-d741-4ddd-90ce-bf914c6b39ec") %>% View()
initial_df %>% filter(condition_id == "0aa8ef6f-98b0-420b-89ce-3d47f8ca4f72") %>% View()

initial_df %>% filter(specimen_id == "f9f7198b-59f4-46eb-8771-b1f19db2b107") %>% View()
initial_df %>% filter(condition_id == "be50322c-e76d-4b05-8b3b-620d693ad60d") %>%  View()



# multiple rows of the same observation_id (from regimen count > 1)----
initial_df %>% group_by(observation_id) %>% count(sort = TRUE) %>%
  ungroup() %>% select(n) %>% group_by(n) %>% count(sort = TRUE)
initial_df %>% group_by(observation_id) %>% count(sort = TRUE) %>%
  ungroup() %>% select(n) %>% transmute(n = as.integer(n)) %>%
  deframe() %>% hist(main = "Multiple rows of single observation_id's",
                     xlab = "n() distinct rows of observation_id")
# histogram of regimen count
initial_df %>% group_by(regimen_count) %>% count(sort = TRUE) -> x; x$transformed <- x$n / x$regimen_count; x



# n() missing results after initial positive----
# cids with one definitive observation_id 
# 52 condition_id's with just one observation
# condensed initial_df of non-"und" results
temp2 <- df_positive_initial %>% select(condition_id:test_date_t,
                              microscopyresults_class:conversion_m,
                              cultureresults_class:conversion_c,results_class) %>%
  distinct() %>%
  filter(!(microscopyresults_class == "und" & cultureresults_class == "und")) %>% 
  group_by(condition_id)

cids <- temp2 %>% group_by(condition_id) %>% summarise(n = n()) %>% filter(n == 1) %>%
  transmute(condition_id = as.character(condition_id)) %>% 
  distinct() %>% deframe()
df_one_obs <- initial_df %>% 
  filter(condition_id %in% cids) %>% distinct()


temp2 %>% filter(conversion_m == 1 | conversion_c == 1) %>% View()




# cases excluded from df_positive_initial----
cids <- df_positive_initial %>% transmute(condition_id = as.character(condition_id)) %>% 
  distinct() %>% deframe()
initial_df %>% filter(!(condition_id %in% cids)) %>% n_groups() # 1165
df_positive_initial %>% n_groups() # 2569
initial_df %>% n_groups() # 3734

# activities_period_end date earlier than activities_period_start---- 
initial_df %>% filter(activities_period_end_t < activities_period_start_t) %>% View() # 1427

df_positive_initial %>% filter(conversion_m == 1 | conversion_c == 1) %>%
  select(condition_id) %>% distinct() %>% 
  transmute(condition_id = as.character(condition_id)) %>% deframe() -> cids

initial_df %>% filter(condition_id %in% cids) %>% select(condition_id,type_of_resistance_2) %>% distinct() %>% group_by(type_of_resistance_2) %>% count()
