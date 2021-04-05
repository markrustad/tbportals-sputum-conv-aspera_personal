# Exploratory----
temp <- df_pos_init %>% select(condition_id:results_class, matches("type_of_resistance")) %>% 
  distinct()

r_counts <- temp %>% group_by(specimen_collection_date_t, type_of_resistance_2) %>%
  count(sort = TRUE, name = "r_counts") %>% group_by(specimen_collection_date_t) %>%
  mutate(r_freq = 100*r_counts/sum(r_counts))


# Define plotting dataframe----

df <- df_pos_init %>% ungroup() %>% 
  select(condition_id:period_span, # skip over activities_period_start/end
         specimen_collection_date_percent, test_date_percent,
         regimen_count:treatment_status, # regimen_drug_short
         comorbidity:organization,
         culture:microscopy,
         lineage, specimen_collection_site, outcome_cd) %>%
  distinct()

# Result frequency vs result date----

# col <- sym("cultureresults_class")
col <- sym("microscopyresults_class")

df1 <- df %>%
  filter(between(specimen_collection_date_t, -750, 1000) & (!!col != "und"))
# !between(specimen_collection_date_t, -30, 30) &

ggplot(data = df1) + 
  stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col),
           position = "fill",
           binwidth = 30) +
  facet_wrap(~ type_of_resistance_2, nrow = 5)

# create line/path geom of n() for each type_of_resistance_2 facet
df1 %>% group_by(type_of_resistance_2, specimen_collection_date_t) %>%
  count(sort = TRUE) %>% count()

# Result count vs result date----
# col <- sym("cultureresults_class")
col <- sym("microscopyresults_class")

df1 <- df %>%
  filter(between(specimen_collection_date_t, -750, 1000) & (!!col != "und"))
           # !between(specimen_collection_date_t, -30, 30) &

ggplot(data = df1) + 
  stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col),
             position = "stack",
           binwidth = 30) +
  facet_wrap(~ type_of_resistance_2, nrow = 5)


head(names(df1), 48)

