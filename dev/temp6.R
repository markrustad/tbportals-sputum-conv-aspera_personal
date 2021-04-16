pos <- df_init %>% select(observation_id,results_class) %>% distinct() %>% filter(results_class == "POSITIVE") %>% select(condition_id) %>% distinct()


no_pos <- setdiff(df_init %>% select(condition_id),pos) %>% deframe()

df_no_pos <- df_init %>% filter(condition_id %in% no_pos)

df_no_pos %>% group_by(country) %>% select(condition_id) %>% distinct() %>% count(sort = TRUE, name="n") %>% print()

df_no_pos %>% group_by(outcome) %>% select(condition_id) %>% distinct() %>% count(sort = TRUE, name="n") %>% print()

df_no_pos %>% group_by(case_definition) %>% select(condition_id) %>% distinct() %>% count(sort = TRUE, name="n") %>% print()

df_pos_init %>% filter(case_definition == "New" & comorbidity == "None") %>% n_groups()

df1 <- df_discard %>% select(condition_id:period_span, # skip over activities_period_start/end
                              specimen_collection_date_percent, test_date_percent,
                              regimen_count:treatment_status, # regimen_drug_short
                              comorbidity:organization,
                              culture:microscopy,
                              lineage, specimen_collection_site, outcome_cd, gender) %>%
  distinct()
df1 %>% group_by(case_definition) %>% count(sort = TRUE) %>% select(case_definition)


df1$case_definition = fct_rev(factor(df1$case_definition,
                             levels = c("New", "Relapse", "Failure", "Lost to follow up", "Other", "Chronic TB", "Unknown")))
df1$country= fct_rev(factor(df1$country,
                            levels = c("Georgia", "Belarus", "Moldova", "Romania", "Azerbaijan", "Kazakhstan", "Ukraine", "Nigeria", "India")))

df1$case_definition
(plot_case_def <- ggplot(data = df1) + 
  geom_bar(mapping = aes(x = country, fill = case_definition),
           position = "stack") +
  # coord_flip() +
  ylab("") +
  xlab("Number of cases") +
  scale_fill_brewer(palette = 3) +
  theme_minimal() +
  theme(text = element_text(size=20),
        legend.position = c(.88,.3)))

df_discard %>% n_groups()

ggsave(filename = str_c("plot_case_def_discard", ".png"),
       plot = plot_case_def,
       device = "png",
       path = "Z:/GitHub/NIH",
       dpi = 300)
df_init %>% View()

df_counts <- df_pos_init %>% select(observation_id, conversion_c, conversion_m, results_class,microscopyresults_class,cultureresults_class, specimen_collection_date_t) %>% distinct()

df_counts %>% group(n(microscopyresults_class) == "NEGATIVE",)
