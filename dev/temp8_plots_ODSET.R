tar_load(df_init_gr)

df <- df_init_gr$df_status_over_time

case_metadata <- df_init_gr$xl_files$`TB Portals Patient Cases_20210202.csv` %>%
  select(c("patient_id", "identifier", "registration_date", "condition_id", "type_of_resistance",
           "period_span","regimen_count", "outcome", "treatment_status", "comorbidity", 
           "social_risk_factors", "age_of_onset", "gender", "country", "education", "employment", 
           "number_of_children", "number_of_daily_contacts", "case_definition", 
           "diagnosis_code", "type_of_resistance", "bmi", "lung_localization", 
           "x_ray_count", "status", "organization","outcome"))

df %<>% left_join(case_metadata) %>%
  mutate(type_of_resistance2 = factor(case_when(type_of_resistance %in% c("Sensitive", "Mono DR") ~ "Sensitive/Mono DR",
                                                type_of_resistance %in% c("MDR non XDR", "Poly DR") ~ "MDR non XDR/Poly DR",
                                                type_of_resistance == "XDR" ~ "XDR"), levels = c("Sensitive/Mono DR",
                                                                                                 "MDR non XDR/Poly DR",
                                                                                                 "XDR"))) %>% 
  mutate(case_definition = factor(case_definition, levels = c("Unknown", "Chronic TB", "Other", "Lost to follow up",
                                                              "Failure", "Relapse", "New"))) %>% 
  mutate(country = factor(country, c("India", "Nigeria", "Ukraine", "Kazakhstan", "Azerbaijan",
                                     "Romania", "Moldova", "Belarus", "Georgia")))

df1 <- df %>% select(condition_id, case_definition, country) %>% distinct()

plot_country <- ggplot(data = df1) +
  geom_bar(mapping = aes(x = country, fill = case_definition)) +
  scale_fill_brewer(palette = 3) +
  theme_minimal() +
  coord_flip() +
  xlab("") +
  ylab("Number of cases") +
  theme(text = element_text(size=20),
        legend.position = c(.80, 0.27),
        legend.title = element_blank())


ggsave(filename = str_c("plot_country", ".png"),
       plot = plot_country,
       device = "png",
       path = "C:/Users/rustadmd/Desktop",
       dpi = 300)

# convert dates from days to months
df1 %<>% mutate(specimen_collection_date_relative_m = specimen_collection_date_relative/30)

ggplot(data = df1) +
  geom_bar(aes(x = specimen_collection_date_relative_m, fill = !!col_),
           position="fill",
           color="black") +
  scale_x_binned(breaks = seq(from=-6,to=30,by=1) + 0.5) +
  facet_wrap(~ type_of_resistance2, nrow = 3)