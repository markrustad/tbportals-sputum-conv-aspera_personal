# variables with 1 to 1 mapping to observation ids
df <- df_norm_dates %>% ungroup() %>% 
  select(condition_id:period_span, # skip over activities_period_start/end
         regimen_count:treatment_status, # regimen_drug_short
         comorbidity:organization,
         culture:microscopy,
         lineage, specimen_collection_site, outcome_cd) %>%
  distinct()

# list: variables with 1 to 1 mapping to observation ids
names1to1 <- df %>% names() %>% as_tibble()
write_csv(names1to1, file = here("export", "variables_1to1-obsID.csv"))