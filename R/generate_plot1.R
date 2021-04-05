generate_plot1 <- function(df_pos_init) {
  
  # variables with 1 to 1 mapping to observation ids
  df <- df_pos_init %>% ungroup() %>% 
    select(condition_id:period_span, # skip over activities_period_start/end
           specimen_collection_date_percent, test_date_percent,
           regimen_count:treatment_status, # regimen_drug_short
           comorbidity:organization,
           culture:microscopy,
           lineage, specimen_collection_site, outcome_cd) %>%
    distinct()
  
  # Histogram specimen_collection_date_t
  # TO DO:
  #      - plot geom_density on second y-axis showing overall counts
  
  df1 <- df %>%
    filter(between(specimen_collection_date_t, -1000, 1000))
  
  output <- ggplot(data = df1, aes(x = specimen_collection_date_t)) +
    geom_bar(aes(fill = results_class),
             stat = "bin", binwidth = 15, position = "fill") +
    # geom_density(aes(y = ..scaled..)) +
    stat_count(aes(fill = results_class)) +
    scale_y_log10() +
    facet_wrap(~ type_of_resistance_2, nrow = 5)
  
  
  # Histogram specimen_collection_date_percent
  # df1 <- df %>%
  #   filter(between(specimen_collection_date_percent, -125, 125))
  # 
  # output <- ggplot(data = df1, aes(x = specimen_collection_date_percent)) + 
  #   geom_bar(aes(fill = results_class),
  #            stat = "bin", binwidth = 5, position = "fill") +
  #   geom_density(aes(y = ..scaled..)) +
  #   facet_wrap(~ type_of_resistance_2, nrow = 5)
  
  
  # Histogram period_span
  # df1 <- df %>%
  #   filter(!is.na(period_span)) %>%
  #   filter(between(period_span, 0, 1000)) %>% # 589 cases with period_span > 1000 days
  #   filter(!(outcome %in% c("Still on treatment", "Unknown")))
  # 
  # output <- ggplot(data = df1, aes(x = period_span, fill = outcome)) +
  #   geom_bar(stat = "bin", binwidth = 30)
  # output + facet_wrap(~ type_of_resistance_2, nrow = 5)
  
  
  return(output)
  
  # # Bar: patients by country----
  # # vec2: vector of data to plot
  # vec2 <- df_norm_dates %>%
  #   group_by(country) %>%
  #   summarise(ndistinct = n_distinct(patient_id))
  # 
  # # output: returned plot
  # output <- ggplot(data = vec2, aes(x = country, y = ndistinct)) +
  #   geom_bar(stat = "identity")
  # 
  # 
  # # PDF: bmi faceted by outcome----
  # # df: plotting dataframe for PDF
  # df <- df_norm_dates %>%
  #   select(condition_id,regimen_count:status, -regimen_drug_short) %>%
  #   distinct() %>%
  #   filter((bmi < 50) & (outcome != "Not Reported"))
  # 
  # # output: returned plot
  # output <- df %>%
  #   ggplot() +
  #   geom_freqpoly(mapping = aes(x = bmi, color = outcome),
  #                  # position = "dodge",
  #                  binwidth = 2, stat = "density") #+
  #   # coord_flip()  
}