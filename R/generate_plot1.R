generate_plot1 <- function(df_pos_init) {
  
  # Exploratory----
  temp <- df_pos_init %>% select(condition_id:results_class, matches("type_of_resistance")) %>% 
    distinct()
  
  r_counts <- temp %>% group_by(specimen_collection_date_t, type_of_resistance_2) %>%
    count(sort = TRUE, name = "r_counts") %>% group_by(specimen_collection_date_t) %>%
    mutate(r_freq = 100*r_counts/sum(r_counts))
  
  
  # Define plotting dataframe and filtering window for dates----
  
  df <- df_pos_init %>% ungroup() %>% 
    select(condition_id:period_span, # skip over activities_period_start/end
           specimen_collection_date_percent, test_date_percent,
           regimen_count:treatment_status, # regimen_drug_short
           comorbidity:organization,
           culture:microscopy,
           lineage, specimen_collection_site, outcome_cd) %>%
    distinct()
  
  # define filtered-date window
  start_day <- -100
  end_day <- 800
  
  # Define microscopy set variables----
  
  col_ <- sym("microscopyresults_class")
  x_label <- "Microscopy Result Date (days after treatment start)"
  y_label_frac <- "Microscopy Result Distribution"
  x_label <- "Microscopy Result Date (days after treatment start)"
  y_label_count <- "Microscopy Result Counts"
  
  df1 <- df %>% filter(between(specimen_collection_date_t, start_day, end_day) &
                         (!!col_ != "und"))
  
  # M: Result frequency vs result date----
  
  plot_m_frc <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_),
             position = "fill",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Microscopy")) +
    xlab(x_label) +
    ylab(y_label_frac)
  
  # create line/path geom of n() for each type_of_resistance_2 facet
  # df1 %>% group_by(type_of_resistance_2, specimen_collection_date_t) %>%
  #   count(sort = TRUE) %>% count()
  
  # M: Result count vs result date----
  
  plot_m_count <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Microscopy")) +
    xlab(x_label) +
    ylab(y_label_count)
  
  plot_m_count_crop <- ggplot(data = df1 %>% filter(specimen_collection_date_t > 15)) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Microscopy")) +
    xlab(x_label) +
    ylab(y_label_count)
  
  # Define culture set variables----
  
  col_ <- sym("cultureresults_class")
  x_label <- "Culture Result Date (days after treatment start)"
  y_label_frac <- "Culture Result Distribution"
  x_label <- "Culture Result Date (days after treatment start)"
  y_label_count <- "Culture Result Counts"
  
  df1 <- df %>% filter(between(specimen_collection_date_t, start_day, end_day) &
                         (!!col_ != "und"))
  
  # C: Result frequency vs result date----
  
  plot_c_frc <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_),
             position = "fill",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Culture")) +
    xlab(x_label) +
    ylab(y_label_frac)
  
  # create line/path geom of n() for each type_of_resistance_2 facet
  # df1 %>% group_by(type_of_resistance_2, specimen_collection_date_t) %>%
  #   count(sort = TRUE) %>% count()
  
  # C: Result count vs result date----
  
  plot_c_count <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Culture")) +
    xlab(x_label) +
    ylab(y_label_count)
  
  plot_c_count_crop <- ggplot(data = df1 %>% filter(specimen_collection_date_t > 15)) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Culture")) +
    xlab(x_label) +
    ylab(y_label_count)
  
  # return()----
  return(plot_m_frc)
}

# Show plots
# plot_m_frc
# plot_c_frc
# plot_m_count
# plot_m_count_crop
# plot_c_count
# plot_c_count_crop
       
# ggsave(filename = str_c("plot_m_frc", ".png"),
#        plot = plot_m_frc,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300)
# 
# ggsave(filename = str_c("plot_c_frc", ".png"),
#        plot = plot_c_frc,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300)
# 
# ggsave(filename = str_c("plot_m_count", ".png"),
#        plot = plot_m_count,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300)
# 
# ggsave(filename = str_c("plot_m_count_crop", ".png"),
#        plot = plot_m_count_crop,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300)
# 
# ggsave(filename = str_c("plot_c_count", ".png"),
#        plot = plot_c_count,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300)
# 
# ggsave(filename = str_c("plot_c_count_crop", ".png"),
#        plot = plot_c_count_crop,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300)