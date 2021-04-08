generate_plot_prop <- function(df_pos_init) {
  
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
  
  col_ <- sym("results_class")
  x_label <- "Result Date (days after treatment start)"
  y_label_frac <- "Result Distribution"
  x_label <- "Result Date (days after treatment start)"
  y_label_count <- "Result Counts"
  
  # M: filter plotting data and add day count columns----
  
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
    ylab(y_label_frac) +
    scale_y_continuous(labels = percent)
  
  # M: Result count vs result date----
  
  (plot_m_count <- ggplot(data = df1) +
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack", color = "black",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    xlab(x_label) +
    ylab(y_label_count) +
    guides(fill=guide_legend(title = "Test Result")) +
    theme_minimal())
  
  plot_m_count_crop <- ggplot(data = df1 %>% filter(specimen_collection_date_t > 15)) +
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack", color = "black",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    xlab(x_label) +
    ylab(y_label_count) +
    guides(fill=guide_legend(title = "Microscopy"))
  
  # M: No labels----
  
  # result frequency
  plot_m_frc_nolab <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_),
             position = "fill",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  # result counts
  plot_m_count_nolab <- ggplot(data = df1) +
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack", color = "black",
             binwidth = 30,) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  # density
  plot_m_den_nolab <- ggplot(data = df1) +
    geom_density(aes(x = specimen_collection_date_t),
                 colour = "black", alpha = 0.2, size = 1) +
    facet_wrap(~ type_of_resistance_2, nrow = 5, scales = "free_y") +
    xlab(NULL) +
    ylab(NULL) +
    theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  
  
  # return()----
  return(plot_m_frc)
}

# Show plots----
# plot_m_frc
# plot_m_count
# plot_m_count_crop
# plot_m_frc_nolab
# plot_m_count_nolab
# plot_m_den_nolab


# Save plots----
# ggsave(filename = str_c("plot_m_frc", ".png"),
#        plot = plot_m_frc,
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
# ggsave(filename = str_c("plot_m_den_nolab", ".png"),
#        plot = plot_m_den_nolab,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300,
#        bg = "transparent")
# 
# 
# ggsave(filename = str_c("plot_m_frc_nolab", ".png"),
#        plot = plot_m_frc_nolab,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300,
#        bg = "transparent")
# 