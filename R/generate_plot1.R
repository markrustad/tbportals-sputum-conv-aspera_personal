generate_plot1 <- function(df_pos_init) {
  
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
  
  # M: filter plotting data and add day count columns----
  
  df1 <- df %>% filter(between(specimen_collection_date_t, start_day, end_day) &
                         (!!col_ != "und"))
  
  # gives total number of results for each collection date
  df1_day_counts <- df1 %>% group_by(specimen_collection_date_t) %>% count(name = "day_counts")
  
  # add day count data to df1 (the filtered, plotting dataframe)
  df1$day_counts <- 0
  for (i in 1:nrow(df1)) {
    date <- df1[[i,"specimen_collection_date_t"]]
    day_count <- df1_day_counts %>% filter(specimen_collection_date_t == date) %>% 
      ungroup() %>% select(day_counts) %>% deframe()
    
    df1[[i,"day_counts"]] <- day_count
  }
  
  # normalized day count column
  df1 %<>% mutate(day_counts_norm = day_counts/max(day_counts))
  
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
  
  plot_m_count <- ggplot(data = df1) +
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack", color = "black",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    xlab(x_label) +
    ylab(y_label_count) +
    guides(fill=guide_legend(title = "Microscopy"))
  
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
  
  
  # Define culture set variables----
  
  col_ <- sym("cultureresults_class")
  x_label <- "Culture Result Date (days after treatment start)"
  y_label_frac <- "Culture Result Distribution"
  x_label <- "Culture Result Date (days after treatment start)"
  y_label_count <- "Culture Result Counts"
  
  # C: filter plotting data and add day count columns----
  
  df1 <- df %>% filter(between(specimen_collection_date_t, start_day, end_day) &
                         (!!col_ != "und"))
  
  # gives total number of results for each collection date
  df1_day_counts <- df1 %>% group_by(specimen_collection_date_t) %>% count(name = "day_counts")
  
  # add day count data to df1 (the filtered, plotting dataframe)
  df1$day_counts <- 0
  for (i in 1:nrow(df1)) {
    date <- df1[[i,"specimen_collection_date_t"]]
    day_count <- df1_day_counts %>% filter(specimen_collection_date_t == date) %>% 
      ungroup() %>% select(day_counts) %>% deframe()
    
    df1[[i,"day_counts"]] <- day_count
  }
  
  # normalized day count column
  df1 %<>% mutate(day_counts_norm = day_counts/max(day_counts))
  
  # C: Result frequency vs result date----
  
  plot_c_frc <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_),
             position = "fill",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Culture")) +
    xlab(x_label) +
    ylab(y_label_frac) +
    scale_y_continuous(labels = percent)
  
  # C: Result count vs result date----
  
  plot_c_count <- ggplot(data = df1) + 
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack", color = "black",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    guides(fill=guide_legend("Culture")) +
    xlab(x_label) +
    ylab(y_label_count)
  
  plot_c_count_crop <- ggplot(data = df1 %>% filter(between(specimen_collection_date_t, 60, 800))) +
    stat_bin(mapping = aes(x = specimen_collection_date_t,
                           fill = !!col_),
             position = "stack", color = "black",
             binwidth = 30) +
    facet_wrap(~ type_of_resistance_2, nrow = 5) +
    xlab(x_label) +
    ylab(y_label_count) +
    guides(fill=guide_legend(title = "Culture"))
  
  # C: No labels----
  
  # result frequency
  plot_c_frc_nolab <- ggplot(data = df1) + 
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
  plot_c_count_nolab <- ggplot(data = df1) +
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
  plot_c_den_nolab <- ggplot(data = df1) +
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
# plot_c_frc
# plot_m_count
# plot_m_count_crop
# plot_c_count
# plot_c_count_crop
# plot_m_frc_nolab
# plot_c_frc_nolab
# plot_m_count_nolab
# plot_c_count_nolab
# plot_m_den_nolab
# plot_c_den_nolab

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
# ggsave(filename = str_c("plot_c_count", ".png"),
#        plot = plot_c_count,
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
# ggsave(filename = str_c("plot_c_den_nolab", ".png"),
#        plot = plot_c_den_nolab,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300,
#        bg = "transparent")
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
# ggsave(filename = str_c("plot_c_frc_nolab", ".png"),
#        plot = plot_c_frc_nolab,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        width = 13.33,
#        height = 7.50,
#        units = "in",
#        dpi = 300,
#        bg = "transparent")
