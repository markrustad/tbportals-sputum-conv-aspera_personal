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
  
  # Define set variables----
  
  col_ <- sym("results_class")
  x_label <- "Result Date (days after treatment start)"
  y_label_frac <- "Result Distribution"
  x_label <- "Result Date (days after treatment start)"
  y_label_count <- "Result Counts"
  
  # filter plotting data and add day count columns----
  
  df1 <- df %>% filter(between(specimen_collection_date_t, start_day, end_day) &
                         !(microscopyresults == "NULL" & cultureresults == "NULL") &
                         (results_class != "und"))
  
   # Result proportion vs result date----
  (plot_prop <- ggplot(data = df1) + 
     stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_),
              position = "fill",
              binwidth = 30,
              color = "black") +
     facet_wrap(~ type_of_resistance_2, nrow = 5) +
     guides(fill=guide_legend("Test Result")) +
     xlab(x_label) +
     ylab(y_label_frac) +
     scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF")) +
     scale_y_continuous(labels = percent) +
     theme_minimal() +
     theme(legend.position = c(.90, .95),
           panel.grid = element_blank(),
           # axis.text.y = element_blank(),
           axis.ticks.x = element_line(),
           text = element_text(size = 20)) +
     xlim(-110,850))
           # axis.text = element_text(size = 16),
           # axis.title = element_text(size = 18),
           # strip.text = element_text(size = 14),
           # legend.text = element_text(size = 12),
           # legend.title = element_text(size = 16)))
  
  (plot_den_leg <- ggplot(data = df1) +
      geom_density(aes(x = specimen_collection_date_t, color = type_of_resistance_2),size = 1) +
      facet_wrap(~ type_of_resistance_2, nrow = 3, scales = "free_y") +
      xlab(x_label) +
      ylab(y_label_frac) +
      scale_color_manual(values = c("Sensitive" = "#FDE725FF", "MDR non XDR" = "#FDE725FF",
                                    "XDR" = "#FDE725FF"), labels = c("POSITIVE", "NEGATIVE", "und"),
                         name = "Test Result") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_line(color = "transparent"),
            text = element_text(color = "transparent", size = 20),
            plot.background = element_blank(),
            strip.text = element_text(color = "transparent")) +
      xlim(-110,850))
  
  
            
  
  
  ggplot(data = df1) +
    geom_density(aes(x = specimen_collection_date_t, color = results_class),size = 1) +
    scale_y_continuous(n.breaks = 5, labels = label_percent(scale = 1000, accuracy = 1)) +
    facet_wrap(~ type_of_resistance_2, nrow = 3, scales = "free_y")       
  
  

  
  
  
  # return()----
  return(plot_prop)
}

# No labels----

# result frequency
# plot_prop_nolab <- ggplot(data = df1) +
#   stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_),
#            position = "fill",
#            binwidth = 30,
#            color = "grey") +
#   facet_wrap(~ type_of_resistance_2, nrow = 5) +
#   guides(fill=guide_legend("Test Result")) +
#   xlab(x_label) +
#   ylab(y_label_frac) +
#   scale_y_continuous(labels = percent) +
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal() +
#   xlab(NULL) + ylab(NULL) +
#   theme(legend.position = "none", axis.text = element_blank(),
#         panel.background = element_rect(fill = "transparent",colour = NA),
#         plot.background = element_rect(fill = "transparent",colour = NA),
#         strip.background = element_blank(), strip.text = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# density
# (plot_den_nolab <- ggplot(data = df1) +
#   geom_density(aes(x = specimen_collection_date_t), size = 1,
#                color = viridis(n = 4)[[2]]) +
#   facet_wrap(~ type_of_resistance_2, nrow = 5, scales = "free_y") +
#   xlab(NULL) + ylab(NULL) +
#   theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "transparent",colour = NA),
#         plot.background = element_rect(fill = "transparent",colour = NA),
#         strip.background = element_blank(), strip.text = element_blank(),
#         aspect.ratio = 9/16))


# Show plots----
# plot_prop
# plot_m_count
# plot_m_count_crop
# plot_prop_nolab
# plot_m_count_nolab
# plot_m_den_nolab


# Save plots----
# 
# ggsave(filename = str_c("plot_prop", ".png"),
#        plot = plot_prop,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        height = 7.50,
#        width = 13.3,
#        units = "in",
#        dpi = 300)
# 
# ggsave(filename = str_c("plot_density", ".png"),
#        plot = plot_den_leg,
#        device = "png",
#        path = "D:/GitHub/NIH",
#        height = 7.50,
#        units = "in",
#        dpi = 300)

