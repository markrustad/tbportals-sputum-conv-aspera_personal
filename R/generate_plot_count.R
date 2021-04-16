# generate_plot_count <- function(df_pos_init) {
#   
#   # Define plotting dataframe and filtering window for dates----
#   
#   df <- df_pos_init %>% ungroup() %>% 
#     select(condition_id:period_span, # skip over activities_period_start/end
#            specimen_collection_date_percent, test_date_percent,
#            regimen_count:treatment_status, # regimen_drug_short
#            comorbidity:organization,
#            culture:microscopy,
#            lineage, specimen_collection_site, outcome_cd) %>%
#     distinct()
#   
#   # define filtered-date window
#   start_day <- -100
#   end_day <- 800
#   
#   # Define microscopy set variables----
#   
#   col_ <- sym("results_class")
#   x_label <- "Result Date (days after treatment start)"
#   y_label_frac <- "Result Distribution"
#   x_label <- "Result Date (days after treatment start)"
#   y_label_count <- "Result Counts"
#   
#   # filter plotting data and add day count columns----
#   
#   df1 <- df %>% filter(between(specimen_collection_date_t, start_day, end_day) &
#                          !(microscopyresults == "NULL" & cultureresults == "NULL") &
#                          (results_class != "und"))
#   
#   # Result count vs result date----
#   
#   (plot_count <- ggplot(data = df1) +
#      stat_bin(mapping = aes(x = specimen_collection_date_t,
#                             fill = !!col_),
#               position = "stack", color = "black",
#               binwidth = 30) +
#      facet_wrap(~ type_of_resistance_2, nrow = 5) +
#      xlab(x_label) +
#      ylab(y_label_count) +
#      guides(fill=guide_legend(title = "Test Result")) +
#      scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF")) +
#      theme_minimal())
#   
#   (plot_count_crop <- ggplot(data = df1 %>% filter(specimen_collection_date_t > 15)) +
#       stat_bin(mapping = aes(x = specimen_collection_date_t,
#                              fill = !!col_),
#                position = "stack", color = "black",
#                binwidth = 30) +
#       facet_wrap(~ type_of_resistance_2, nrow = 5) +
#       xlab(x_label) +
#       ylab(y_label_count) +
#       guides(fill=guide_legend(title = "Test Result"))+
#       scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF")) +
#       theme_minimal())
#   
#   
#   # return()----
#   return(plot_count)
# }
# 
# # M: No labels----
# 
# # result counts
# # plot_count_nolab <- ggplot(data = df1) +
# #   stat_bin(mapping = aes(x = specimen_collection_date_t,
# #                          fill = !!col_),
# #            position = "stack", color = "black",
# #            binwidth = 30) +
# #   facet_wrap(~ type_of_resistance_2, nrow = 5) +
# #   xlab(x_label) +
# #   ylab(y_label_count) +
# #   guides(fill=guide_legend(title = "Test Result")) +
# #   scale_fill_viridis(discrete = TRUE) +
# #   theme_minimal() +
# #   theme(legend.position = "none",
# #         panel.background = element_rect(fill = "transparent",colour = NA),
# #         plot.background = element_rect(fill = "transparent",colour = NA))
# 
# # Show plots----
# # plot_m_frc
# # plot_count
# # plot_count_crop
# # plot_m_frc_nolab
# # plot_m_count_nolab
# # plot_m_den_nolab
# 
# 
# # Save plots----
# # ggsave(filename = str_c("plot_m_frc", ".png"),
# #        plot = plot_m_frc,
# #        device = "png",
# #        path = "D:/GitHub/NIH",
# #        width = 13.33,
# #        height = 7.50,
# #        units = "in",
# #        dpi = 300)
# # 
# # ggsave(filename = str_c("plot_count", ".png"),
# #        plot = plot_count,
# #        device = "png",
# #        path = "D:/GitHub/NIH",
# #        width = 13.33,
# #        height = 7.50,
# #        units = "in",
# #        dpi = 300)
# # 
# # ggsave(filename = str_c("plot_m_den_nolab", ".png"),
# #        plot = plot_m_den_nolab,
# #        device = "png",
# #        path = "D:/GitHub/NIH",
# #        width = 13.33,
# #        height = 7.50,
# #        units = "in",
# #        dpi = 300,
# #        bg = "transparent")
# # 
# # 
# # ggsave(filename = str_c("plot_m_frc_nolab", ".png"),
# #        plot = plot_m_frc_nolab,
# #        device = "png",
# #        path = "D:/GitHub/NIH",
# #        width = 13.33,
# #        height = 7.50,
# #        units = "in",
# #        dpi = 300,
# #        bg = "transparent")
# # 