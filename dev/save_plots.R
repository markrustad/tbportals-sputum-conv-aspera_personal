# initialize----
library(extrafont)
loadfonts(device = "win")
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
                                                                                                       "XDR")))

# define filtered-date window
start_day <- -135
end_day <- 855

# Define set variables
col_ <- sym("derived_result")
x_label <- "Result Date (months after treatment start)"
y_label_frac <- "Result Distribution"
x_label <- "Result Date (months after treatment start)"
y_label_count <- "Result Counts"

# filter plotting data and add day count columns
df1 <- df %>% filter(derived_result != "und" & between(specimen_collection_date_relative, start_day, end_day))


# 13.33 x 7.5, caption----
(plot <- ggplot(data = df1) + theme_minimal() +
         stat_bin(mapping = aes(x = specimen_collection_date_relative, fill = !!col_),
                  position = "fill", binwidth = 30, color = "black") +
         scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF"),
                           name = "Test Result") +
         scale_x_continuous(breaks = seq(from=-90, to=870, by=90),
                            labels = seq(from=-90, to=870, by=90)/30) +
         scale_y_continuous(n.breaks = 3, labels = percent) +
         geom_density(mapping = aes(x = specimen_collection_date_relative, y=..ndensity..),
                      color = "#FDE725FF", size=1, show.legend = FALSE) +
         facet_wrap(~ type_of_resistance2, nrow = 3) +
         labs(caption = 'Time series of 30-day proportions of POSITIVE (green) and NEGATIVE (purple) sputum test results and the normalized counts of test observations per day\n(yellow) relative to treatment start on day 0 for cases (N = 1,787) stratified by resistance group.  The number of test observations recorded between days\n-15 to 15 is indicated for each resistance group.  POSITIVE test results correspond to microscopy/culture test observations of "Positive", "20 to 100",\n"100 to 200", "More than 200", "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)", "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", or "More than 99 in 1 (4+)";\nNEGATIVE test results correspond to “Negative” microscopy/culture test observations.') +
         xlab("Result Date (months after treatment start)") +
         ylab(y_label_frac) +
         theme(legend.position = "bottom",
               strip.background = element_blank(),
               axis.ticks = element_blank(),
               text = element_text(size = 20, family = "Calibri"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16),
               strip.text = element_text(size = 20),
               plot.caption = element_text(size = 14, hjust = 0))
)

ggsave(filename = str_c("plot_prop_caption-1333x750", ".png"),
       plot = plot,
       device = "png",
       path = "C:/Users/rustadmd/Desktop",
       dpi = 300,
       width = 13.33,
       height = 7.5,
       units = "in")

#13.33 x 13.33, caption----
(plot <- ggplot(data = df1) + theme_minimal() +
         stat_bin(mapping = aes(x = specimen_collection_date_relative, fill = !!col_),
                  position = "fill", binwidth = 30, color = "black") +
         scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF"),
                           name = "Test Result") +
         scale_x_continuous(breaks = seq(from=-90, to=870, by=90),
                            labels = seq(from=-90, to=870, by=90)/30) +
         scale_y_continuous(n.breaks = 3, labels = percent) +
         geom_density(mapping = aes(x = specimen_collection_date_relative, y=..ndensity..),
                      color = "#FDE725FF", size=1, show.legend = FALSE) +
         facet_wrap(~ type_of_resistance2, nrow = 3) +
         labs(caption = 'Time series of 30-day proportions of POSITIVE (green) and NEGATIVE (purple) sputum test results and the normalized counts of test observations per day\n(yellow) relative to treatment start on day 0 for cases (N = 1,787) stratified by resistance group.  The number of test observations recorded between days\n-15 to 15 is indicated for each resistance group.  POSITIVE test results correspond to microscopy/culture test observations of "Positive", "20 to 100",\n"100 to 200", "More than 200", "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)", "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", or "More than 99 in 1 (4+)";\nNEGATIVE test results correspond to “Negative” microscopy/culture test observations.') +
         xlab("Result Date (months after treatment start)") +
         ylab(y_label_frac) +
         theme(legend.position = "bottom",
               strip.background = element_blank(),
               axis.ticks = element_blank(),
               text = element_text(size = 20, family = "Calibri"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16),
               strip.text = element_text(size = 20),
               plot.caption = element_text(size = 14, hjust = 0))
)

ggsave(filename = str_c("plot_prop_caption-1333x1333", ".png"),
       plot = plot,
       device = "png",
       path = "C:/Users/rustadmd/Desktop",
       dpi = 300,
       width = 13.33,
       height = 13.33,
       units = "in")

# 13.33x7.5, no caption----
(plot_nocap <- ggplot(data = df1) + theme_minimal() +
         stat_bin(mapping = aes(x = specimen_collection_date_relative, fill = !!col_),
                  position = "fill", binwidth = 30, color = "black") +
         scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF"),
                           name = "Test Result") +
         scale_x_continuous(breaks = seq(from=-90, to=870, by=90),
                            labels = seq(from=-90, to=870, by=90)/30) +
         scale_y_continuous(n.breaks = 3, labels = percent) +
         geom_density(mapping = aes(x = specimen_collection_date_relative, y=..ndensity..),
                      color = "#FDE725FF", size=1, show.legend = FALSE) +
         facet_wrap(~ type_of_resistance2, nrow = 3) +
         xlab("Result Date (months after treatment start)") +
         ylab(y_label_frac) +
         theme(legend.position = "bottom",
               strip.background = element_blank(),
               axis.ticks = element_blank(),
               text = element_text(size = 20, family = "Calibri"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16),
               strip.text = element_text(size = 20))
)

ggsave(filename = str_c("plot_prop", ".png"),
       plot = plot_nocap,
       device = "png",
       path = "C:/Users/rustadmd/Desktop",
       dpi = 300,
       width = 13.33,
       height = 7.5,
       units = "in")

# 13.33x13.33, no caption----
(plot_nocap <- ggplot(data = df1) + theme_minimal() +
         stat_bin(mapping = aes(x = specimen_collection_date_relative, fill = !!col_),
                  position = "fill", binwidth = 30, color = "black") +
         scale_fill_manual(values = c("NEGATIVE" = "#440154FF", "POSITIVE" = "#238A8DFF"),
                           name = "Test Result") +
         scale_x_continuous(breaks = seq(from=-90, to=870, by=90),
                            labels = seq(from=-90, to=870, by=90)/30) +
         scale_y_continuous(n.breaks = 3, labels = percent) +
         geom_density(mapping = aes(x = specimen_collection_date_relative, y=..ndensity..),
                      color = "#FDE725FF", size=1, show.legend = FALSE) +
         facet_wrap(~ type_of_resistance2, nrow = 3) +
         xlab("Result Date (months after treatment start)") +
         ylab(y_label_frac) +
         theme(legend.position = "bottom",
               strip.background = element_blank(),
               axis.ticks = element_blank(),
               text = element_text(size = 20, family = "Calibri"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16),
               strip.text = element_text(size = 20))
)

ggsave(filename = str_c("plot_prop-1333x1333", ".png"),
       plot = plot_nocap,
       device = "png",
       path = "C:/Users/rustadmd/Desktop",
       dpi = 300,
       width = 13.33,
       height = 13.33,
       units = "in")
