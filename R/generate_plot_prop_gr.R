##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param df_init_gr
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_plot_prop_gr <- function(df_init_gr) {

  # Define plotting dataframe and filtering window for dates----

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
  start_day <- -120
  end_day <- 850

  # Define set variables----

  col_ <- sym("derived_result")
  x_label <- "Result Date (days after treatment start)"
  y_label_frac <- "Result Distribution"
  x_label <- "Result Date (days after treatment start)"
  y_label_count <- "Result Counts"

  # filter plotting data and add day count columns----

  df1 <- df %>% filter(derived_result != "und" & between(specimen_collection_date_relative, start_day, end_day))

  # Result proportion vs result date----
  plot_prop <- ggplot(data = df1) +
    stat_bin(mapping = aes(x = specimen_collection_date_relative, fill = !!col_),
             position = "fill",
             binwidth = 30,
             color = "black") +
    facet_wrap(~ type_of_resistance2, nrow = 5) +
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
    xlim(start_day,end_day)

  plot_den_leg <- ggplot(data = df1) +
    geom_density(aes(x = specimen_collection_date_relative, color = type_of_resistance2),size = 1) +
    facet_wrap(~ type_of_resistance2, nrow = 3, scales = "free_y") +
    xlab(x_label) +
    ylab(y_label_frac) +
    scale_color_manual(values = c("Sensitive/Mono DR" = "#FDE725FF", "MDR non XDR/Poly DR" = "#FDE725FF",
                                  "XDR" = "#FDE725FF"), labels = c("POSITIVE", "NEGATIVE", "und"),
                       name = "Test Result") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          # axis.text.y = element_blank(),
          axis.ticks.x = element_line(color = "transparent"),
          text = element_text(color = "transparent", size = 20),
          plot.background = element_blank(),
          strip.text = element_text(color = "transparent")) +
  xlim(start_day,end_day)

  # return()----
  return(list("plot_prop" = plot_prop, "plot_den_leg" + plot_den_leg))

}
