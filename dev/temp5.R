# M: Result frequency vs result date----

plot_m_frc <- ggplot(data = df1) + 
  stat_bin(mapping = aes(x = specimen_collection_date_t, fill = !!col_), position = "fill", binwidth = 30) +
  geom_line(mapping = aes(x = specimen_collection_date_t, y = day_counts_norm)) +
  scale_y_continuous(name = "Microscopy result distribution", sec.axis = sec_axis(name = "Microscopy result counts", labels = bks))

ggplot(data = df1) + 
  geom_line(mapping = aes(x = specimen_collection_date_t, y = day_counts_norm))

bks <- as.character(max(df1$day_counts)*c(0, 0.25, 0.5, 0.75, 1))

ggplot(df1, aes(specimen_collection_date_t, fill = day_counts, group = day_counts)) +
  stat_bin(position = "dodge")

library(gcookbook)  # Load gcookbook for the marathon data set

m_plot <- ggplot(marathon, aes(x = Half,y = Full)) +
  geom_point()

m_plot +
  coord_fixed()

ggplot(mtcars) +
  geom_bar(aes(mpg)) +
  scale_x_binned()