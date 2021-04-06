# reproduce figures and tables of 2017 TB Portals Paper

# initialize
tar_make()
tar_load(c(files, data, df_init, df_pos_init, plot1))

# fig1a----

# user-supplied grouping variable
col_ <- sym("type_of_resistance_2")

# data for the figure
df_f1a <- df_init %>% select(condition_id, !!col_) %>% distinct() %>% 
  group_by(!!col_) %>% count(sort = TRUE, name = "counts") %>%
  ungroup() %>%
  arrange(desc(!!col_))%>% 
  mutate(percent = round(100*counts/sum(counts),0)) %>% 
  mutate(ypos = cumsum(percent) - 0.5*percent)

# figure
# ggplot way
f1a <- ggplot(data = df_f1a, aes(x = "", y = percent, fill = !!col_)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = paste0(percent," %")), color = "white", 
            size = 6)

# R base way
f1a_rstyle <- pie(x = df_f1a$percent,labels = df_f1a[["type_of_resistance"]])

# fig1b----
df_f1b <- df_init %>% select(condition_id, country, type_of_resistance) %>%
  distinct() %>% group_by(country) %>% arrange(country)

f1b <- ggplot(data = df_f1b) +
  geom_bar(mapping = aes(y = country, fill = type_of_resistance),
           position = "stack") +
  coord_flip() +
  guides(fill=guide_legend("")) +
  ylab("") +
  xlab("Number of cases")

# fig2----
# subset df_init with columns of DST data
dst_names <- data[[2]] %>% names()
df_f2 <- df_init %>% select(dst_names) %>% distinct()

# change factors to strings
df_f2 <- ungroup(df_f2) %>% mutate(across(where(is.factor), as.character))

# list of drug names
drugs <- df_f2 %>% select(starts_with("bactec_")) %>% names() %>% tibble()
drugs$. = substr(drugs$., 8, nchar(drugs$.)); drugs %<>% deframe()


# Excludes any row that has sensitivity to a particular drug
df_f2 %>% select(condition_id, ends_with("_isoniazid")) %>% distinct() %>% 
  filter(if_any(ends_with("_isoniazid"), ~ . == "R")) %>%
  filter(!if_any(ends_with("_isoniazid"), ~ . == "S"))

# make logical columns testing for equaility with a for loop
# for (var in names(df_f2)) {
#   df_f2 %>% filter(.data[[var]] == "R")
# }

# fig
