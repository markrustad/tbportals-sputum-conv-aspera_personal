# Import data----


fnames <- list.files(path=file.path(here("data", "TB Portals Published Clinical Data_20210202")), recursive=T, full.names=T)
xl_files <- map(.x=fnames, .f=function(filename) read_csv(filename)) %>% map(.f=function(df) type.convert(df))
names(xl_files) <- basename(fnames)

# dataframes
patient <- xl_files$`TB Portals Patient Cases_20210202.csv`
dst <- xl_files$`TB Portals DST_20210202.csv`
reg <- xl_files$`TB Portals Regimens_20210202.csv`

# determine intersection of variables and what to drop when joining
var_patient <- names(select(patient, -condition_id))
var_dst <- names(dst)
var_reg <- names(reg)

dst_rmv <- intersect(var_patient,var_dst)
patient_rmv <- intersect(var_patient,var_reg)

patient %<>% select(-patient_rmv)
dst %<>% select(-dst_rmv)

# create master dataframe from joining on condition_id
df <- reg %>% full_join(patient, by = "condition_id") %>%
  full_join(dst, by = "condition_id") %>%
  group_by(condition_id) %>% 
  mutate(min_period_start = min(period_start)) %>% 
  select(condition_id,patient_id,specimen_id,observation_id,min_period_start,period_start:genexpert_ethionamide) %>% 
  select(condition_id:observation_id,registration_date,
         test_date,specimen_collection_date,
         min_period_start:genexpert_ethionamide) %>%
  arrange(specimen_collection_date, test_date, .by_group = TRUE)
# re-arrange column order  
allcolumns <- names(df)
shortcolumns <- df %>%
  select(c(condition_id,specimen_id,observation_id,test_date:period_end,culture,microscopy,outcome,outcome_cd,cultureresults,microscopyresults)) %>% 
  arrange(specimen_collection_date, test_date, .by_group = TRUE) %>% 
  names()
setdiff(allcolumns,shortcolumns)
df %<>% select(c(shortcolumns,allcolumns))


# Analysis with master data set----


# categories of sputum outcomes
pos_outcomes <- c("Positive", "20 to 100", "100 to 200", "More than 200",
                  "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)",
                  "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", "More than 99 in 1 (4+)")
und_outcomes <- c("NULL", "Nonspecific microflora", "Study in progress",
                  "Unknown result", "Not done","MOTT", "Saliva", "Unknown data")
nonpos_outcomes <- append(und_outcomes, "Negative")

# positive sputum collected near regimen start date: (date - t1, date + t2)
t1 <- 30
t2 <- 7
df_positive_initial <- df %>% 
  filter((cultureresults %in% pos_outcomes) | (microscopyresults %in% pos_outcomes)) %>%   # 3734 -> 3623  
  rowwise() %>% 
  filter(between(specimen_collection_date, min_period_start - t1, min_period_start + t2)) %>%    # 3623 -> 2569
  group_by(condition_id)

# plot the effect of t1, t2 on group size


# cid's based on culture assay vs microscopy assay
df_culturebased <- df_positive_initial %>% filter(cultureresults != "NULL") %>%   # 2516
  select(condition_id) %>% distinct()

df_microscopybased <- df_positive_initial %>% filter(microscopyresults != "NULL") %>% # 1533
  select(condition_id) %>% distinct()

setdiff(df_microscopybased,df_culturebased)
setdiff(df_culturebased, df_microscopybased)
intersect(df_culturebased, df_microscopybased)


# Data exploration----


# cid's based on culture assay vs microscopy assay
df_culturebased <- df_positive_initial %>% filter(cultureresults != "NULL") %>%   # 2516
  select(condition_id) %>% distinct()

df_microscopybased <- df_positive_initial %>% filter(microscopyresults != "NULL") %>% # 1533
  select(condition_id) %>% distinct()

# setdiff(df_microscopybased,df_culturebased)
# setdiff(df_culturebased, df_microscopybased)
# intersect(df_culturebased, df_microscopybased)

# arrange columns and sort by test_date and specimen_collection_date
# df_positive_initial %<>% select(condition_id:observation_id,registration_date,
#                                test_date,specimen_collection_date,
#                                min_period_start:genexpert_ethionamide) %>% 
#   arrange(specimen_collection_date, test_date, .by_group = TRUE)

# smaller frame to work on
df_positive_initial_sm <- df_positive_initial %>% 
  select(c(condition_id,specimen_id,observation_id,test_date:period_end,culture,microscopy,outcome,outcome_cd,cultureresults,microscopyresults))

# filter rows with a Negative culture/microscopy result 
df_negatives <- df_positive_initial_sm %>%
  filter(grepl("Negative",culture) | grepl("Negative",microscopy))

# condition_id's with a "Negative" culture/microscopy result
cids_neg <- df %>%
  filter(cultureresults == "Negative" | microscopyresults == "Negative") %>% 
  select(condition_id) %>% distinct()

# condition_id's with an initial positive and then "Negative" culture/microscopy result
cids_pos <- df_positive_initial %>% 
  select(condition_id) %>% distinct()
cids_convert <- intersect(cids_pos,cids_neg)

# cases with conversion
df_convert_sm <- df_positive_initial_sm %>%
  filter(condition_id %in% deframe(cids_convert))

# cases with conversion
df_convert <- df_positive_initial %>%
  filter(condition_id %in% deframe(cids_convert))

# df %>% ungroup() %>% group_by(outcome) %>% count()
# df_convert %>% ungroup() %>% group_by(outcome) %>% count()

# df %>% filter(condition_id == "00e53f98-afdd-4ac9-94b5-72d1cfa11f0f") %>%  View()
# df %>% select(condition_id,culture) %>% attributes()
# fct_cross(df$culture,df$microscopy) %>% glimpse()
# df_explore <- df %>% select(condition_id,culture,microscopy) %>%
#   ungroup() %>% 
#   distinct()
# temp <- df_explore %>% select(c("culture", "microscopy")) %>%
#   mutate_each(as.character)
# temp[1,1] %>% attributes()


#  replacer <- function(x) {
#   
#   
#   pos_outcomes <- c("Positive", "20 to 100", "100 to 200", "More than 200",
#                     "1 to 19", "10 to 99 in 100 (1+)", "1 to 9 in 100 (1-9/100)",
#                     "1 to 9 in 1 (2+)", "10 to 99 in 1 (3+)", "More than 99 in 1 (4+)")
#   und_outcomes <- c("NULL", "Nonspecific microflora", "Study in progress",
#                     "Unknown result", "Not done","MOTT", "Saliva", "Unknown data")
#   nonpos_outcomes <- append(und_outcomes, "Negative")
#   
#   for (outcome in x) {
#     
#     if(outcome %in% pos_outcomes) x[outcome] <- "+"
#   }
#   
#   return(x)
# }


















