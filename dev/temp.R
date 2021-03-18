# Import data----


fnames <- list.files(path=file.path(here("data", "TB Portals Published Clinical Data_20210202")), recursive=T, full.names=T)
xl_files <- map(.x=fnames, .f=function(filename) read_csv(filename)) %>% map(.f=function(df) type.convert(df))
names(xl_files) <- basename(fnames)

# dataframes
patient <- xl_files$`TB Portals Patient Cases_20210202.csv`
dst <- xl_files$`TB Portals DST_20210202.csv`
reg <- xl_files$`TB Portals Regimens_20210202.csv` %>% 
  rename(regimen_drug_short = regimen_drug)

# determine intersection of variables and what to drop when joining
var_patient <- names(patient)
var_dst <- names(select(dst, -condition_id))
var_reg <- names(select(reg, -condition_id))
dst_rmv <- intersect(var_patient,var_dst)
reg_rmv <- intersect(var_patient,var_reg)

# create master dataframe from joining on condition_id

df <- patient %>% full_join(select(dst, -dst_rmv), by = "condition_id") %>% 
  full_join(select(reg, -reg_rmv), by = "condition_id") %>%
  group_by(condition_id) %>% 
  mutate(min_period_start = min(period_start)) %>% 
  select(condition_id, specimen_id, observation_id, specimen_collection_date,
         test_date, cultureresults, microscopyresults, min_period_start,
         period_start:treatment_status,regimen_drug_short, comorbidity,
         social_risk_factors, patient_id:reinfusioned) %>%
  select(condition_id:genexpert_test,
         (starts_with("bactec_") & !("bactec_test")),
         (starts_with("le_") & !("le_test")),
         (starts_with("hain_") & !("hain_test")),
         (starts_with("lpaother_") & !("lpaother_test")),
         (starts_with("genexpert_") & !("genexpert_test")),
         regimen_drug:reinfusioned) %>%
  arrange(specimen_collection_date, test_date, .by_group = TRUE) %>% 
  arrange(case_definition, type_of_resistance, country, age_of_onset,
          desc(registration_date))


# df %<>% arrange(case_definition,
#                 type_of_resistance,
#                 country,
#                 age_of_onset,
#                 desc(registration_date))


# df %>% select(condition_id,case_definition,
#               type_of_resistance,
#               country,
#               age_of_onset,
#               registration_date) %>%
#   distinct() %>% View()



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



# Data exploration----


# cid's based on culture assay vs microscopy assay
df_culturebased <- df_positive_initial %>% filter(cultureresults != "NULL") %>%   # 2516
  select(condition_id) %>% distinct()

df_microscopybased <- df_positive_initial %>% filter(microscopyresults != "NULL") %>% # 1533
  select(condition_id) %>% distinct()

# setdiff(df_microscopybased,df_culturebased)
# setdiff(df_culturebased, df_microscopybased)
# intersect(df_culturebased, df_microscopybased)

# filter rows with a Negative culture/microscopy result 
df_negatives <- df_positive_initial %>%
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
df_convert_sm <- df_positive_initial %>%
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







