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


# Filter sputum collected near beginning of regimen----


# Get the earliest regimen start date for each condition_id
regimens <- xl_files$`TB Portals Regimens_20210202.csv` %>% 
  group_by(condition_id) %>%
  mutate(min_period_start = min(period_start)) %>% 
  select(patient_id:condition_id, min_period_start, period_start:reinfusioned)

# All sputum specimens with added min_period_start column
sputum <- xl_files$`TB Portals DST_20210202.csv` %>% 
  filter(specimen_collection_site == "sputum") %>% 
  group_by(condition_id) %>% 
  arrange(test_date, .by_group = TRUE) %>%
  select(c(condition_id:test_date, specimen_collection_date:microscopytype)) %>%
  left_join(regimens, by="condition_id") %>%
  select(c(condition_id:observation_id, min_period_start, test_date:reinfusioned))

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
positive_initial <- sputum %>% 
  filter((cultureresults %in% pos_outcomes) | (microscopyresults %in% pos_outcomes)) %>%    # 3532 -> 3380  
  rowwise() %>% 
  filter(between(specimen_collection_date, min_period_start - t1, min_period_start + t2)) %>%    # 3380 -> 2376
  group_by(condition_id)

# Import patient case data 
pc <- xl_files$`TB Portals Patient Cases_20210202.csv` %>%
  select(-c(patient_id, culturetype, period_start:period_span, regimen_drug))

# Append to positive_initial sputum
positive_initial %<>% left_join(pc, by="condition_id") %>% 
  group_by(condition_id)

# Data exploration----

# cid's based on culture assay vs microscopy assay
culturebased <- positive_initial %>% filter(cultureresults != "NULL") %>%
  select(condition_id) %>% unique()

microscopybased <- positive_initial %>% filter(microscopyresults != "NULL") %>%
  select(condition_id) %>% unique()

# setdiff(microscopybased,culturebased)
# setdiff(culturebased, microscopybased)

data <- positive_initial %>% ungroup() %>% select(condition_id:microscopytype)

data %>% count(cultureresults)
data %>% count(microscopyresults)
