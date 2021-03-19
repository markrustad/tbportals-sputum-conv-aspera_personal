library(targets)
library(tarchetypes)
source("R/packages.R")
source("R/functions.R")



# Define the pipeline. A pipeline is just a list of targets.
list(
  
  # create list of file targets
  tar_files(files, paste("data/", list.files("data"), sep = "")),

  # map over files using list iteration
  tar_target(data,
             read_csv(files) %>% type.convert(),
             pattern = map(files),
             iteration = "list"),
  
  # generate initial dataframe from raw data
  tar_target(df_init, generate_df_init(files, data)),
  
  # filter cond. id's that have documented POSITIVE near treatment start
  tar_target(df_pos_init, generate_df_pos_init(df_init, t1 = 14, t2 = 0)),
  
  # create plot target
  tar_target(plot1, generate_plot1(df_pos_init))
  
)
