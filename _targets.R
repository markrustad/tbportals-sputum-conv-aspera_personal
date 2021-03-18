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
  tar_target(df_initial, generate_df_initial(files, data))
  
)
