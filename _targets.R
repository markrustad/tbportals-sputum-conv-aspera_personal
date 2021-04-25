## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
# tar_plan (
# # target = function_to_make(arg), ## drake style
# # tar_target(target2, function_to_make2(arg)) ## targets style
# )
tar_plan(
  
  # create list of file targets
  tar_files(files, paste("data/", list.files("data"), sep = "")),
  
  # map over files using list iteration
  tar_target(data,
             read_csv(files) %>% type.convert(),
             pattern = map(files),
             iteration = "list"),
  
  # generate initial dataframe from raw data
  df_init = generate_df_init(files, data),
  
  df_init_gr = generate_df_init_gr(files, data),
  
  plot_prop_gr = generate_plot_prop_gr(df_init_gr),
  
  # cond_ids w/ documented POSITIVE(culture OR microscopy) near treatment start
  df_pos_init = generate_df_pos_init(df_init, t1 = 14, t2 = 0),
  
  # complement of df_init, df_pos_init
  df_discard = generate_df_discard(df_init, df_pos_init),
  
  plot_prop = generate_plot_prop(df_pos_init)
  
  # plot_count = generate_plot_count(df_pos_init)
)

# tar_make()
# tar_load(c(df_init_gr, plot_prop_gr))