the_plan <-
  drake_plan(

    xl_files = {
      fnames <- list.files(path=file.path(here("data", "TB Portals Published Clinical Data_20210202")), recursive=T, full.names=T)

      o <- map(.x=fnames, .f=function(filename) read_csv(filename)) %>%
        map(.f=function(df) type.convert(df))

      names(o) <- basename(fnames)

      return(o)
    },

    initial_df = generate_initial_df(xl_files),

    df_positive_initial = generate_df_positive_initial(initial_df, t1 = 14, t2 = 0),

    plot_one = create_plot_one(df_positive_initial)

  )
