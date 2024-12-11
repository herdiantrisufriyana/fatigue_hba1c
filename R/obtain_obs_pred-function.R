obtain_obs_pred <-
  function(
      prefix
      , data_dir
      , model_dir
      , task = "classification"
    ){
      read_csv(
          paste0(data_dir, prefix, ".csv")
          , show_col_types = F
        ) |>
        select(id, outcome) |>
        cbind(
          read_csv(
            paste0(
              model_dir, "/"
              , ifelse(task == "classification", "prob", "predictions")
              , ".csv"
            )
            , show_col_types = F
          )
        ) |>
        `colnames<-`(c("id", "obs", "pred")) |>
        mutate(
          obs = 1 - obs
          , pred = 1 - pred
        ) |>
        mutate_at("obs", as.factor)
  }