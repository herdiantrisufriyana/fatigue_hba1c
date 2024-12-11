discrimination <-function(eval_df,  boot = 30,  seed){
  source("R/compute_metrics-function.R")
  
  disc_plot <-
    compute_metrics(
      eval_df
      , p = FALSE
      , ppv = FALSE
      , npv = FALSE
      , nb = FALSE
      , interp = FALSE
    )$metrics |>
    select(th,  tpr,  tnr) |>
    ggplot(aes(tnr, tpr)) +
    geom_abline(slope = 1, intercept = 1, lty = 2) +
    geom_path(na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    geom_text(
      aes(label = round(th, 2))
      , hjust = -0.1
      , vjust = 1.1
      , size = 3
      , check_overlap = TRUE
    ) +
    coord_equal() +
    scale_x_reverse("Specificity", breaks = seq(0, 1, 0.1)) +
    scale_y_continuous("Sensitivity", breaks = seq(0, 1, 0.1)) +
    theme_classic()
  
  eval_df_boot <- list()
  
  for(i in seq(boot)){
    set.seed(seed + i)
    eval_df_boot[[i]] <-
      compute_metrics(
        eval_df[sample(seq(nrow(eval_df)), nrow(eval_df), TRUE), ]
        , p = FALSE
        , ppv = FALSE
        , npv = FALSE
        , nb = FALSE
        , interp = FALSE
      )$metrics |>
      mutate(boot = i)
  }
  
  eval_df_boot <-
    eval_df_boot |>
    reduce(rbind)
  
  disc_metrics <-
    eval_df_boot |>
    select(-th) |>
    group_by(boot) |>
    arrange(tnr, tpr) |>
    mutate(seq = seq(n())) |>
    ungroup() |>
    rename(x = tnr, y = tpr)
  
  disc_metrics <-
    disc_metrics |>
    left_join(
      slice(disc_metrics, -1) |>
        mutate(seq = seq - 1) |>
        rename(x2 = x, y2 = y)
      , by = c("boot", "seq")
    ) |>
    mutate(auc = 0.5 * (x2 - x) * (y - y2) + (x2 - x) * y2) |>
    group_by(boot) |>
    summarize(auc = sum(auc, na.rm=TRUE), .groups="drop") |>
    summarize(
      term = "AUC-ROC"
      , estimate = mean(auc)
      , ci = qnorm(0.975) * sd(auc) / sqrt(n())
    )
  
  list(plot=disc_plot, metrics=disc_metrics)
}