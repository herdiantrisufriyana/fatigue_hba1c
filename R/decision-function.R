decision <- function(eval_df, boot = 30, seed){
  source("R/compute_metrics-function.R")
  
  prevalence <- mean(eval_df$obs == levels(eval_df$obs)[2])
  
  dec_plot <-
    compute_metrics(
      eval_df
      , p = FALSE
      , tpr = FALSE
      , tnr = FALSE
      , ppv = FALSE
      , npv = FALSE
      , interp = FALSE
    )$metrics |>
    select(th, nb) |>
    ggplot(aes(th, nb)) +
    geom_hline(yintercept = 0, lty = 2) +
    annotate(
      "text", 0, 0, label = "treat none", hjust = 0, vjust = -0.4, size = 3
    ) +
    geom_abline(
      slope = -1
      , intercept = prevalence
      , lty = 2
    ) +
    annotate(
      "text"
      , 0
      , prevalence
      , angle = -50
      , label = "treat all"
      , hjust = -0.4
      , vjust = 1.4
      , size = 3
    ) +
    geom_path(na.rm = TRUE) +
    scale_x_continuous("Threshold", breaks = seq(0, 1, 0.01)) +
    scale_y_continuous("Net benefit", breaks = seq(0, 1, 0.01)) +
    theme_classic()
  
  eval_df_boot <- list()
  
  for(i in seq(boot)){
    set.seed(seed + i)
    eval_df_boot[[i]] <-
      compute_metrics(
        eval_df[sample(seq(nrow(eval_df)), nrow(eval_df), TRUE), ]
        , tpr = FALSE
        , tnr = FALSE
        , ppv = FALSE
        , npv = FALSE
        , interp = FALSE
      )$metrics |>
      mutate(boot = i)
  }
  
  eval_df_boot <-
    eval_df_boot |>
    reduce(rbind)
  
  dec_metrics <-
    eval_df_boot |>
    group_by(boot, p) |>
    arrange(th, nb) |>
    mutate(seq = seq(n())) |>
    ungroup() |>
    rename(x = th, y = nb)
  
  dec_metrics <-
    dec_metrics |>
    left_join(
      slice(dec_metrics, -1) |>
        mutate(seq = seq - 1) |>
        rename(x2 = x, y2 = y)
      , by = c("boot", "p", "seq")
    ) |>
    mutate(auc = 0.5 * (x2 - x) * (y - y2) + (x2-x) * y2) |>
    group_by(boot, p) |>
    summarize(auc = sum(auc, na.rm = TRUE), .groups = "drop") |>
    mutate(auc = (auc - 0.5 * p^2) / p) |>
    summarize(
      term = "Net% AUC-DC"
      , estimate = mean(auc)
      , ci = qnorm(0.975) * sd(auc) / sqrt(n())
    )
  
  list(plot = dec_plot, metrics=dec_metrics, prevalence = prevalence)
}