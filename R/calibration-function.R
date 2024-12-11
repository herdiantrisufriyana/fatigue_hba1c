calibration <- function(eval_df, binwidth = 1, boot = 30, seed){
    eval_df_bin <-
      eval_df |>
      mutate(pred = round(pred, binwidth))
    
    calib_plot_df <-
      eval_df_bin |>
      group_by(pred) |>
      summarize(
        true = mean(obs == levels(obs)[2])
        , se = qnorm(0.975) * sd(obs == levels(obs)[2]) / sqrt(n())
        , lb = true-se
        , ub = true+se
      )
    
    calib_plot <-
      calib_plot_df |>
      ggplot(aes(pred, true)) +
      geom_abline(slope = 1, intercept=0, lty = 2) +
      geom_linerange(aes(ymin = lb, ymax = ub), na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      geom_smooth(
        method = "lm", formula = y ~ x, color = "black", na.rm = TRUE
      ) +
      coord_equal() +
      scale_x_continuous(
        "Predicted probability", limits = 0:1, breaks = seq(0, 1, 0.1)
      ) +
      scale_y_continuous(
        "True probability", limits = 0:1, breaks = seq(0, 1, 0.1)
      ) +
      theme_classic()
    
    calib_dist <-
      eval_df_bin |>
      group_by(obs, pred) |>
      summarize(n = n(), .groups="drop") |>
      ggplot(aes(pred, n)) +
      geom_col(width = 0.075, na.rm = TRUE) +
      facet_grid(obs ~ ., scales = "free_y") +
      scale_x_continuous(
        "Predicted probability", limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.1)
      ) +
      scale_y_continuous("Frequency (n)", trans = "log10") +
      theme_classic()
    
    sqr_error <-
      eval_df |>
      mutate(sqr_error = (pred - as.integer(obs == levels(obs)[2]))^2) |>
      pull(sqr_error)
    
    sqr_error_boot <- list()
    
    for(i in seq(boot)){
      set.seed(seed + i)
      sqr_error_boot[[i]] <-
        data.frame(term = "rmse", boot = i) |>
        mutate(rmse = sqrt(mean(sample(sqr_error, length(sqr_error), TRUE))))
    }
    
    sqr_error_boot <-
      sqr_error_boot |>
      reduce(rbind)
    
    calib_metrics <-
      lm(true ~ pred, data = calib_plot_df) |>
      tidy() |>
      mutate(term = c("intercept", "slope")) |>
      mutate(ci = qnorm(0.975) * std.error) |>
      select(-std.error, -statistic, -p.value) |>
      rbind(
        sqr_error_boot |>
          group_by(term) |>
          summarize(
            estimate = mean(rmse)
            , ci = qnorm(0.975) * sd(rmse) / sqrt(n())
            , .groups = "drop"
          )
      )
    
    list(plot = calib_plot, dist = calib_dist, metrics = calib_metrics)
  }