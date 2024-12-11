arrange_dec_plot <-
  function(
    decision_obj
    , threshold = 0.5
    , dc_xmin = 0
    , dc_xmax = 1
    , dc_xby = 0.01
    , dc_ymin = 0
    , dc_ymax = 1
    , dc_yby = 0.01
    , dc_ta_angle = -50
  ){
    suppressMessages(
      decision_obj$plot$data |>
        ggplot(aes(th, nb)) +
        geom_vline(xintercept = threshold, lty = 2, na.rm = TRUE) +
        geom_hline(yintercept = 0, lty = 2) +
        annotate(
          "text", 0, 0, label = "treat none", hjust = 0, vjust = -0.4, size = 3
        ) +
        geom_abline(slope = -1, intercept = decision_obj$prevalence, lty = 2) +
        annotate(
          "text"
          , 0, decision_obj$prevalence, angle = dc_ta_angle, label = "treat all"
          , hjust = -0.4, vjust = 1.4, size = 3
        ) +
        geom_hline(
          yintercept = filter(decision_obj$plot$data, th == threshold)$nb
          , lty = 2, na.rm = TRUE
        ) +
        geom_path(na.rm = TRUE) +
        geom_point(aes(y=ifelse(th == threshold, nb, NA)), na.rm = TRUE) +
        scale_x_continuous(
          "Threshold"
          , breaks = seq(dc_xmin, dc_xmax, dc_xby)
          , limits = c(dc_xmin, dc_xmax)
        ) +
        scale_y_continuous(
          "Net benefit"
          , breaks = seq(dc_ymin, dc_ymax, dc_yby)
          , limits = c(dc_ymin, dc_ymax)
        ) +
        theme_classic()
    )
    
  }