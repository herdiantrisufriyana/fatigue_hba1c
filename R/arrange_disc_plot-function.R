arrange_disc_plot <- function(discrimination_obj, threshold = 0.5){
  discrimination_obj$plot$data |>
    ggplot(aes(tnr, tpr)) +
    geom_abline(slope = 1, intercept = 1, lty = 2) +
    geom_vline(
      xintercept = filter(discrimination_obj$plot$data, th == threshold)$tnr
      , lty = 2, na.rm = TRUE
    ) +
    geom_hline(
      yintercept = filter(discrimination_obj$plot$data, th == threshold)$tpr
      , lty = 2, na.rm = TRUE
    ) +
    geom_path() +
    geom_point(aes(x=ifelse(th == threshold, tnr, NA)), na.rm = TRUE) +
    geom_text(
      aes(label = ifelse(th == threshold, round(th, 2), NA))
      , hjust = -0.1, vjust = 1.1, size = 3
      , check_overlap = TRUE, na.rm = TRUE
    ) +
    coord_equal() +
    scale_x_reverse("Specificity", breaks = seq(0, 1, 0.1), limits = c(1, 0)) +
    scale_y_continuous(
      "Sensitivity", breaks = seq(0, 1, 0.1), limits = c(0, 1)
    ) +
    theme_classic()
}