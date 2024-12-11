arrange_calib_plot <- function(calibration_obj, threshold = 0.5){
  labels <-
    calibration_obj$metrics |>
    mutate(
      term = ifelse(term == "rmse", "Brier score", str_to_sentence(term))
    ) |>
    mutate(lb = estimate - ci, , ub = estimate + ci) |>
    mutate_at(c("estimate", "lb", "ub"), format, digits = 3) |>
    unite(ci, lb, ub, sep = ", ") |>
    mutate_at("ci", \(x) paste0("(", x, ")")) |>
    unite(summary, term, estimate, ci, sep = " ") |>
    pull(summary) |>
    paste0(collapse = "\n")
  
  ggarrange(
    calibration_obj$plot +
      geom_vline(xintercept = threshold, lty = 2, na.rm = TRUE) +
      annotate(
        x = 0.1, y = 0.9, geom = "label", label = labels, hjust=0, size=3
      )
    , calibration_obj$dist +
      geom_vline(xintercept = threshold, lty = 2, na.rm = TRUE)
    , nrow = 2
    , ncol = 1
    , heights = c(4, 2)
  )
}