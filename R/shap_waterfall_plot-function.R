shap_waterfall_plot <- function(identifier, data, threshold, exp_prob, exp_obs){
  
  data <-
    data |>
    filter(id == identifier) |>
    arrange(abs(shap_value)) |>
    mutate(
      feature = reorder(feature, abs(shap_value))
      , shap_end = exp_prob + cumsum(shap_value)
      , shap_direction =
        ifelse(shap_value >= 0, "Positive", "Negative") |>
        factor(c("Negative", "Positive"))
    )
  
  data <-
    data |>
    mutate(seq=seq(nrow(data)))
  
  data <-
    data |>
    left_join(
      mutate(data, seq = seq + 1) |>
        select(seq, shap_start = shap_end)
      , by = join_by(seq)
    ) |>
    mutate(shap_start = ifelse(is.na(shap_start), exp_prob, shap_start)) |>
    mutate(
      shap_offset =
        ifelse(
          shap_start <= shap_end
          , shap_start + (shap_end - shap_start)/2
          , shap_end + (shap_start - shap_end)/2
        )
      , exp_prob_offset =
        ifelse(
          exp_prob <= last(shap_end)
          , exp_prob + (last(shap_end) - exp_prob)/2
          , last(shap_end) + (exp_prob - last(shap_end))/2
        )
      , shap_pos_v1 =
        ifelse(abs(shap_end - shap_start) > 0.1, shap_end, shap_start)
      , shap_pos_v2 =
        ifelse(abs(shap_end - shap_start) > 0.1, shap_start, shap_end)
      , shap_pos =
        ifelse(
          shap_offset >= exp_prob_offset
          , ifelse(shap_start <= shap_end, shap_pos_v1, shap_pos_v2)
          , ifelse(shap_start <= shap_end, shap_pos_v2, shap_pos_v1)
        )
    ) |>
    mutate(
      shap_end = ifelse(shap_end <= 0, 0, ifelse(shap_end >= 1, 1, shap_end))
      , shap_pos = ifelse(shap_pos <= 0, 0, ifelse(shap_pos >= 1, 1, shap_pos))
    )
  
  shap_direction_color <-
    data.frame(
      shap_direction =
        factor(c("Negative", "Positive"), c("Negative", "Positive"))
      , code = c("#008AFB", "#FF0053")
    ) |>
    filter(shap_direction %in% data$shap_direction)
  
  data |>
    ggplot(aes(feature)) +
    geom_hline(
      yintercept = threshold, color ="grey", linewidth = 0.5, lty = 2
    ) +
    geom_hline(yintercept = exp_obs, color = "grey", linewidth = 0.5, lty = 3) +
    geom_hline(
      yintercept = last(data$shap_end), color = "grey", linewidth = 0.5, lty = 1
    ) +
    geom_segment(
      aes(xend = feature, y = shap_start, yend = shap_end, color = shap_direction)
      , linewidth = 9
    ) +
    geom_text(
      aes(label = sprintf("%+0.2f", shap_value), y = shap_pos)
      , hjust = "inward", size = 3
    ) +
    facet_grid(cm ~ cluster) +
    coord_flip() +
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
    scale_color_manual(values = shap_direction_color$code) +
    theme_minimal() +
    xlab("") +
    ylab("Predicted probability (model output)") +
    theme(
      panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , axis.line.x = element_line()
      , axis.ticks.x = element_line()
      , axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0.5)
      , legend.position = "none"
    )
}