shap_beeswarm_plot <- function(data, seed, scaled = TRUE, transparency = 1){
  
  if(scaled){
    data <-
      data |>
      group_by(feature) |>
      mutate(feature_value = scale(feature_value)) |>
      ungroup() 
  }
  set.seed(seed)
  
  data <-
    data |>
    group_by(feature) |>
    mutate(
      direction = mean(shap_value >= 0)
      , magnitude = max(shap_value)
    ) |>
    ungroup() |>
    mutate(
      magnitude =
        (magnitude - min(magnitude)) / (max(magnitude) - min(magnitude))
      ,impact =
        0.5 * direction + 0.5 * magnitude
    ) |>
    mutate(feature = reorder(feature, magnitude)) |>
    arrange(seq, feature) |>
    mutate(shap_value_bin = round(shap_value * 100, 0) / 100) |>
    group_by(feature, shap_value_bin) |>
    mutate(freq = n()) |>
    ungroup()|>
    mutate(
      jitter_width = freq / max(freq) * 0.3
      , feature_num = as.numeric(feature)
      , jitter_feature = feature_num + runif(n(), -jitter_width, jitter_width)
    )
  
  data |>
    ggplot(aes(x = jitter_feature, color = feature_value)) +
    geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
    geom_errorbar(
      data =
        data |>
        mutate(
          feature_value_cat =
            ifelse(
              feature_value >= 0
              , max(data$feature_value)
              , min(data$feature_value)
            )
        ) |>
        group_by(feature_num, feature_value_cat) |>
        summarize(
          avg = mean(shap_value)
          , std = sd(shap_value)
          , .groups = "drop"
        ) |>
        mutate(
          lb = avg - qnorm(0.975) * std
          , ub = avg + qnorm(0.975) * std
        )
      , aes(
          x =
            ifelse(feature_value_cat == max(feature_value_cat), feature_num, NA)
          , ymin = lb
          , ymax = ub
          , color = feature_value_cat
        )
      , width = 0.25, na.rm = TRUE
    ) +
    geom_point(
      aes(y = shap_value)
      , position = "identity"
      , size = 1.5
      , alpha = transparency
    ) +
    coord_flip() +
    scale_x_continuous(
      breaks = unique(select(data, feature, feature_num))$feature_num
      ,labels =
        paste0(
          unique(select(data, feature, feature_num))$feature
          , ' - '
          , max(data$feature_num)
            - unique(select(data, feature,feature_num))$feature_num
            + 1
        )
    ) +
    scale_color_gradient(
      "Feature value"
      ,low = "#008AFB"
      ,high = "#FF0053"
      ,breaks = c(min(data$feature_value), max(data$feature_value))
      ,labels = c("Low", "High")
    ) +
    theme_minimal() +
    xlab("") +
    ylab("SHAP value (impact on model output)") +
    theme(
      panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , axis.ticks.x = element_line()
      , axis.line.x = element_line()
      , legend.position = "right"
      , legend.title = element_text(angle = 90, hjust = 0.5)
    ) +
    guides(
      color = 
        guide_colorbar(
          barwidth = 0.2
          , barheight = 4
          , title.position = "right"
          , title.hjust = 0.5
          , label.hjust = 0.5
          , ticks = FALSE
          , draw.ulim = TRUE
          , draw.llim = TRUE
        )
    )
}