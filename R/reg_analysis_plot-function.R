reg_analysis_plot <- function(data){
  data <-
    data |>
    mutate(variable = reorder(variable, UB)) |>
    arrange(variable) |>
    mutate(variable_num = as.numeric(variable))
  
  data |>
    ggplot(aes(variable_num, log(OR))) +
    geom_errorbar(aes(ymin = log(LB), ymax = log(UB)), width = 0.25) +
    geom_point() +
    geom_hline(yintercept = log(1), color = "grey", linewidth = 1) +
    coord_flip() +
    scale_x_continuous(
      breaks = unique(select(data, variable, variable_num))$variable_num
      ,labels =
        paste0(
          unique(select(data, variable, variable_num))$variable
          , ' - '
          , max(data$variable_num)
          - unique(select(data, variable, variable_num))$variable_num
          + 1
        )
    ) +
    theme_minimal() +
    xlab("") +
    scale_y_continuous(limits = c(log(1/60), log(60))) +
    theme(
      panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , axis.ticks.x = element_line()
      , axis.line.x = element_line()
      , legend.position = "right"
      , legend.title = element_text(angle = 90, hjust = 0.5)
    )
}