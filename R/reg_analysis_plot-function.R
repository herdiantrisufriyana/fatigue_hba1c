reg_analysis_plot <- function(data, strata = NULL){
  
  data <-
    data |>
    mutate(variable = reorder(variable, UB)) |>
    arrange(variable) |>
    mutate(variable_num = as.numeric(variable))
  
  if(is.null(strata)){
    p <-
      data |>
      ggplot(aes(variable_num, log2(OR)))
  }else{
    variable_label <-
      data.frame(
        variable_num = unique(select(data, variable, variable_num))$variable_num
        , label =
          paste0(
            unique(select(data, variable, variable_num))$variable
            , ' - '
            , max(data$variable_num)
            - unique(select(data, variable, variable_num))$variable_num
            + 1
          )
      )
    
    data <-
      data |>
      rename_at(strata, \(x) "strata") |>
      left_join(variable_label,  by = join_by(variable_num)) |>
      mutate_at("label", \(x) factor(x, rev(unique(x))))
    
    p <-
      data |>
      ggplot(aes(as.numeric(strata), log2(OR), color = strata))
  }
  
  p <-
    p +
    geom_errorbar(aes(ymin = log2(LB), ymax = log2(UB)), width = 0.25) +
    geom_point() +
    geom_hline(yintercept = log2(1), color = "grey", linewidth = 1)
  
  if(!is.null(strata)){
    p <-
      p +
      facet_grid(label ~ ., switch = "y")
  }
  
  p <-
    p +
    coord_flip()
  
  if(is.null(strata)){
    p <- p +
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
      )
  }else{
    p <-
      p +
      scale_x_reverse()
  }
  
  p <-
    p +
    theme_minimal() +
    xlab("") +
    scale_y_continuous(
      "log2(OR) (95% CI)"
      , limits = c(log2(1/2^6), log2(2^6))
      , breaks = log2(c(rev(1/2^seq(6)), 1, 2^seq(6)))
    ) +
    theme(
      panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , axis.ticks.x = element_line()
      , axis.line.x = element_line()
      , legend.position = "right"
      , legend.title = element_text(angle = 90, hjust = 0.5)
    )
  
  if(!is.null(strata)){
    p <-
      p +
      theme(
        axis.text.y = element_blank()
        , strip.text.y.left = element_text(angle = 0, hjust = 1)
        , strip.placement = "outside"
        , legend.title = element_blank()
      )
  }
  
  p
}