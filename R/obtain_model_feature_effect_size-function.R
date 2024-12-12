obtain_model_feature_effect_size <- function(eval_df, th = 0.5){
  predictors |>
    `names<-`(predictors) |>
    lapply(c, "outcome") |>
    lapply(rev) |>
    lapply(paste0, collapse='~') |>
    lapply(as.formula) |>
    lapply(
      glm
      , family = binomial()
      , data =
        predmod_data |>
        select(-outcome) |>
        mutate_at(predictors, scale) |>
        left_join(
          eval_df |>
            mutate(outcome = factor(as.integer(pred >= th))) |>
            select(id, outcome)
          , by = join_by(id)
        ) |>
        column_to_rownames(var = "id")
    ) |>
    lapply(tidy) |>
    imap( ~ mutate(.x, variable = .y)) |>
    reduce(rbind) |>
    select(variable, everything()) |>
    mutate(
      term = str_remove_all(term, variable)
      ,term = ifelse(term == "", "value", term)
      ,OR = exp(estimate)
      ,LB = exp(estimate - qnorm(0.975) * std.error)
      ,UB = exp(estimate + qnorm(0.975) * std.error)
    )  |>
    filter(term != "(Intercept)")
}