predict_with_shap_waterfall <- function(comorbidity, hba1c, dm_treatment){
  mod_input <-
    data.frame(
      comorbidity = as.integer(comorbidity)
      , hba1c = round(as.numeric(hba1c), 1)
      , dm_treatment = as.integer(dm_treatment)
    )
  
  mod_input <-
    mod_input |>
    left_join(
      mutate(depmod_data, hba1c = round(hba1c, 1))
      , by = colnames(mod_input)
    )
  
  pred_prob <-
    mod_input |>
    left_join(best_model_prob, by = join_by(id)) |>
    pull(pred)
  
  mod_input <-
    mod_input |>
    mutate(
      cluster =
        "SHAP value (impact on model output)"
      , cm =
        paste0(
          "Fatigue (="
          , ifelse(pred_prob >= th[[best_model]], "yes", "no")
          , ")"
        )
    )
  
  mod_input |>
    pull(id) |>
    shap_waterfall_plot(
      dep_formatted_data |>
        left_join(
          select(mod_input, id, cluster, cm), by = join_by(id)
        )
      , th[[best_model]], exp_prob, exp_obs, shap_text_size = 2.5
    ) +
    theme(
      axis.title.x = element_text(size = 8)
      , axis.text.x = element_text(size = 7)
      , axis.text.y = element_text(size = 7)
      , strip.text = element_text(size = 7)
    )
}