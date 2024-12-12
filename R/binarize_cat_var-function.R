binarize_cat_var <- function(colname, data){
  if(is.factor(data[[colname]])){
    cat <-
      as.numeric(data[[colname]])[!is.na(data[[colname]])] |>
      unique() |>
      sort()
    
    data <-
      data |>
      select_at(colname) |>
      rownames_to_column(var = "id") |>
      gather(variable, value, -id) |>
      mutate(value = factor(value, levels(data[[colname]]))) |>
      mutate(variable = paste0(colname, "_", as.numeric(value))) |>
      mutate(value = ifelse(is.na(value), NA, 1)) |>
      filter(variable != paste0(colname, "_NA")) |>
      spread(variable, value, fill = 0) |>
      select_at(paste0(colname, "_", cat[-1]))
    
    if(ncol(data)==1){
      data <-rename_all(data, str_remove_all, "_[:digit:]+$")
    }
    
    data
  }else{
    data |>
      select_at(colname)
  }
}