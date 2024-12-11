thresholding <-
  function(
    eval_df
    , standard = TRUE
    , optimal = TRUE
    , clinical = TRUE
    , custom_metric = NULL
    , custom_ref = NULL
    , boot = 30
    , seed
  ){
    
    source("R/compute_metrics-function.R")
    
    cm_metrics <- list()
    
    for(i in seq(boot)){
      set.seed(seed + i)
      cm_metrics[[i]] <-
        compute_metrics(
          eval_df[sample(seq(nrow(eval_df)), nrow(eval_df), TRUE), ]
          , p = FALSE
          , f1 = TRUE
          , interp = FALSE
        )$metrics
    }
    
    cm_metrics <-
      cm_metrics |>
      reduce(rbind)
    
    thresholds = list()
    
    if(standard){
      thresholds$standard <-
        cm_metrics |>
        filter(th == 0.5) |>
        gather(metric, value, -th) |>
        mutate(ref_type = "Threshold") |>
        rename(ref_value = th) |>
        select(ref_type, everything())
    }
    
    if(optimal){
      thresholds$optimal <-
        cm_metrics |>
        mutate(avg_acc = (tpr + tnr) / 2) |>
        filter(avg_acc == max(avg_acc)) |>
        gather(metric, value, -avg_acc) |>
        mutate(ref_type = "(TPR+TNR)/2") |>
        rename(ref_value = avg_acc) |>
        select(ref_type, everything())
    }
    
    if(clinical){
      thresholds$clinical <-
        cm_metrics |>
        filter(abs(tnr-0.9) == min(abs(tnr-0.9))) |>
        gather(metric, value, -tnr) |>
        mutate(ref_type = "TNR") |>
        rename(ref_value = tnr) |>
        select(ref_type, everything())
    }
    
    if(!is.null(custom_metric) & !is.null(custom_ref)){
      thresholds$custom <-
        cm_metrics |>
        rename_at(custom_metric, \(x)"custom_metric") |>
        filter(custom_metric == custom_ref) |>
        gather(metric, value, -custom_metric) |>
        rename(ref_value = custom_metric) |>
        mutate(ref_type = str_to_upper(custom_metric)) |>
        select(ref_type, everything())
    }
    
    thresholds |>
      reduce(rbind) |>
      group_by(ref_type, ref_value, metric) |>
      summarize(
        avg = mean(value)
        , lb = mean(value) - qnorm(0.975) * sd(value) / sqrt(n())
        , ub = mean(value) + qnorm(0.975) * sd(value) / sqrt(n())
        , .groups = "drop"
      ) |>
      mutate(
        avg =
          ifelse(
            metric %in% c("th", "nb", "f1")
            , round(avg, 2)
            , round(avg * 100, 2)
          )
        , lb =
          ifelse(
            metric %in% c("th", "nb", "f1")
            , round(lb, 2)
            , round(lb * 100, 2)
          )
        , ub =
          ifelse(
            metric %in% c("th", "nb", "f1")
            , round(ub, 2)
            , round(ub * 100, 2)
          )
      )
    
  }