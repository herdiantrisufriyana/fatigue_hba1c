compute_metrics <-
  function(
    eval_df
    , p = TRUE
    , tpr = TRUE
    , tnr = TRUE
    , ppv = TRUE
    , npv = TRUE
    , nb = TRUE
    , f1 = FALSE
    , interp = FALSE
  ){
    
    cm <-
      eval_df |>
      cbind(
        seq(0, 1, 0.01) |>
          matrix(
            nrow = 1
            , byrow = TRUE
            , dimnames = list(NULL, seq(0, 1, 0.01))
          ) |>
          as.data.frame()
      )
    
    cm <-
      cm |>
      mutate(seq = seq(nrow(cm))) |>
      select(seq, everything()) |>
      gather(th, value, -seq, -pred, -obs) |>
      mutate(
        pred =
          factor(
            ifelse(pred >= value, levels(obs)[2], levels(obs)[1])
            , levels(obs)
          )
      ) |>
      group_by(th) |>
      summarize(
        tp = sum(obs  ==  levels(obs)[2] & pred == levels(obs)[2])
        , fn = sum(obs == levels(obs)[2] & pred == levels(obs)[1])
        , fp = sum(obs == levels(obs)[1] & pred == levels(obs)[2])
        , tn = sum(obs == levels(obs)[1] & pred == levels(obs)[1])
        , .groups = "drop"
      ) |>
      mutate(th = as.numeric(th))
    
    metrics <- cm
    
    if(p){
      metrics <-
        metrics |>
        mutate(p = (tp + fn) / (tp + fn + fp + tn))
    }
    
    if(tpr){
      metrics <-
        metrics |>
        mutate(tpr = tp / (tp + fn))
    }
    
    if(tnr){
      metrics <-
        metrics |>
        mutate(tnr = tn / (fp + tn))
    }
    
    if(ppv){
      metrics <-
        metrics |>
        mutate(ppv = tp / (tp + fp))
    }
    
    if(npv){
      metrics <-
        metrics |>
        mutate(npv = tn / (tn + fn))
    }
    
    if(nb){
      metrics <-
        metrics |>
        mutate(nb = (tp - fp * th / (1 - th)) / (tp + fn + fp + tn))
    }
    
    if(f1){
      metrics <-
        metrics |>
        mutate(
          f1 =
            2 * (
              (tp / (tp + fn) * tp / (tp + fp))
              / (tp / (tp + fn) + tp / (tp + fp))
            )
        )
    }
    
    metrics <-
      metrics |>
      select(-tp, -fn, -fp, -tn) |>
      mutate_all(round, 4)
    
    if(interp){
      for(colname in colnames(metrics)){
        metric_min_max <-
          select_at(metrics, colname) |>
          `names<-`("metric") |>
          filter(!is.na(metric)) |>
          filter(!is.nan(metric)) |>
          filter(metric == min(metric)| metric == max(metric)) |>
          unique() |>
          arrange(metric) |>
          pull(metric)
        
        metrics <-
          metrics |>
          full_join(
            data.frame(
                metric =
                  seq(metric_min_max[1], metric_min_max[2], 0.0001)
              ) |>
              `names<-`(colname)
            , by = colname
          )
        
        for(colname2 in colnames(metrics)[colnames(metrics) != colname]){
          metrics <-
            metrics |>
            arrange_at(c(colname, colname2)) |>
            mutate_at(
              colname2
              , \(x) approx(seq(length(x)), x, seq(length(x)))$y
            )
        }
        
        metrics <-
          metrics |>
          mutate_all(round, 4)
      }
    }
    
    list(cm = cm, metrics = metrics)
    
  }