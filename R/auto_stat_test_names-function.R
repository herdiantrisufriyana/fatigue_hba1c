auto_stat_test_names <- 
  function(
    V1
    ,V2
    ,normal_V1 = TRUE
    ,normal_V2 = TRUE
    ,perfect_separation = FALSE
  ){
    if(is.numeric(V1) & is.numeric(V2)){
      ifelse(
        normal_V1 & normal_V2
        ,"Pearson's correlation test"
        ,"Spearman's correlation test"
      )
    }else if(!is.numeric(V1) & !is.numeric(V2)){
      if(perfect_separation){
        data <-
          data.frame(
            V1 = V1
            ,V2 = V2
          )
        
        if(length((unique(V1))) > 2){
          "Bayesian multinomial logistic regression analysis"
        }else{
          "Bayesian binomial logistic regression analysis"
        }
      }else{
        "Fisher's exact test"
      }
    }else if(!is.numeric(V1)){
      if(length((unique(V1))) > 2){
        if(normal_V2){
          "Analysis of variance (ANOVA)"
        }else{
          "Kruskal-Wallis test"
        }
      }else{
        if(normal_V2){
          "Welch's t-test"
        }else{
          "Mann-Whitney U test (Wilcoxon rank-sum test)"
        }
      }
    }else{
      if(length((unique(V2))) > 2){
        if(normal_V1){
          "Analysis of variance (ANOVA)"
        }else{
          "Kruskal-Wallis test"
        }
      }else{
        if(normal_V1){
          "Welch's t-test"
        }else{
          "Mann-Whitney U Test (Wilcoxon rank-sum Test)"
        }
      }
    }
  }