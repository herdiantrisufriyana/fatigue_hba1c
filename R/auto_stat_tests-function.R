auto_stat_tests <- 
  function(
    V1
    ,V2
    ,normal_V1 = TRUE
    ,normal_V2 = TRUE
    ,perfect_separation = FALSE
  ){
    if(is.numeric(V1) & is.numeric(V2)){
      func <- function(V1, V2){
        cor.test(
          V1
          ,V2
          ,method =
            ifelse(
              normal_V1 & normal_V2
              ,"pearson"
              ,"spearman"
            )
        )
      }
    }else if(!is.numeric(V1) & !is.numeric(V2)){
      if(perfect_separation){
        data <-
          data.frame(
            V1 = V1
            ,V2 = V2
          )
        
        if(length((unique(V1))) > 2){
          func <- function(V1, V2){
            prior_info <-
              get_prior(
                formula = V1 ~ V2
                ,family = categorical()
                ,data = data
              )
            
            brm(
              formula = V1 ~ V2
              ,family = categorical()
              ,data = data
              ,prior = prior_info
              ,silent = 2
              ,refresh = 0
              ,open_progress = FALSE
            )
          }
        }else{
          func <- function(V1, V2){
            prior_info <-
              get_prior(
                formula = V1 ~ V2
                ,family = bernoulli()
                ,data = data
              )
            
            brm(
              formula = V1 ~ V2
              ,family = bernoulli()
              ,data = data
              ,prior = prior_info
              ,silent = 2
              ,refresh = 0
              ,open_progress = FALSE
            )
          }
        }
      }else{
        func <- function(V1, V2){
          fisher.test(V1, V2)
        }
      }
    }else if(!is.numeric(V1)){
      if(length((unique(V1))) > 2){
        func <- function(V1, V2){
          if(normal_V2){
            data <-
              data.frame(
                V2 = V2
                ,V1 = V1
              )
            
            aov(
              V2 ~ V1
              ,data = data
            )
          }else{
            V2_group_V1 <-
              V1 |>
              unique() |>
              lapply(\(x) V2[V1 == x])
            
            kruskal.test(V2_group_V1)
          }
        }
      }else{
        func <- function(V1, V2){
          V2_group_V1_1 = V2[V1 == unique(V1)[1]]
          V2_group_V1_2 = V2[V1 == unique(V1)[2]]
          if(normal_V2){
            t.test(
              V2_group_V1_1
              ,V2_group_V1_2
              ,alternative = "two.sided"
              ,paired = FALSE
              ,var.equal = FALSE
            )
          }else{
            wilcox.test(
              V2_group_V1_1
              ,V2_group_V1_2
              ,alternative = "two.sided"
              ,paired = FALSE
            )
          }
        }
      }
    }else{
      if(length((unique(V2))) > 2){
        func <- function(V1, V2){
          if(normal_V1){
            data <-
              data.frame(
                V2 = V2
                ,V1 = V1
              )
            
            aov(
              V1 ~ V2
              ,data = data
            )
          }
          else{
            V1_group_V2 <-
              V2 |>
              unique() |>
              lapply(\(x) V1[V2 == x])
            
            kruskal.test(V1_group_V2)
          }
        }
      }else{
        func <- function(V1, V2){
          V1_group_V2_1 = V1[V2 == unique(V2)[1]]
          V1_group_V2_2 = V1[V2 == unique(V2)[2]]
          if(normal_V1){
            t.test(
              V1_group_V2_1
              ,V1_group_V2_2
              ,alternative = "two.sided"
              ,paired = FALSE
              ,var.equal = FALSE
            )
          }
          else{
            wilcox.test(
              V1_group_V2_1
              ,V1_group_V2_2
              ,alternative = "two.sided"
              ,paired = FALSE
            )
          }
        }
      }
    }
    
    V1_no_ms <- V1[!is.na(V1) & !is.na(V2)]
    V2_no_ms <- V2[!is.na(V2) & !is.na(V1)]
    
    func(V1_no_ms, V2_no_ms)
  }