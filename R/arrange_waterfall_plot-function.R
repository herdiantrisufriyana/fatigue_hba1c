arrange_waterfall_plot <-
  function(cm_type, data, metadata, top = FALSE, bottom = FALSE){
    p1 <-
      data[[
        filter(metadata, cluster == 1 & cm == cm_type)$id
      ]] +
      theme(strip.text.y = element_blank())
    
    p2 <-
      data[[
        filter(metadata, cluster == 2 & cm == cm_type)$id
      ]]
    
    if(!top){
      p1 <- p1 + theme(strip.text.x = element_blank())
      p2 <- p2 + theme(strip.text.x = element_blank())
    }
    
    if(!bottom){
      p1 <-
        p1 +
        theme(
          axis.title.x = element_blank()
          , axis.ticks.x = element_blank()
          , axis.text.x = element_blank()
        )
      
      p2 <-
        p2 +
        theme(
          axis.title.x = element_blank()
          , axis.ticks.x = element_blank()
          , axis.text.x = element_blank()
        )
    }
    
    ggarrange(p1, p2, nrow = 1, ncol = 2, widths = c(5.25, 4.75))
  }