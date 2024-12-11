arrange_eval_plot <-
  function(
    eval_obj
    , threshold = 0.5
    , dc_xmin = 0
    , dc_xmax = 1
    , dc_xby = 0.01
    , dc_ymin = 0
    , dc_ymax = 1
    , dc_yby = 0.01
    , dc_ta_angle = -50
    , labels = LETTERS[1:3]
  ){
    
    ggarrange(
      arrange_calib_plot(eval_obj$calibration, threshold)
      , arrange_dec_plot(
          eval_obj$decision, threshold
          , dc_xmin, dc_xmax, dc_xby
          , dc_ymin, dc_ymax, dc_yby
          , dc_ta_angle
        )
      , arrange_disc_plot(eval_obj$discrimination, threshold)
      , nrow = 1
      , ncol = 3
      , widths = c(5, 4, 6)
      , labels = labels
    )
    
  }