

pre_mable_fit = function( .startingMonth = NULL ,
                              .missing_reports = NULL ,
                              .evaluation_month = NULL  ,
                              
                              .error = NULL , 
                              forecast = FALSE ,
                              simulate = FALSE ,
                              bootstrap = FALSE , 
                              times = 100 ,
                              distribution = FALSE ,
                              .cat = NULL , ... ){

    trend.data = mable_data(     .startingMonth = .startingMonth ,
                                  .missing_reports = .missing_reports ,
                                  .evaluation_month = .evaluation_month  ,
                                  .error = .error , 
                                  forecast = forecast ,
                                  .cat = .cat )
    
    group_by_cols = groupByCols( period = dataPeriod( trend.data ), 
                                 hts = hts , 
                                 levelNames = levelNames, 
                                 split = split , .cat = .cat  )
    
    n_facilities = mable_facilities(.startingMonth = .startingMonth ,
                                  .missing_reports = .missing_reports ,
                                  .evaluation_month = .evaluation_month  ,
                                  .error = .error
    )
  
    train.data <- trend.data %>% as_tibble %>% 
      filter( Month <= ( yearmonth( .evaluation_month )   ) ) %>%
      as_tsibble( index = Month , key = key_vars( trend.data )) 
      # group_by( Selected, dataSet, RTSS, agegrp )
    
     # test.data  = trend.data %>%
     #          select( - total ) %>%
     #          filter( Month > ( yearmonth( evaluation_month )   )  , 
     #                   Month < ( yearmonth( evaluation_month ) +12 ))
     
     test.data <- trend.data %>% as_tibble %>% 
       filter( Month > ( yearmonth( .evaluation_month )   )  , 
                       Month < ( yearmonth( .evaluation_month ) + horizon )) %>%
      as_tsibble( index = Month , key = key_vars( trend.data ))
      # group_by( Selected, dataSet, RTSS, agegrp )
    
    if ( .cat ) cat("\n - fitting data")
    fit <- train.data %>%
      model(
        ets = ETS( total ),
        arima = ARIMA( total ) ,
        arima.tr = ARIMA( fabletools::box_cox( total , lambda = .5  ) ~  pdq() + PDQ( period = "1 year" )  ) , 
        arima.cov = ARIMA( total ~  pdq() + PDQ( period = "1 year" ) + 
                             xreg( any_stock_out + avg_mm   ) ) ,
        arima.cov.tr = ARIMA( fabletools::box_cox( total , lambda = .5  ) ~  pdq() + PDQ( period = "1 year" ) + 
                                xreg( any_stock_out + avg_mm  ) )
      ) %>%
      mutate( mixed = (ets + arima + arima.tr + arima.cov + arima.cov.tr) / 5)
    
    if ( .cat ) cat("\n - forecasting data")
    
    if ( distribution ){
      
      if ( .cat ) cat("\n - forecasting iterations:" , times )
      
      fc = fit %>% generate( new_data = test.data 
                             # ,  simulate = simulate 
                             # , bootstrap = bootstrap  
                             , times = times ) 
      
      fc_accuracy = key.mpe( fc , trend.data , split = split , var = ".sim" ,  
                             agg_level = agg_level,  .cat = .cat ) 
    } else {
      
      fc <- fit %>% forecast( new_data = test.data )
      
      fc_accuracy <- accuracy( fc, trend.data,
                               measures = list(
                                 point_accuracy_measures
                                 # , interval_accuracy_measures
                                 # , distribution_accuracy_measures
                               )
      )
    }
    
    fc_accuracy = fc_accuracy  %>%  left_join( n_facilities  )
    
    
    if ( forecast ){
      i = list()
      i$i = fc_accuracy 
      i$forecast = fc 
    } else {
      i = fc_accuracy 
    }
    

return( i )
}

mable_data = function(         
                        ml.rtss.data = ml.rtss ,
                        .orgUnit = TRUE , # group by orgunit
                        .startingMonth = NULL ,
                        .endingMonth = NULL , 
                        .missing_reports = NULL ,
                        # .evaluation_month = NULL  ,
                        .error = NULL , 
                        covariates = covariates , 
                        .split = split , 
                        aggregate_by_split = TRUE ,
                        .cat = NULL , ...
){
  
      d = ml.rtss.data %>% 
      error_factor %>% 
      # filter( Month >= ( yearmonth( startingMonth )   ) )  %>%
      cleanedData( . , 
                   error =  .error , .cat = .cat ) %>%
      
      selectedData(  startingMonth = .startingMonth ,
                     # data_categories = "agegrp" , 
                     endingMonth = .endingMonth ,
                     missing_reports = .missing_reports ,
                     .cat = .cat ) %>% as_tibble()
  
      group_by_cols = groupByCols( period = dataPeriod( d ) , 
                                 orgUnit = .orgUnit ,
                                 hts = hts , 
                                 levelNames = levelNames, 
                                 split = .split , .cat = .cat )
    
   
      num_facilities = mostFrequentReportingOUs( d ,
                                               startingMonth = .startingMonth ,
                                               endingMonth = .endingMonth ,
                                               missing_reports = .missing_reports , 
                                               .cat = .cat ) %>% length()
    
      .dataSets = unique( d$dataSet ) 
      num_datasets = length( .dataSets )
    
      data.total = dataTotal( data = d , 
                            group_by_cols = group_by_cols , 
                            dataSets = .dataSets, 
                            covariates = covariates , 
                            .cat = .cat  )
    
    if ( hts ){
      
      hts_formula = htsFormula( hts = hts , 
                                levelNames = levelNames ,  
                                agg_level = agg_level , 
                                all.levels = FALSE  , 
                                num_facilities =  num_facilities ,
                                num_datasets = num_datasets , 
                                split = split , .cat = .cat ) 
      
      data.agg.ts = htsData( data = data.total ,  
                          hts = hts , 
                          hts_formula = hts_formula , 
                          covariates = covariates , 
                          group_by_cols = group_by_cols , .cat = .cat )
    } else {
      
      data.agg.ts = aggData( data.total = data.total ,  
                             covariates = covariates , 
                             group_by_cols = group_by_cols , .cat = .cat )
    }
    
  

    trend.data = trendData( .d = data.agg.ts , 
                          levelNames = levelNames , 
                          startingMonth = .startingMonth , 
                          endingMonth = .endingMonth ,
                          selected.only = selected.only  ,
                          num_facilities =  num_facilities ,
                          num_datasets = num_datasets , 
                          split = split ,
                          agg_level = agg_level  , 
                          remove.aggregate = remove.aggregate, .cat = .cat  )
   
}

mable_facilities = function(  ml.rtss.data = ml.rtss ,
                              .startingMonth = NULL ,
                              .endingMonth = NULL ,
                              .missing_reports = NULL ,
                              .error = NULL , ...
){
  
      d = ml.rtss.data %>% 
      error_factor %>% 
      # filter( Month >= ( yearmonth( startingMonth )   ) )  %>%
      cleanedData( . , 
                   error =  .error , .cat = .cat ) %>%
      
      selectedData(  startingMonth = .startingMonth ,
                     endingMonth = .endingMonth ,
                     missing_reports = .missing_reports ,
                     .cat = .cat ) %>% as_tibble()
  
     group_by_cols = groupByCols( period = dataPeriod( d ), 
                                 hts = hts , 
                                 levelNames = levelNames, 
                                 split = split , .cat = .cat  )
    
    .dataSets = unique( ml.rtss$dataSet ) 
    
    data.total = dataTotal( data = d , 
                            group_by_cols = group_by_cols , 
                            dataSets = .dataSets, 
                            covariates = covariates , 
                            .cat = .cat  )
    
            # n facilities per group  
    n_facilities = data.total %>% 
      group_by_at( setdiff( group_by_cols, c("agegrp", "Month", "orgUnit", "Cluster") ) ) %>%
      summarise( clusters = n_distinct( Cluster ) , 
                   facilities = n_distinct( orgUnit )) 
    
}



trend.chart = function( 
                        trend_data = NULL ,
                        filter_data = NULL , 
                        # ml_data = ml.rtss , 
                        .size = .8 ,  
                        .font = 11 , 
                        .endingMonth = NULL ,
                        facet_row = NULL ,
                        facet_col = NULL ,
                        highlight_peak_periods = NULL ,
                        display_start =  NA  ,
                        .caption = NULL ,
                        ...   ){
  
  if (is.null( trend_data )){
    cat( "\n trend.data required for trend.chart()")
    return()
  }
  
  if ( !is.null( filter_data ) ){
    trend_data = trend_data %>% filter( eval( filter_data ))
  }
  
  # base trend plot (impact plots will add to this one)  
  RTSS_labels = c(  `TRUE` = 'RTSS' ,  `FALSE` = 'no-RTSS' )
  agegrp_labels = c( 'under5' = "Under5" , 'over4' = 'Over4' )
  
  global_labeller <- labeller(
    RTSS = RTSS_labels ,
    agegrp = agegrp_labels  
  )
  
  g = trend_data %>%
    filter( !is.na( total ) ) %>%
    ggplot( aes( x = !! rlang::sym( .period ), y = total 
                 , group =  agegrp  # as.character( !! rlang::sym( input$agg_level  ) )
                 , color =  agegrp
                 # , linetype = agegrp
    ) )  +
    scale_color_brewer( type = "qual", palette = 'Dark2' ) + 
    scale_fill_brewer( type = "qual", palette = 'Dark2' ) + 
    geom_line( linewidth = .size / 2  , alpha = .75 ) +
    theme_minimal() +
    labs( x = NULL , y = 'Cases' ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0) , 
          legend.position = "bottom" , 
          text=element_text( size = .font ) )  
  # + 
  #   geom_vline( xintercept = yearmonth(evaluation_month, format = "%M %Y") %>% as.Date , 
  #               col = 'blue', alpha = .75 , 
  #               linewidth = .35  ) 
  
  if (! is.na( display_start )){
     g = g +  scale_x_yearmonth( limits = c( yearmonth(display_start) %>% as.Date() , NA ) ,
                       date_breaks = "12 months"  ) 

  } else {
      g = g +  scale_x_yearmonth( limits = c( yearmonth( startingMonth) %>% as.Date() , NA ) ,
                       date_breaks = "12 months"  ) 
  }
  
  if ( ! is.null( facet_row ) ){
    
    # facet_grid (campaignNet  ~ RTSS , labeller = global_labeller , scales = 'free' ) +
    g = g + facet_grid (  rows = vars( !! rlang::sym( facet_row )  ) , 
                          cols  = vars( !!  rlang::sym( facet_col )  ) , 
                          scales = 'free' ,
                          labeller = global_labeller )
    
  } else {
    g = g + facet_grid (  rows =  NULL  , 
                          cols  =  vars( !! rlang::sym( facet_col ) )  , 
                          scales = 'free',
                          labeller = global_labeller  )
  }
  
  if ( ! is.null( highlight_peak_periods ) ){
    
    g= g +
    geom_rect( data = highlight_peak_periods, 
               inherit.aes=FALSE, 
               aes( xmin = start, xmax = end, ymin= 0,
                  ymax = Inf , group = group), 
               color="transparent", 
               fill="orange", alpha=0.1) +
      labs( caption = .caption 
      )
  }
  
  return( g )
  
}


impact_chart = function( accuracy_data = NULL ,  
                         base_chart = NULL , 
                         trend_data = NULL , 
                         forecast_data = NULL , 
                         size = 2 , font = 11  , 
                         facility_label = TRUE ,
                         cluster_label = TRUE ,
                         .title = NULL ,
                         .subtitle = NULL , 
                         .caption = NULL , 
                         ci = FALSE , 
                         facet_row = NULL ,
                         facet_col = NULL ,
                         legend = TRUE ,
                         highlight_peak_periods = NULL ,
                         display_start = NA ,
                         no.xaxis = FALSE ,
                         ... ){ 
  
  if (is.null( accuracy_data ) | 
      is.null( forecast ) | 
      is.null( trend_data) ){
    cat("\n impact_chart() requires forecast and accuracy data")
    return()
  }
  
  if ( is.null( base_chart ) ) base_chart = trend.chart( trend_data , 
                              .size = size , .font = font , 
                              facet_row = facet_row ,
                              facet_col = facet_col ,
                              .caption = .caption , 
                              highlight_peak_periods = highlight_peak_periods ,
                              display_start = display_start )
  
  g = 
    base_chart + 
    geom_line( data = forecast_data , aes( x = Month, y = .mean , 
                                           color = agegrp , linetype = agegrp  ) , 
               # color = "black" , 
               # alpha = .5 ,
               linewidth = size /2  , 
               linetype = 'dotted'
               # , size = size 
    ) +
    
    
    labs( title =  .title , subtitle = .subtitle 
          # subtitle = paste(  min(forecast_data$Month) , "to" ,
          #                    max(forecast_data$Month) ) 
  ) +
    
    geom_vline( xintercept = min(forecast_data$Month) %>% as.Date , 
                col = 'blue', linewidth = .35 , alpha = .75  ) +
    
    geom_text( data = accuracy_data %>% 
                 inner_join( trend_data %>% as_tibble %>%
                               group_by_at( key_vars( trend_data ) ) %>%
                               summarise( 
                                         Month = max( Month ) ,
                                         mid_max = max( total , na.rm = T ) /2 
                                         ) 
                   ), 
                     aes(
                        # x = yearmonth( evaluation_month, format = "%M %Y") %>% as.Date()  ,
                        # y = Inf ,
                        x = Month   ,
                        y = mid_max  ,
                        label = scales::percent( MPE/100, 0.1) ,
                        color = agegrp 
                        ) ,
                     # color = 'black',
                     size = size * 2  ,
                     fontface = 2 ,
                     hjust = -0.1 , vjust = 1
                     ) + 
    expand_limits( x = c(NA, max( forecast_data$Month) + 12 ) ) 
  
  if ( ! legend ) g = g + theme(legend.position = "none") 
  
  if ( ci ){
    g = g  + geom_ribbon(data = forecast_data,  
                         # inherit.aes = FALSE ,
                         aes(
                             x = Month, 
                             y = .mean ,
                             ymin = pred_lower,
                             ymax = pred_upper,
                             # linetype = agegrp , 
                             # color = agegrp ,
                             fill = agegrp  ), 
                         alpha = .1 , linewidth = .1  )
    
    # g = g  + geom_hilo_ribbon( data = forecast_data, 
    #                            aes( hilo = `80%`) ) 
    
    
  }
  
   if ( cluster_label ){
    g = g + 
      geom_text( data = accuracy_data , 
                       aes(x = Inf  , 
                           y = Inf , label = paste( clusters , 'clusters\n' , facilities , 'facilities' ) 
                           ) ,
                       color = "grey40" ,
                       size = size , 
                       fontface = 1 , 
                       hjust = 1 , vjust = 1 ) 
   }
  
  # if ( facility_label ){
  #   g = g + 
  #     geom_text( data = accuracy_data , 
  #                      aes(x = Inf  , 
  #                          y = Inf , label = paste( '/n' , facilities , 'facilities' ) 
  #                          ) ,
  #                      color = "grey40" ,
  #                      size = size , 
  #                      fontface = 1 , 
  #                      hjust = 1 , vjust = 1 ) 
  # }
  
  g = g +

    theme(
          axis.text.x = element_text(angle = -90, hjust = 0) ,
          # legend.position = "bottom" ,
          text = element_text( size = font ) 
          )
  
  if (no.xaxis)  g = g +

    theme(
          # legend.position = "bottom" ,
          text = element_text( size = font ) ,
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
          )
  
  return( g )
}


