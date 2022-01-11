


htmlOutput("mad_value")

 # Print the MAD value
  output$mad_value <- renderText({
    
    req( input$mad )
    

      HTML("You've selected a MAD cutoff <code>", 
           input$mad, 
           "for extreme values", 
           "</code>")

  })

### Extreme Outliers 
pb = NULL
# .total = reactive({ length( key_size( dTs() ) ) })
# pb <- progress_bar$new( 
#     format = ":current :percent  [:bar] :elapsedfull",
#     total = .total() , clear = FALSE, width= 50 )

mad = reactive({ input$mad })

dTs.extreme = reactive({ 
  
  print( 'dTs.extreme ... ' )
  print( paste( 'MAD is ', mad() ) )
  
  dtse = 
    d() %>%
    filter( orgUnit %in% selectedOUs() ) %>%
    as_tsibble( index = period, key = orgUnit ) %>% 
    pivot_wider( id_cols = c( orgUnit , period ) , 
                 names_from = "vars", 
                 values_from = "dataCol" ) %>%
    group_by( orgUnit ) %>% 
    mutate( across( all_of( dataCols$x ) , 

                                ~clean_ts( . ,
                      interpolate = FALSE ,
                      .clean = "MAD" ,
                      MAD = as.numeric( mad() ) # median absolute deviation > 5
                      , .pb = pb
                      )  
                    # , otherwise = NA )
    ) 
           
    ) 
  
  # %>% 
  # 
  #   pivot_longer( {{ dataCols$x }} ,
  #                 names_to =  'vars' ,
  #                 values_to = 'cleanEO' )
  
  # glimpse( dtse )
  print( 'dTs.extreme ... finished' )
  return( dtse )
})

dTs.extreme.total = reactive({
  
  
  dTs.extreme() %>% 
    ungroup() %>%
    summarise( across( all_of(  dataCols$x ) , sum, na.rm = TRUE ) )
  
})

# plot_mad
plotOutput( 'plot_mad' , 
    click = "plot_mad_click" ,
    dblclick = "plot_mad_dblclick" ,
    hover = "plot_mad_hover" ,
    brush = "plot_mad_brush" )

plot_mad = reactive({
  
  if ( !is.character( dataCols$x ) ) return()
  
  g = ggplot( dTs.extreme.total() %>% 
                pivot_longer( dataCols$x  ,
                  names_to =  'vars' ,
                  values_to = 'cleanEO' ) ,
              aes( x = period , y = cleanEO ) ) +
    facet_wrap( ~ vars , scales = 'free' ) +
    geom_line()

  return(g)
})

output$plot_mad <- renderPlot({  plot_mad()  })

### STL Outliers 

pb = NULL

mad = reactive({ input$mad })

dTs.stl = reactive({ 
  
  print( 'dTs.stl ... ' )

  dtsstl =  
    dTs.extreme() %>% 
    filter( orgUnit %in% unique( dTs.extreme()$orgUnit )[] ) %>%
    group_by( orgUnit ) %>% 
    mutate( across( all_of( dataCols$x ) , 

            ~clean_ts( . ,
                      interpolate = FALSE ,
                      .clean = 'tsclean' ,
                      MAD = as.numeric( mad() ) # median absolute deviation > 5
                      , .pb = pb
                      )  
                    # , otherwise = NA )
    ) 
    )
  
  # glimpse( dtsstl )
  print( 'dTs.stl ... finished' )
  return( dtsstl )
})

dTs.stl.total = reactive({
  
  
  dTs.stl() %>% 
    ungroup() %>%
    summarise( across( all_of(  dataCols$x ) , sum, na.rm = TRUE ) )
  
})
# plot_dTs.stl , eval=FALSE}

plotOutput( 'plot_dTs.stl' , 
    click = "plot_dTs.stl_click" ,
    dblclick = "plot_dTs.stl_dblclick" ,
    hover = "plot_dTs.stl_hover" ,
    brush = "plot_dTs.stl_brush" )

plot_dTs.stl = reactive({
  
  if ( !is.character( dataCols$x ) ) return()
  
  g = ggplot( dTs.stl.total() %>% 
                pivot_longer( dataCols$x  ,
                  names_to =  'vars' ,
                  values_to = 'cleanEO' ) ,
              aes( x = period , y = cleanEO ) ) +
    facet_wrap( ~ vars , scales = 'free' ) +
    geom_line()

  return(g)
})

output$plot_dTs.stl <- renderPlot({  plot_dTs.stl()  })
