

### Map {.no-padding }

ous.file = reactive({
  req( data.folder() )
  print( 'ous.file(): ')
  
  dir = data.folder() 
  
  if ( file.exists( paste0( data.folder() , 'newOUS.rds')  ) ){
     
    f = paste0( dir,'newOUS.rds'  )
    
    } else {
    
    print( 'looking for geofeatures file' )
    dir.files = list.files( dir )
    ous.files = dir.files[ grepl( 'geoFeatures' , dir.files,
                                              fixed = TRUE  )]
    
    print( "ous.files:" ) ; print( ous.files )
    if (  is_empty(ous.files) ){
      cat("WARNING: missing file geoFeatures.rds or newOUS.rds ")
    } 
  
    f = paste0( dir, most_recent_file( ous.files ) )
    }
    
    print( 'ous file:' ) ; print( f )
    
    if ( !any(file.exists( f ) )  ) return( NULL )
    
    return( f )
})

ous = reactive({  print( 'reading ous file' ) ; 
  req( ous.file() )
  if ( !any(file.exists( ous.file() ) )) return( NULL )
  # showNotification("reading geo features file")
  tic()
  o = readRDS( ous.file() )  
  print( 'finished reading ous file' ) ; toc() 
  print( ' OUS: ' ) ; # print( glimpse( o ))
  return( o )
  })

levelNames = reactive({ 
  req( ous() )
  print( 'levelNames():' )
  l = count( ous() %>% as_tibble, level, levelName ) %>% 
    arrange( level ) %>% pull(levelName ) 
  l = l[ !is.na(l) ]
  print( 'end levelNames():' )
  return(l)
})

levels = reactive({ 
  print( 'levels():' )
  levels = 
    count( ous() %>% as_tibble, level, levelName ) %>% 
    arrange( level ) 
  print( "levels()"); print( levels )
  return( levels )
  print( 'end levels():' )
})

observeEvent( levelNames(), 
              updateSelectInput( session, 'level' , 
                                 choices = c('leaf',
                                             levelNames() ) , 
                                 selected = 'leaf' )
              )

  verbatimTextOutput("selected_rows")

  # Print the rows of the data frame which match the x value
  output$selected_rows <- renderPrint({
    if ( is.null( selectedOUs() ) ) return()
    else {
      head( orgunit.reports() %>% 
              filter( orgUnit %in% selectedOUs() ), 10) #
    }
  })


checkboxInput( "show_map", label ='Display map (may take a minute...)', 
               value = FALSE ) 

plotOutput( 'map' , 
    click = "map1_click" ,
    dblclick = "map1_dblclick" ,
    hover = "map1_hover" ,
    brush = "map1_brush" )


hf = reactive({ ous() %>% filter( feature %in% "POINT"  ) })
admins = reactive({ ous() %>% filter( feature %in% c('MULTIPOLYGON', 'POLYGON') ) })
fs_selected = reactive({ ous() %>% filter( orgUnit %in% selectedOUs() ) })

# split_geofeatures = reactive({ split( ous() ,  ous$levelName ) })
# levels = names( split_geofeatures )
# not_all_empty_geo = map_lgl( split_geofeatures ,
#                                  ~!all(is.na(st_dimension(.x))) )
map1 = reactive({ 
  
  req( admins())
  req( hf())
  
  if ( input$show_map ){
    ggplot( ) + 
    geom_sf( data = isolate( admins() ) ) +
    geom_sf( data = isolate( hf() ) , alpha = .25 ) +
    geom_sf( data = fs_selected() , color = 'brown' ) +
    theme_ipsum()
  } else { NULL }
  
})

output$map<-renderCachedPlot({ map1() } ,
                              cacheKeyExpr = { list( #input$country , 
                                                     input$show_map , 
                                                     fs_selected() )
                                } ) 


output$Map.info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str( input$map1_click ),
      "dblclick: ", xy_str(input$map1_dblclick),
      "hover: ", xy_str(input$map1_hover),
      "brush: ", xy_range_str(input$map1_brush)
    )
  })

