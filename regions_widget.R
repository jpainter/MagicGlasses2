regions_widget_ui = function ( id ){
        ns <- NS(id)  
        
    tagList( 
      
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
            # , margins = c(70, 1200)
          ) ,

fillPage(       
    tabsetPanel( type = "tabs",
                 
            tabPanel( "Regions",  
                    

            h5( 'Filter Data to Selected Regions/Org Units') ,
 
            inputPanel(
            
             
              selectInput( ns("level2"), label = "OrgUnit Level2" , 
                            choices = NULL, 
                            selected = NULL ,
                            multiple = TRUE ) ,
              
              selectInput( ns("level3"), label = "OrgUnit Level3" ,
                            choices = NULL,
                            selected = NULL ,
                            multiple = TRUE ) ,
              
              selectInput( ns("level4"), label = "OrgUnit Level4" ,
                            choices = NULL,
                            selected = NULL  ,
                            multiple = TRUE  ) ,
              
              selectInput( ns("level5"), label = "OrgUnit Level5" ,
                            choices = NULL,
                            selected = NULL  ,
                            multiple = TRUE  ) 
              
              # actionButton( ns('select_regions') , label = "Update orgUnits ") 
      
        ) , 
        
        fluidRow(
                    column(
                      6, # Half-width column for the map
                      leafletOutput(ns("geoFeatures_map"), height = "60vh") # Adjust height dynamically
                    ),
                    column(
                      6, # Half-width column for the table
                      div(
                          style = "overflow-y: auto; height: 60vh;", # Adjust height to fit within the viewport
                          DTOutput(ns("geoFeaturesTable"))
                        )
                      )
                  )

)))
)
}
        
regions_widget_server <- function( id , 
                                     directory_widget_output = NULL ,
                                     metadata_widget_output = NULL,
                                     data_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {

    options( shiny.trace = FALSE )
    options( shiny.reactlog = FALSE )
    options( dplyr.summarise.inform = FALSE )
    
    # cat('\n** regions_widget\n -importing data from other modules')
    
    data.folder = reactive({ directory_widget_output$directory() })
    geofeatures.files = reactive({ directory_widget_output$geofeatures.files() })
  
    indicator = reactive({ data_widget_output$indicator() })
    formulas = reactive({ data_widget_output$formulas() })
    dataset.file = reactive({ data_widget_output$dataset.file() })
    # dataset = reactive({ data_widget_output$data1() })
    
    data1 = reactive({ data_widget_output$data1() })
    
    formula_elements = reactive({ data_widget_output$formula_elements() })
    
    orgUnits = reactive({ metadata_widget_output$orgUnits() })  
    orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
    geoFeatures = reactive({ metadata_widget_output$geoFeatures() })
    ousTree = reactive({ metadata_widget_output$ousTree() })
    
  
  # Selected org unit levels  
  # selected_regions = reactiveValues( 
  #   level2 = NULL , level3 = NULL , level4 = NULL , level5 = NULL )
  # 
  selected_regions = reactive({
    
    cat('\n * regions_widget selected_regions')
    
    # selected_regions$level2 = input$level2
    # selected_regions$level3 = input$level3
    # selected_regions$level4 = input$level4
    # selected_regions$level5 = input$level5
    
    # Testing
    sr = list( level2 = input$level2 ,
               level3 = input$level3 ,
               level4 = input$level4 ,
               level5 = input$level5 )
    # TESTING
    # saveRDS( sr , "selected_regions.rds")
    
    cat( "\n - selected_regions:" , unlist( sr ) )
    return( sr )
  })
  
  # Levels ####
  # level 2
  observeEvent( orgUnits()  , {  
    
            cat( '\n* regions_widget updating level2' , levels()[2] )
    
            l2 = orgUnits() %>% filter( level %in% levels()[2] ) %>%
              pull( name )
            
            # TESTING
            # saveRDS( levels(), 'levels.rds')
            # saveRDS( orgUnits(), 'orgUnits.rds')
            cat('\n - level2 name:', l2)
              
            updateSelectInput( session, 'level2' ,
                                choices = l2 ,
                                selected = NULL
                                )
    } )

  # level 3
  observe({
    req( input$level2 )
    cat( '\n* regions_widget updating level3')
      
    ls = orgUnits() %>% filter( parent %in% input$level2 ) %>% pull( name )
              
    cat( "\n - level3 update to:" , paste( ls , collapse = "" ) )
              
    updateSelectInput( session, 'level3' ,choices = ls, selected = NULL )
    } )

  # level 4
  observe({  
    req( input$level3 )
    cat( '\n* regions_widget updating level4')
      
    ls = orgUnits() %>% filter( parent %in% input$level3 ) %>% pull( name )
              
    # cat( "\n - level4 update to:" , paste( ls , collapse = "" ) )
              
    updateSelectInput( session, 'level4' ,choices = ls, selected = NULL)
    } )

# level 5  
   observe({
    req( input$level4 )
    cat( '\n* regions_widget updating level5')
      
    ls = orgUnits() %>% filter( parent %in% input$level4 ) %>%
              pull( name )
              
    # cat( "\n - level3 update to:" , paste( ls , collapse = "" ) )
              
    updateSelectInput( session, 'level5' ,choices = ls, selected = NULL )
  })
  
  
  levelNames = reactive({ 
    req( orgUnits() )
    cat( '\n* regions_widget levelNames():' )
    
    l = getLevelNames( orgUnits() )
    
    cat( '\n - ' , l )
    return(l)
})

  levels = reactive({ 
    req( orgUnitLevels() )
    cat( '\n* regions_widget levels():' )
    levels = orgUnitLevels() %>% pull( level )
    return( levels )
})
 
  
# Recursive function with cycle detection
getDescendantsRecursive <- function( node, df, visited = character() ) {
  # If the node is already visited, return an empty vector to prevent cycles.
  if( any( node %in% visited ) ) return( character(0) )
  
  # Mark the node as visited.
  visited <- c( visited, node )
  
  # Find direct children of the node.
  lower_parent <- stri_trim_both( stri_trans_tolower(df$parentName) )
  lower_node <- stri_trim_both( stri_trans_tolower(node) )

  children <-  df$name[ lower_parent %in%  lower_node ] %>% na.omit()
 
  # If no children, return an empty vector.
  if(length(children) == 0) return(character(0))
  
  # Recursively find descendants of each child, passing along the visited nodes.
  allDescendants <- children
  for(child in children) {
    allDescendants <- c(allDescendants, getDescendantsRecursive(child, df, visited))
  }
  return(allDescendants)
}

# OrgUnit Table ####

    geoFeatures.ous = reactive( {
      req( geoFeatures() )
      req( ousTree() )
      
      cat( '\n * geoFeatures.ous()' )
      
      gf.ous = geoFeatures() %>% 
        semi_join( ousTree() , by = c('id' = 'orgUnit') )
      
      if (!is.null( input$level2 ) & is.null( input$level3 ) ){
        
        cat("\n - input$level2: ", input$level2 )
        
        # TESTING
        # saveRDS( gf.ous, "gf.ous.rds" )
    
        # Get all descendant nodes for the selected node
        descendants <- getDescendantsRecursive( input$level2, gf.ous)
        # Include the selected node itself
        nodesToKeep <- c(input$level2, descendants)
        # cat("\n - nodesToKeep: ", nodesToKeep )
    
        gf.ous = gf.ous %>% filter( name %in% nodesToKeep )
      }
      
      if (!is.null( input$level3 ) & is.null( input$level4 )  ){
        # Get all descendant nodes for the selected node
        descendants <- getDescendantsRecursive( input$level3 , gf.ous )
        # Include the selected node itself
        nodesToKeep <- c( input$level3, descendants)
    
        gf.ous = gf.ous %>% filter( name %in% nodesToKeep )
      }
      
      if (!is.null( input$level4 ) & is.null( input$level5 )  ){
        
         # Get all descendant nodes for the selected node
        descendants <- getDescendantsRecursive( input$level4 , gf.ous )
        # Include the selected node itself
        nodesToKeep <- c( input$level4, descendants)
        
        gf.ous = gf.ous %>% filter( name %in% nodesToKeep )
      }
      
      if ( !is.null( input$level5 ) ) {
      
       # Get all descendant nodes for the selected node
        descendants <- getDescendantsRecursive( input$level5 , gf.ous )
        # Include the selected node itself
        nodesToKeep <- c( input$level5, descendants)
        
        gf.ous = gf.ous %>% filter( name %in% nodesToKeep )
      }
      
      # Remove slashes from levelNames
      cat( '\n - remove slashes from levelName')
      gf.ous$levelName = str_replace_all( gf.ous$levelName , fixed("/") , ";")
      
      return( gf.ous )
    })
  
    output$geoFeaturesTable = DT::renderDT( 
      DT::datatable(

        geoFeatures.ous() %>% 
          st_drop_geometry() %>%
          as_tibble() %>% 
          select( name, level, levelName, parentName, id, leaf, latitude, longitude ) ,
    
          rownames = FALSE,
          filter = 'top' ,
          options = list( DToptions_no_buttons(), scrollX = TRUE ) ,
          selection = "multiple" 
    ))
  
# Map ####

  gf.map = reactive({
    req( geoFeatures.ous() )

    # req( orgUnitLevels() )
    cat( '\n * gf.map():')

    
    gf = geoFeatures.ous()
    # gf. = shared_geofeatures
    
    
    cat( '\n - split geofeatures')
    split_geofeatures = split( gf , f = gf[['levelName']]  )

        
    # levels = names( split_geofeatures )
    levels = bind_rows(gf %>% st_drop_geometry()) %>% filter( !is.na( level)) %>% distinct( level, levelName ) 

    cat( '\n geoFeatures map for:' , levels$levelName  , '\n')
    
    # reorder levels
    split_geofeatures = split_geofeatures[ levels$levelName ]

    # test for empty geometry
    not_all_empty_geo = map_lgl( split_geofeatures , ~!all(is.na(st_dimension(.x))) )
    # print( paste( 'not_all_empty_geo: ', not_all_empty_geo ) )

    n_levels = sum( not_all_empty_geo ) # length(split_geofeatures) # 

    cat( paste('geoFeatures split into' , n_levels , 'levels' ,
                 paste( names( split_geofeatures ), collapse = ',' ), sep = " " ) , '\n')

    level.colors = RColorBrewer::brewer.pal(n_levels, 'Set2')
    names( level.colors ) = levels[ not_all_empty_geo, 'levelName' ]


    split_gf = split_geofeatures[ not_all_empty_geo ]
    
    admins = gf %>% filter( st_geometry_type(.) != 'POINT') %>% filter( !st_is_empty(.) )
    admin.levels = admins$levelName %>% unique 

    gf.map =
      leaflet( options = leafletOptions( preferCanvas = TRUE ,  updateWhen = FALSE ) ) %>%
      addProviderTiles( "Esri.WorldStreetMap", group = "Esri World Street Map") %>%
      addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
      addProviderTiles( "OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles( providers$Stadia.StamenToner, group = "Toner" ) %>%
      addTiles( group = "No Background" , options = providerTileOptions( opacity = 0 ) ) %>%

      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters"
      )  %>%

      # Layers control
      addLayersControl(
        baseGroups = c("Esri World Street Map", "Esri.WorldImagery" ,
                       "OpenStreetMap" , "Toner" ),
        overlayGroups = c( "Facility" , rev( admin.levels )   ),
        options = layersControlOptions( collapsed = FALSE )
      )
 
    for ( i in rev( seq_along( admin.levels ) ) ){
      gf.map = gf.map %>%
              addPolygons( data = admins %>% filter( levelName == admin.levels[ i ] ) ,
                     group = admin.levels[ i ] ,
                     label = ~paste( name ,   
                                     ifelse( level < 3 , '' , 
                                             paste( 'in' ,  parentName ) )
                                            ) ,
                     layerId = ~name,
                     color = "black", 
                     weight = 1, smoothFactor = 0.5,
                     opacity = 1.0, fillOpacity = 0 , fillColor = "lightblue" ,
                     highlightOptions = highlightOptions( color = "white", weight = 2,
                                                         bringToFront = TRUE) ,
                     labelOptions = labelOptions(
                       noHide = FALSE,
                       direction = "auto",
                       opacity = 1
                     )
                     ) 
    }
    
    gf.map = gf.map %>%
      addCircleMarkers( data = gf %>%
                          filter( st_geometry_type(.) == 'POINT') , group = "Facility" ,
                        radius = 3 ,
                        color = "blue" ,
                        stroke = FALSE, fillOpacity = .9,
                        label = ~name,
                        layerId = ~name,
                        labelOptions = labelOptions(
                          noHide = FALSE,
                          direction = "auto",
                          opacity = 1
                          )
      ) 
    
  #   options = popupOptions(closeButton = FALSE)
    
    return( gf.map )

  })
  
  output$geoFeatures_map <- renderLeaflet({
    gf.map()
    }) 

# Crossreactivity between map and table ####
    
  # Observe map marker click and highlight corresponding row in table
  observeEvent(input$geoFeatures_map_shape_click, {
    click <- input$geoFeatures_map_shape_click
    
    if (!is.null(click)) {

      selected_name <- click$id
      cat( paste('\n - clicked on:', selected_name ) )
      proxy <- dataTableProxy("geoFeaturesTable")
      
      # Apply the selected name to the filter box
      updateSearch(proxy, keywords = list(global = NULL, 
                                          columns = list( selected_name, NULL, NULL, NULL , NULL, NULL  ) ) )
  
    } else {
      updateSearch(proxy, keywords = list(global = NULL, 
                                          columns = list(NULL, NULL, NULL, NULL, NULL , NULL  ) ) )
    }
  })
  
  observeEvent(input$geoFeatures_map_marker_click, {
    click <- input$geoFeatures_map_marker_click
    
    if (!is.null(click)) {

      selected_name <- click$id
      cat( paste('\n - clicked on:', selected_name ) )
      proxy <- dataTableProxy("geoFeaturesTable")
      
      # Apply the selected name to the filter box
      updateSearch(proxy, keywords = list(global = NULL, 
                                          columns = list( selected_name, NULL, NULL, NULL , NULL, NULL  ) ) )
  
    } else {
      updateSearch(proxy, keywords = list(global = NULL, 
                                          columns = list(NULL, NULL, NULL, NULL, NULL , NULL  ) ) )
    }
  })
  
    
  # Create a reactive value to monitor table selection
  selected_row <- reactive({
      input$geoFeaturesTable_rows_selected
    }) 
  
  # Observe table row selection and highlight corresponding marker on map  
  observe({
    
    # revert to all points when input deselected and clear highlighted polygon
    if ( is.null( selected_row() ) ){
      cat('\n - no row selected')
      
      leafletProxy( "geoFeatures_map" ) %>%
        clearGroup( "highlighted_points" ) %>%
        clearGroup( "highlighted_polygons" )
      
      return()
    }
    
    gf = geoFeatures.ous()
    
    selected_row <- selected_row()
    
    cat( paste('\n - selected geofeatures row:', selected_row ) )
    selected_locations <- gf[selected_row, ]
    
    selected_polygons = which(grepl( 'POLYGON', st_geometry_type( selected_locations )))
    selected_points = which(grepl( 'POINT', st_geometry_type( selected_locations )))
    
    
    # if selected row is a polygon
    if ( length( selected_polygons ) > 0 ) {
      cat("\n - polygon selected")
        
      leafletProxy("geoFeatures_map") %>%
          clearGroup( "highlighted_polygons" ) %>%
          addPolygons(
            data = selected_locations[ selected_polygons , ] ,
            color = "brown", weight = 3, fillOpacity = 0.7,
            group = "highlighted_polygons"
          ) 
    } else { leafletProxy("geoFeatures_map") %>%
          clearGroup( "highlighted_polygons" )  }
    
    # If row is selected and is a point
    if ( length( selected_points ) > 0 ) {

      leafletProxy("geoFeatures_map") %>%
         clearGroup( "highlighted_points" ) %>%
         addCircleMarkers(
          data = selected_locations[ selected_points , ] ,
          radius = 3,
          color = "red",
          layerId = ~name,
          fill = TRUE,
          fillOpacity = 1,
          label = ~name ,
          group = "highlighted_points"
        )
    } else { leafletProxy("geoFeatures_map") %>%
         clearGroup( "highlighted_points" ) }
  })

  
  # Return ####
  return( 
    list( 
         selected_regions = selected_regions 
    )
    )
} )
}


