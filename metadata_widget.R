
metadata_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fillCol( height = 600, flex = c(NA ) ,
           
  tagList(
    
    tags$head(tags$style(".button{width: 50%;}" )) ,
    
    # add_busy_spinner(spin = "fading-circle",
    #                position = "top-right") ,


    tabsetPanel(type = "tabs",

            tabPanel( "systemInfo", 
                         
                          # Set up shinyjs
                          useShinyjs() , 

                         fluidRow(
                           column( 1 , 
                                   tags$br() ,
                                   actionButton( ns("getMetadataButton") , 
                                                     "Request Metadata" , style='margin-top:25px' 
                                                 ) ) ,
                          
                           column( 1 , 
                                   tags$br() , tags$br() ,
                                   downloadButton( ns( 'downloadInfo' ), 'Save metadata and system info' , 
                                                            style = "width: 100px") 
                          ) ,
                           column( 2 , 
                                   tags$br() , tags$br() ,
                                   tableOutput( ns('variables') ) ) ,
                          
                           column( 8, 
                                   tags$br() , tags$br() ,
                                   DTOutput( ns('systemInfo') ) ) 
                           
                           ) 
                          ) ,
                
                tabPanel("dataElements", 
                         
                         DTOutput( ns('dataElementDictionary') ) 
                         ) ,
                
                tabPanel("Categories", 
                         
                         DTOutput( ns('categories') ) 
                         ) ,

                tabPanel("dataSets",

                         DTOutput( ns( 'dataSets' )  ) 
                         # , style = "overflow-x: scroll;"

                         ) ,
                
                tabPanel("Indicators", 
                         
                         DTOutput( ns('indicators') ) 
                         ) ,
                
                tabPanel("Organizational Unit Levels",

                          DTOutput( ns( 'orgUnitLevels' )  ) 
                 ) ,

                tabPanel("Organizational Units",

                         DTOutput( ns( 'orgUnits' )  ) 
                         ) ,
                
                tabPanel("Organizational Unit Hierarchy",

                         DTOutput( ns( 'orgUnitHierarchy' )  ) 
                         ) ,

                tabPanel("Duplicates",

                         DTOutput( ns( 'OrgUnit_duplicates' )  ) 
                         ) ,

                tabPanel(
                  "Map",
                  fluidRow(
                    column(
                      6, # Half-width column for the map
                      leafletOutput(ns("geoFeatures_map"), height = "80vh") # Adjust height dynamically
                    ),
                    column(
                      6, # Half-width column for the table
                      div(
                        downloadButton(ns('downloadGeoFeatures'), 'Save geo features (.rds file)'),
                        br(), # Add some spacing between the button and the table
                        div(
                          style = "overflow-y: auto; height: 75vh;", # Adjust height to fit within the viewport
                          DTOutput(ns("geoFeaturesTable"))
                        )
                      )
                    )
                  )
                ) , 

                
                tabPanel("API Resources", 
                         
                         h4( "The table below lists a link to retrieve metadata (not the data) for each DHIS2 attribute.") ,
                         
                         h5( "Simply paste the link into a new brower window.  This is the mechanism used to retrieve all the information displayed in this app." ) ,
                         
                         h5( "Note that the user will need to login into the DHIS2 in order to see the metadata.") ,
                         
                         h5( "- The href column lists a url (e.g. https://play.dhis2.org/2.32/api/categories) that will returns a short list of information for each attribute.") ,

                         h5( "- Appending '?fields=:all&paging=false' (e.g. https://play.dhis2.org/2.32/api/categories?fields=:all&paging=false) to the url will provide all available information for that attribute. " ) ,
                         
                         tableOutput( ns('resource_table') ) 
                         
                         )
    )
    
    
  ) ) # end fillColl
          
          } # ui

# Server function ####
metadata_widget_server <- function( id , 
                                    login_widget_output = NULL , 
                                    directory_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              # loginInfo = loginDetails ,
              # dataDirectory = dataDir 
              ) {
      
  # cat('\n **** Starting metadata_widget_server \n')
      
  # reactives to toggle login status
  login = reactive({ login_widget_output$login() })
  baseurl = reactive({ login_widget_output$baseurl() })
  username = reactive({ login_widget_output$username() })
  password = reactive({ login_widget_output$password() })
  
  dir = reactive({ directory_widget_output$directory() })
  metadata.files = reactive({ directory_widget_output$metadata.files() })
  geofeatures.files = reactive({ directory_widget_output$geofeatures.files() })
  
  loginFetch = reactiveVal( FALSE )
  

# Request Metatadata ####
  observeEvent( input$getMetadataButton  , {
    
    cat('\n * metadata_widget getMetadataButton login()' , login(), '\n')
    
    if ( login()  ){
        cat('\n baseurl()' , baseurl(), '\n')
    
        loginFetch( TRUE )
        o = orgUnitLevels()
        p = orgUnits()
        q = ousTree()
        w = geoFeatures()
        x = dataElementDictionary()
        y = indicatorDictionary()
        z = systemInfo()
        zz = resources() 
        
        # also save as RDS
        meta = list( systemInfo = z ,
                     meta_variables = meta_variables() ,
                     orgUnitLevels = o ,
                     orgUnits = p ,
                     dataElementDictionary = x ,
                     indicatorDictionary = y ,
                     dataSets. = dataSets.() ,
                     categories = categories() ,
                     dataElementGroups =  dataElementGroups() ,
                     ousTree = q ,
                     geoFeatures = w 
        ) 
        
        saveRDS( meta , paste0( dir() , "metadata.RDS" ) )
        
        removeModal()

      } else {
 
       showModal(
                  modalDialog( title = "Please login before requesting metadata", 
                       easyClose = TRUE ,
                       size = 'm' ,
                       footer=NULL
                       )
       )
      loginFetch( FALSE ) 
      
      }
})
  
## system info  ####
  systemInfo = reactive({
    
    if ( login()  & loginFetch() ){
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      cat( '\n metadata widget requesting system.info.resources \n')
    
      url = paste0( baseurl() , "api/system/info" )
      getInfo = GET( url )
      getInfo.content =  content( getInfo , "text")
      
      info =   jsonlite::fromJSON( getInfo.content ) 
      
      info = info[ map_dbl( info , length ) == 1 ] %>% 
        as_tibble() %>% 
        gather( Attribute, Value )  
      
      cat( '\n metadata widget system.info completed \n')
      return( info )
    }
      

  }) 
  
  output$systemInfo =  DT::renderDT(DT::datatable(

      systemInfo() ,
      rownames = FALSE, 
      options = DToptions_no_buttons()
  ) )
 
## data Elements ####
  dataElements = reactive({
    cat('\n* metadata_widget dataElements():')
    if (  login()  & loginFetch() ){ 
    cat( '\n *reactive dataElements: \n')
      
      showModal(
        modalDialog( title = "Downloading list of data elements", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
 
      cols = c( 'id', 'name', 'shortName' , 'displayName', 'displayShortName' , 
                'zeroIsSignificant' , 'categoryCombo')
      
      url <- paste0( baseurl() ,"api/dataElements.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      x = get( source_url = url )[[1]] 
      
      cat( '\n -dataElement() colnames(x) : ' ,
           colnames(x) , '\n')
      
      dataElements =  x %>% select( !!cols )
      
      # remove list of associated category combos and add it back as a column
      de.categoryCombo = dataElements$categoryCombo
      dataElements = dataElements %>% select( -categoryCombo ) 
      dataElements$categoryCombo.id = de.categoryCombo$id 
  
      removeModal()
    } else {
            return()
        }
    
    cat( '\n -finished metadata_widget dataElements \n')
    return( dataElements )
     
  }) 
  
  dataElementGroups = reactive({

    
    if (  login()  & loginFetch() ){ 
    cat( '\n* metadata_widget dataElementGroups \n') 
      
      showModal(
        modalDialog( title = "Downloading list of data element groups", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )

      url<-paste0( baseurl() , "api/dataElementGroups.json?fields=:all&paging=false")
      
      cols = c( 'id', 'name' , 'dataElements' )
      
      dataElementGroups =  get( source_url = url )[[1]] %>% select( !!cols ) %>%
        rename( dataElementGroups.id = id , dataElementGroup = name )

      deg = map_df( 1:length( dataElementGroups$dataElementGroup) ,
                    ~merge( dataElementGroups[ .x, 1:2] ,
                            dataElementGroups$dataElements[[.x]] , all = T)
                    ) %>%
        rename( dataElement.id = id) 
      
      
       removeModal()
    } else {
    
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          deg = read_excel( file , sheet = 'DataElementGroups' )
          cat('\n- dataElementGroups has' , nrow(deg) , "rows" ) 
        } else {
            return()
        }
    }
    
    cat( '\n -finished metadata_widget data element groups\n')
    return( deg )
    
  })
  
  # data sets
  dataSets = reactive({
    
    if (  login()  & loginFetch() ){ 
    cat( '\n -reactive dataSets')
      
      showModal(
        modalDialog( title = "Downloading list of datasets", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
      cols = c( 'id', 'name' , 'periodType' , 
                'dataSetElements', # format of this causes problem (drc)
                'timelyDays' )
      
      url <- paste0( baseurl() ,"api/dataSets.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      x = get( source_url = url )[[1]]
      
      if( !all( cols %in% colnames(x) ) ) return( data.frame() ) 
            
      dataSets =  x %>% select( !!cols ) %>%
        rename( dataSet.id = id, 
                dataSet = name 
                , dataSetElements.id = dataSetElements
                )

      removeModal()

    } else {
    
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          dataSets = read_excel( file , sheet = 'DataSets' )
          cat('\n- DataSets has' , nrow(dataSets) , "rows" ) 
        } else {
            return()
        }
    }
    
      cat( '\n -finished metadata_widget datasets \n')
      return( dataSets )
    
  })
  
  # category combos
  categoryCombos = reactive({

    if (  login()  & loginFetch() ){ 
    cat( '\n -reactive categoryCombos')
      
      showModal(
        modalDialog( title = "Downloading list of category combos", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )   
    # url<-paste0( baseurl() , "api/categoryCombos.json?fields=:all&paging=false")

            cols = c( 'id', 'name', 'categoryOptionCombos'  )

      url <- paste0( baseurl() ,"api/categoryCombos.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      x = get( source_url = url )[[1]]
      
      if( !all( cols %in% colnames(x) ) ) return( data.frame() ) 
      
      categoryCombos =  x %>% select( !!cols ) 

       removeModal()
    } else { return() }
    
    cat( '\n -finished metadata_widget category combos \n')
    return( categoryCombos )
    
    
  })
  
  # category option combos
  categoryOptionCombos = reactive({
    
    
    if (  login()  & loginFetch() ){ 
    cat( '\n -reactive categoryOptionCombos')
      
      showModal(
        modalDialog( title = "Downloading list of category option combos", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )  
    # url<-paste0( baseurl() , "api/categoryOptionCombos.json?fields=:all&paging=false")
    
    cols = c( 'id', 'name' , 'categoryCombo' )
    
    url <- paste0( baseurl() ,"api/categoryOptionCombos.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
          
    x = get( source_url = url )[[1]]
      
    if( !all( cols %in% colnames(x) ) ) return( data.frame() ) 
      
    categoryOptionCombos =  x %>% select( !!cols ) 
    
    removeModal()
    } else { return() }
    
    cat( '\n -finished metadata_widget category option combos \n')
    return( categoryOptionCombos )
    
  })
  
  # Categories: full list of category option combos
  categories = reactive({
    
    if (  login()  & loginFetch() ){ 
    cat( '\n *reactive categories ') 

      cc = categoryCombos()
      coc = categoryOptionCombos()
      
      # Testing
      # saveRDS( cc,  "cc.rds")
      # saveRDS( coc,  "coc.rds")
      
      cat( "\n - collating categories  " )
      showModal(
        modalDialog( title = "Collating categories", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
      
      cat( "\n - cc.coc " )
      
      cc. = cc %>% select( id, name, categoryOptionCombos ) %>%  
        rename( categoryCombo.id = id
                , categoryCombo = name ) %>%
        unnest( categoryOptionCombos )  %>% rename( categoryOptionCombo.id = id )
      
      coc. = coc %>%  
          rename( categoryOptionCombo.id = id , categoryOptionCombo = name ) %>% 
          unnest( categoryCombo) %>% 
          rename( categoryCombo.id = id )
      
      cc.coc = coc.  %>% 
        left_join( cc %>% select( id, name ) %>%  
                     rename( categoryCombo.id = id , categoryCombo = name )  , 
                   by = c( "categoryCombo.id"  ) )
      
      # Modified with above to accomodate Benin having missing categoryCombos, thus caegoryOptionCombos were unlinked
      # cc.coc = cc %>% select( id, name, categoryOptionCombos ) %>%  
      #   rename( categoryCombo.id = id , categoryCombo = name ) %>%
      #   unnest( categoryOptionCombos ) %>% 
      #   right_join( coc , by = "id" ) %>%
      #   rename( categoryOptionCombo.id = id , categoryOptionCombo = name )
      
      # Testing
      # saveRDS( cc.coc,  "cc.coc.rds")
      
      cat( "\n - categories " )
      categories = cc.coc %>%
        group_by( categoryCombo.id, categoryCombo ) %>%
        summarise(
          n_categoryOptions = n() ,
          Categories = paste( categoryOptionCombo , collapse = ' ;\n '  ) ,
          categoryOptionCombo.ids = paste( categoryOptionCombo.id , collapse = ' ;\n '  )
        )
      
    } else {
    
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          categories = read_excel( file , sheet = 'Categories' )
          cat('\n- Categories has' , nrow(categories) , "rows" ) 
        } else {
            return()
        }
    }
    
    cat( '\n -finished metadata_widget categories \n')
    removeModal()
    return( categories )
  })
  
# dataElementDictionary and dsde
  dataElementDictionary = reactive({

    if (  login()  & loginFetch() ){ 
    cat( '\n *** creating dataElementDictionary \n' )

    de = dataElements()
    ds = dataSets()
    cats = categories()
    deg = dataElementGroups()
    
    cat( '\n -creating dsde..' )
    
    # testing
    # saveRDS( ds , 'ds.rds')
    # saveRDS( de , 'de.rds')
    # saveRDS( cats ,  'cats.rds' )
    # saveRDS( deg ,  'deg.rds' )
    
    
    showModal(
        modalDialog( title = "Compiling data element dictionary", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
    
      # DSDE : create matrix of data elements within each dataset
      if ( nrow(ds) > 0 && 'dataSetElements.id' %in% names( ds )){
        dsde = map_df( 1:length( ds$dataSet),
                       ~map_df( ds$dataSetElements.id[[.x]],
                                ~as.matrix(.x) )) %>%
          select( dataSet, dataElement ) %>%
          rename( dataElement.id = dataElement )
      } else if ( nrow(ds) > 0) {
        dsde = ds
      } else { 
        # empty data.frame for demo instances with missing ds
        dsde = data.frame( dataSet = NULL , dataElement = NULL )
      }
      
    cat( '\n - glimpse(dsde):\n') ; glimpse(dsde)
    
    # Base Dictionary Line List (with categories collapsed)
    cat( '\n -creating dictionary..' )
    #Testing
    # save( de, ds, dsde , deg, cats , file = 'dataElementDictionary.rda')
   
    dictionary = de  %>%  rename( dataElement.id = id ) %>%
      
      left_join( dsde , by = 'dataElement.id' ) %>%
      
      rename(  
              dataElement = name ,
              dataSet.id = dataSet  ) %>%
      
      left_join( ds  , by = 'dataSet.id' ) %>%
      
      left_join( deg  , by = 'dataElement.id' , 
                 relationship = "many-to-many") %>%
      
      left_join( cats  , by = 'categoryCombo.id' ) %>%
      
      # reorder; move ids to end
      select( dataElement , 
              Categories , dataElementGroup , dataSet , periodType , 
              zeroIsSignificant , shortName , displayShortName , displayName ,
              dataElement.id , categoryCombo.id , categoryOptionCombo.ids, dataSet.id , dataElementGroups.id ,
              n_categoryOptions, categoryCombo ) %>%
      
      mutate( dataElement = dataElement %>% str_trim() , 
              Categories = Categories %>% str_trim()
              ) %>%
      
      # collapse all muliptle entries for each data element
      group_by( dataElement.id , dataElement ) %>%
      
      summarise_all(
        
        list( ~paste( unique(.) , collapse = ';\n' ) )
      )

     removeModal()

    } else {
    
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          dictionary = read_excel( file , sheet = 'DataElements' )
          cat('\n- dataElements has' , nrow(dictionary) , "rows" ) 
        } else {
            return()
        }
    }
    
     cat( '\n -finished metadata_widget dataElementDictionary \n')
     return( dictionary )
  
      
  })
  
  # output tables 

  output$dataElementDictionary = 
    DT::renderDT(DT::datatable(
   
    dataElementDictionary()   ,
    
    rownames = FALSE, 
    filter = 'top' ,
    # options = DToptions_no_buttons()
    options = list(
        # bPaginate = FALSE, 
        autoWidth = TRUE ,
        scrollY = "60vh"  ,
        scrollX = TRUE ,
        scrollCollapse = TRUE ,
        paging = TRUE ,
        searching = TRUE , 
        info = TRUE ,
        lengthMenu = list( c(  10, 25, 100, -1 ) , 
                           list( '10', '25', '100', 'All' ) ) ,
        pageLength = 10 ,
        server = TRUE ,
        dom = 'tilrp' ) ,
      fillContainer = TRUE
  ))
  
  output$dataElementGroups = 
    DT::renderDT(DT::datatable(

    if ( !is.null( dataElementGroups() ) ) dataElementGroups()  ,

    rownames = FALSE,
    filter = 'top' ,
    options = DToptions_no_buttons()
  ))
    
  output$categories = 
    DT::renderDT(DT::datatable(

    if ( !is.null( categories() ) ) categories()  ,

    rownames = FALSE, 
    filter = 'top' ,
    # options = DToptions_no_buttons()
    options = list(
      # bPaginate = FALSE, 
      autoWidth = TRUE ,
      scrollY = "60vh"  ,
      scrollX = TRUE ,
      scrollCollapse = TRUE ,
      paging = TRUE ,
      searching = TRUE , 
      info = TRUE ,
      lengthMenu = list( c(  10, 25, 100, -1 ) , 
                         list( '10', '25', '100', 'All' ) ) ,
      pageLength = 10 ,
      server = TRUE ,
      dom = 'tilrp' ) ,
    fillContainer = TRUE
  ))
  
  # DataSets: remove dataSetElements...if it has it!
  dataSets. = reactive({
          if ( 'dataSetElements.id' %in% names( dataSets() ) ){
          dataSets() %>% select( - dataSetElements.id )
      }  else {
          dataSets()  
      }
  })
  
  output$dataSets = 
    DT::renderDT(DT::datatable(

    if ( !is.null( dataSets.() ) ){ 
      dataSets.()
      },

    rownames = FALSE,
    filter = 'top' ,
    options = DToptions_no_buttons()
  ))
  
  
  
## Indicators ####
  
  indicators = reactive({
    
    cat( '\n metadata widget: reactive indicators \n' )
    if (  login()  & loginFetch() ){ 
      
      showModal(
        modalDialog( title = "Downloading list of indicators", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
      
      cols = c( 'id', 'name', 'displayName', 
                # 'description' , # col not available in Guinea Feb 2022
                'numerator' , 'denominator' ,
                'annualized'
      )
            
      # if available, use resources method
      # url<-paste0( baseurl() ,"api/indicators.json?fields=:all&paging=false")
      url <- paste0( baseurl() ,"api/indicators.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      

      
      indicators =  get( source_url = url )$indicators  %>% select( !!cols ) 
    
      removeModal()
    } else { return() }
      
    cat( '\n -finished metadata_widget indicators \n')
    return( indicators )
    
  })
  
  # combine table of data elements and category option combos
  id_names = reactive({

    req( dataElements() )
    # req( categoryOptionCombos() )

    cat( '\n metadata_widget id_names():')
    de = dataElements()  %>% select( id, name )

    coc = categoryOptionCombos()  %>%  select( id, name )

    bind_rows( de , coc )

  })

  
  indicator_formula_translator = function( num_denom, id_names ){


    ids_between_braces = str_extract_all( num_denom , "\\{.*?\\}" )[[1]] %>% gsub("\\{|\\}", "", .)

    unique_ids = str_split( ids_between_braces , "\\.") %>% unlist %>% unique

    if( is.null( unique_ids ) ) return( num_denom )

    # lookup table
    element_names = id_names %>% filter( id %in% unique_ids )

    # when no match with data elem/coc ...
    if( nrow( element_names ) == 0 ) return( num_denom )


    # replace ids with names
    for( .x in 1:nrow( element_names ) ){
      if ( .x ==1 ) text = num_denom
      text = gsub( element_names[.x, 'id'] ,
                   paste0( '[' , element_names[.x, 'name'], ']' ) ,
                   text, fixed = TRUE  )
    }

    # trim braces and expand space around operators
    num_names = text %>%
      gsub( "\\{|\\}|\\#" , "", .) %>%
      gsub( "\\+" , " + " , . ) %>%
      gsub( "\\-" , " + " , . )

    return( num_names )
  }

  indicatorDictionary = reactive({
    
    if (login() & loginFetch() ){
    cat( '\n *collating indicators ')
    
    id_names = id_names()
    indicators = indicators()
    
    showModal(
        modalDialog( title = "Compiling indicator dictionary", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
    
    translated =

      indicators %>%

      # replace formula with id for formulat with labels
      rename( denominator.ids = denominator, numerator.ids = numerator ) %>%

      mutate(

        numerator =  map_chr( numerator.ids , ~indicator_formula_translator( .x , id_names ) ) ,

        denominator = map_chr( denominator.ids , ~indicator_formula_translator( .x , id_names ) )

          ) 
    # %>%
    # 
    #   select( name, 
    #           # description,  # col not available in Guinea Fev 2022
    #           numerator, denominator, annualized,
    #           id, displayName, numerator.ids , denominator.ids )
    
      removeModal()
    } else {
    
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          translated = read_excel( file , sheet = 'Indicators' )
          cat('\n- Indicators has' , nrow(translated) , "rows" ) 
        } else {
            return()
        }
    }
    
    cat ('\n -end indicatorDictionary \n' )
    return( translated )
     
    })
  
  output$indicators = 
    DT::renderDT(DT::datatable(

    indicatorDictionary()   ,
    
    rownames = FALSE, 
    filter = 'top' ,
    # options = DToptions_no_buttons()
    options = list(
      # bPaginate = FALSE, 
      autoWidth = TRUE ,
      scrollY = "60vh"  ,
      scrollX = TRUE ,
      scrollCollapse = TRUE ,
      paging = TRUE ,
      searching = TRUE , 
      info = TRUE ,
      lengthMenu = list( c(  10, 25, 100, -1 ) , 
                         list( '10', '25', '100', 'All' ) ) ,
      pageLength = 10 ,
      server = TRUE ,
      dom = 'tilrp' ) ,
    fillContainer = TRUE
    
  ))
  
## orgUnitLevels ####
  
  orgUnitLevels = reactive({
    # req( metadata.files() )
    
    cat('\n* metatdata_widget orgUnitLevels():')
    if (  login() & loginFetch() ){
      
      cat( '\n - reactive orgUnitLevels_with_counts')
      
      showModal(
        modalDialog( title = "Downloading list of orgUnit levels", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
     
      cols = c( 'level' , 'name', 'created' , 'lastUpdated' , 'displayName' , 'id' )
      
      
      url <- paste0( baseurl() ,"api/organisationUnitLevels.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
      
      cat('\n - orgUnitLevels URL'); print(url)
      
      #Testing
      # saveRDS( get( url )[[1]] , "getOusLevels.rds")
      # saveRDS( cols , "cols.rds")
      
      ousLevels =  get( source_url = url )[[1]]  %>% 
        select( !!cols ) %>% 
        arrange( level ) %>%
        rename( levelName = name ) 
      
      removeModal()
      
    } else { 
      
        file = paste0( dir(), metadata.files()[1] )
        cat('\n- looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){ 
          
          cat('\n- orgUnitLevels reading from'  )
          ousLevels = read_excel( file  , 
                                  sheet = 'OrgUnitLevels' )
          cat('\n- orgUnitLevels has' , nrow(ousLevels) , "rows\n" )
        } else { ousLevels = NULL } 
    }
   
    cat('\n- end orgUnitLevels():')
    return( ousLevels)
  }) 
  
  orgUnitLevels_with_counts = reactive({ 
    req( orgUnitLevels() )
    req(  n_orgUnits_level() )
    
    # if ( login() & loginFetch() ){
    cat( '\n* reactive orgUnitLevels_with_counts' )
    
    if ( 'Number_Units' %in% names( orgUnitLevels() ) ){
      ous_w_counts = 
        orgUnitLevels() %>%
        select( level, levelName , Number_Units , lastUpdated , created , displayName, id )
    } else { 
      ous_w_counts = inner_join( 
          orgUnitLevels() , 
          n_orgUnits_level()  , by = 'level' ) %>%
          rename( Number_Units = n ) %>%
          select( level, levelName , Number_Units , lastUpdated , created , displayName, id )
    } 
    return( ous_w_counts )
    })
  
  output$orgUnitLevels = DT::renderDT(DT::datatable(
    
    orgUnitLevels_with_counts()  , 
    
    class = 'white-space: nowrap',
    rownames = FALSE ,
    options = DToptions_no_buttons()
  ))
  

## OrgUnits ####
  orgUnits = reactive({
    cat('\n* metatdata_widget orgUnits():')
    
    if (  login() & loginFetch() ){
      cat( '\n* reactive orgUnits \n')

      showModal(
        modalDialog( title = "Downloading list of organisation units", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
 
      # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
      # if available, use resources method
      
      cols = c( 'level' , 'name', 'id', 'shortName' , 'displayName', 'displayShortName', 
                "leaf" , 
                'created' , 'openingDate' , 'lastUpdated' , 
                'closedDate' ,
                # 'path',  
                "parent" , 
                # 'dataSets' , 
                'code' )
      
      # print( paste( 'cols:' , cols ) )

      url <- paste0( baseurl() ,"api/organisationUnits.json?fields=" ,
                     paste( cols, collapse = ",") , 
                     "&paging=false")
      
      ous =  get( source_url = url )[[1]] 
      
      cat( '\n*joining ous with itself to get parent names')

      ouLevels = orgUnitLevels()
      
      
      ous. = ous %>% 
        # select( !!cols ) %>% # closedDate missing for guinea--results in error.  already in url, so why select here? 
        left_join( ouLevels %>% 
                     select( level, levelName ) , by = 'level' 
                   ) %>%
        select( level, levelName , everything() ) %>% 
        mutate( parent.id = ous$parent$id ) %>% 
        select(-parent) %>%
        # Parent names 
        left_join( ous %>% select( id, name ) %>% 
                     rename( parent.id = id, parent = name ) , 
                   by = 'parent.id' ) %>%
        arrange( level, parent , name ) 
      
      removeModal()
      # print( paste( 'col names:' , names( ous ) ) )
      # test:
      # saveRDS( ous , 'orgUnits.rds' )
    } else { 
      
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- orgUnits reading from'  )
          ous. = read_excel( file , 
                                  sheet = 'OrgUnits' )
          cat('\n- orgUnits has' , nrow(ous.) , "rows" ) 
        } else {
            return()
        }
    }
    
    # glimpse( ous )
    cat( '\n- finished reactive orgUnits \n')
    return( ous.)
  })
  
  orgUnitDuplicates = reactive({
    
    req( orgUnits() )
    cat( 'reactive orgUnitDuplcates \n')
    
    duplicates = orgUnits() %>%
      group_by( name ) %>%
      summarise( n = n() ) %>%
      filter( n > 1 )
    
    orgUnitDuplicates = inner_join( orgUnits() ,
                                     duplicates ,
                                     by = 'name' )
    
    cat( 'finished reactive orgUnitDuplcates \n')
    return( orgUnitDuplicates )
    
  })
  
  n_orgUnits_level = reactive({ 
    req( orgUnits() )
    cat( '\n* reactive n_orgUnits_level ')
    orgUnits() %>% count( level ) 
    })
    
  n_orgUnits = reactive({
    req( orgUnits() )
    cat( '\n* reactive n_orgUnits')
    ou.rows = nrow( orgUnits() )
    paste( ou.rows , 'organisation units' )
    return( ou.rows )
  })
  
  output$orgUnits = DT::renderDT(DT::datatable(
  
    # do not include dataSets (if downloaded )
    if ( 'dataSets' %in% names(orgUnits() ) ){
      orgUnits() %>% select( - dataSets )  
    }  else {
      orgUnits() 
      } , 
    
    rownames = FALSE, 
    filter = 'top' ,
    options = DToptions_no_buttons()
    ))

  
  output$OrgUnit_duplicates = DT::renderDT(

    orgUnitDuplicates()   , 
    
    rownames = FALSE, 
    extensions = 'Buttons' , 
    options = DToptions_with_buttons( 
      file_name = paste( 'OrgUnit_duplicates_' , Sys.Date() ) 
      )
    )
  
  
## ousTree ####
  ousTree = reactive({
        req( orgUnitLevels() ) 
        req( orgUnits() )
        
    if (  login()  & loginFetch() ){ 
    cat( '\n *** creating ousTree after download \n' )

    ous = orgUnits()
    ouLevels = orgUnitLevels()
    
    showModal(
        modalDialog( title = "Compiling org unit tree", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
    
    #TESTING
    # saveRDS( ous , 'ous.rds');  
    # saveRDS( ouLevels, 'ouLevels.rds')
    
    ous.tree = ous_tree( ous , ouLevels )
    
    removeModal()

    } else {
    
        file = paste0( dir(), metadata.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          ous.tree = read_excel( file , sheet = 'orgUnitHierarchy', guess_max = 1e6 )
          
          # if leaf not in ous.tree, add it
          if ( ! 'leaf' %in% names( ous.tree ) ){
            cat('\n- adding leaf to ous.tree'  )
            # cat('\n- orgUnits() names:'  , names(orgUnits()  ))
            # cat('\n- ous.tree names:'  , names( ous.tree  ))
            ous.tree = ous.tree %>% left_join( orgUnits()  %>% select( id, leaf ) , by = c('orgUnit' = 'id') )
          }
          
          cat('\n- ous.tree has' , nrow(ous.tree) , "rows" ) 
        
          } else {
          cat( '\n *** creating ousTree \n' )
            
          ous = orgUnits()
          ouLevels = orgUnitLevels()

          showModal(
              modalDialog( title = "Compiling org unit tree",
                           easyClose = TRUE ,
                           size = 'm' ,
                           footer=NULL
                           )
              )

          ous.tree = ous_tree( ous , ouLevels )

          removeModal()
        }
    }
    
     cat( '\n - finished metadata_widget ous.tree \n')
     
     # testing
     # saveRDS( ous.tree , 'ousTree.rds' )
     
     return( ous.tree )
  
      
  })
 
  output$orgUnitHierarchy = DT::renderDT(DT::datatable(
  
        ousTree() , 
    
    rownames = FALSE, 
    filter = 'top' ,
    options = DToptions_no_buttons()
    ))
  
## geoFeatures ####
  ## for description of properties, see table 1.59, 
  ## https://docs.dhis2.org/2.22/en/developer/html/ch01s32.html
  
  geoFeatures_download = function( level = 2 , .pb = NULL ){

    cat( "downloading geoFeatures level", level , "\n")

    if ( !is.null( .pb ) ) update_progress(.pb)

    url<-paste0( baseurl() , "api/organisationUnits.geojson?level=", level ,
                 "&fields=:all&paging=false")

    cat( '\n geoFeatures request:' ,  url )

    geo = content( GET(url) , "text")  # indirect?

    # print( 'geo glimpse') ;  print( glimpse( geo ))

    # test
    print( 'converting geojson to sf...')
    
    if ( ! jsonlite::validate( geo )[[1]]  ){
      cat('\n - geojson not formatted correctly')
      print( jsonlite::validate( geo ) )
      return( data.frame() )
    } 
    
    geojsonsf = try( geojsonsf::geojson_sf( geo ) ) 
    if ( "try-error" %in% class( geojsonsf ) ){
      cat('\n - geojson not formatted correctly')
      print(geojsonsf  )
      return( data.frame() )
    }
    
    geojsonsf$id = fromJSON( geo )$features$id

    # geojsonsf = geojsonsf::geojson_sf( geo ) # returns group instead of id???

    # test
    # glimpse( geojsonsf )

    return( geojsonsf )
  }

  geoFeatures = reactive({
    
    req( orgUnits() , orgUnitLevels() )
    cat('\n * geoFeatures():'  )
    
    # if (  login() & loginFetch() ){
    #   
    #   cat( '\n - geoFeatures...')
    # 
    #   showModal(
    #     modalDialog( title = "Downloading list of geoFeatures", 
    #                  easyClose = TRUE ,
    #                  size = 'm' ,
    #                  footer=NULL
    #                  )
    #     )
    # 
    #   levels = orgUnitLevels()$level %>% unique 
    #   cat( '\n - geoFeatures has levels:' , length( levels ) , '\n')
    # 
    #   geosf = list()
    #   
    #     # login_status = try( loginDHIS2( baseurl() , username(), password() ) )
    #     # print( paste( 'try loginDHIS2 is' , login_status , 
    #     #               baseurl()  
    #     #               # , username(), password()  
    #     #               ))
    #     
    #   # pb = progress_estimated( length( levels ) )
    #   
    #   for ( l in levels ){
    #     
    #     cat( '\n -geoFeatures download level-' , l , '\n' )
    #     
    #     # xx= data.frame()
    #     xx =  geoFeatures_download( level = l  )
    #     # glimpse( x )
    #     if ( "sf" %in% class(xx) ){  
    #       cat('\n - this level is SF \n')
    #       geosf[[ l ]] = xx  
    #     } else { next }
    #   }
    #   
    #   # print( 'geosf[[ l ]]') ; print(  glimpse( geosf[[ l ]] ) )
    #   
    # 
    #   # saveRDS( geosf, 'pre_bind_geosf.rds')
    #   
    #   # before binding, find common col
    #   geo_nonzero_rows = map_dbl( geosf, ~ifelse( !is_empty(.x) , nrow(.x), 0  )) > 0
    #   geo_names_in_common = map(  geosf[geo_nonzero_rows], names ) %>% Reduce(intersect, .)
    #   geosf. = lapply( geosf[geo_nonzero_rows] , "[", geo_names_in_common ) 
    #   
    #   geosf. <- do.call( rbind , geosf.)
    # 
    #   cat( '\n - names geosf: ' ,  names( geosf. ) )
    #     # 
    #   # ous = ous %>% select( id, geometry ) 
    #   # ous = orgUnits()
    #   cat( '\n - geoFeatures:' , nrow( geosf. ) , 'rows \n' )
    #   
    #   cat( '\n - join ous with orgUnits()')
    # 
    #   geosf. = geosf. %>%
    #     right_join( orgUnits() %>%
    #                  # filter( ! is.na( code ) ) %>%
    #                  select( id, name, levelName, leaf, parent ) %>%
    #                   rename( parentName = parent ),
    #                by = c( 'id' ,  'name' )
    #     )
    #   
    #   cat( "\n - rows with ous linked to orgUnits" , nrow( geosf. ) , '\n')
    #   
    #   # test
    #   # saveRDS( geosf. , 'geosf.rds')
    #   
    #   filename = paste0( dir() , "geoFeatures_", Sys.Date()  , ".rds"  )
    #   saveRDS( geosf. , filename )
    #   
    #   # TODO: impute location of missing facilities/admin areas 
    #   
    #   # glimpse( ous )
    #   cat( '\n - missing geometry for' , sum( is.na( geosf.$geometry )), '\n' )
    #   
    #   # test
    #   
    #   # saveRDS( ous , 'geometry.rds')
    #   
    #   removeModal()
    # 
    # } else {
    
        file = paste0( dir(), geofeatures.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          geosf. = readRDS( file )
          cat('\n- geofeatures have' , nrow(geosf.) , "rows" ) 
        } else {
            return()
        }
    # }
    
    # Fill latitude and longitude only if geometry is POINT
    cat( '\n add lat and long when geometry is point')
    geosf.$geom_type <- st_geometry_type( geosf. )
    is_point <- geosf.$geom_type == "POINT"
    geosf.$latitude[is_point] <- st_coordinates(geosf.[is_point, ])[, "Y"]
    geosf.$longitude[is_point] <- st_coordinates(geosf.[is_point, ])[, "X"]
    
    return( geosf. )
    
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
    selection = "single", # Allow single row selection

    ))
  

## Map ####
  
  geoFeatures.ous = reactive( {
    req( geoFeatures() )
    req( ousTree() )
    
    cat( '\n * geoFeatures.ous()' )
    
    gf.ous = geoFeatures() %>% 
      semi_join( ousTree() , by = c('id' = 'orgUnit') )
    
    # Remove slashes from levelNames
    cat( '\n - remove slashes from levelName')
    gf.ous$levelName = str_replace_all( gf.ous$levelName , fixed("/") , ";")
    
    return( gf.ous )
  })
  
  # * CROSSTALK
  # shared_geofeatures <- SharedData$new(  geoFeatures.ous )
  
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
    
    gf.map =
      leaflet( options = leafletOptions( preferCanvas = TRUE ,  updateWhen = FALSE ) ) %>%
      addTiles(group = "OSM (default)") %>%
      
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters"
      ) %>%
      
      addProviderTiles(providers$Stadia.StamenToner, group = "Toner") %>%
      # addProviderTiles( providers$Stamen.TonerLite , group = "Toner Lite") %>%
      # addProviderTiles( "Stadia.Stamen.Terrain" , group = "Stamen.Terrain" ) %>%
      addProviderTiles( "Esri.WorldStreetMap", group = "Esri.WorldStreetMap" )  %>%
      addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
      addTiles( group = "No Background" , options = providerTileOptions( opacity = 0 ) ) 

    admin.levels = admins$levelName %>% unique 
    
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
                       opacity = 0.5
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
      ) %>%
    
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Stamen.Terrain", 
                       "Esri.WorldStreetMap" , "Esri.WorldImagery" ),
        overlayGroups = c( "Facility" , rev( admin.levels )   ),
        options = layersControlOptions( collapsed = FALSE )
      )
    
  #   options = popupOptions(closeButton = FALSE)
    
    return( gf.map )

  })
  
  output$geoFeatures_map <- renderLeaflet({
    gf.map()
    }) 
  
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
  
    
# Observe table row selection and highlight corresponding marker on map
    # Create a reactive value to monitor selection
    selected_row <- reactive({
      input$geoFeaturesTable_rows_selected
    }) 
    
  observe({
    
    gf = geoFeatures.ous()
    
    # revert to all points when input deselected
    if ( is.null( selected_row() ) ){
      cat('\n - no row selected')
      
      leafletProxy("geoFeatures_map") %>%
        clearMarkers() %>%
        addCircleMarkers( data = gf %>%
                            filter( st_geometry_type(.) == 'POINT') , group = "Facility" ,
                          radius = 3 ,
                          color = "blue" ,
                          layerId = ~name,
                          stroke = FALSE, fillOpacity = .9,
                          label = ~name
        )
      return()
    }
    
    selected_row <- selected_row()
    
    cat( paste('\n - selected geofeatures row:', selected_row ) )
    
    selected_location <- gf[selected_row, ]
    
    # If row is selected and is a point
    if ( length(selected_row) > 0 && st_geometry_type( selected_location ) %in% "POINT" ) {

      leafletProxy("geoFeatures_map") %>%
        clearMarkers() %>%
         addCircleMarkers( data = gf %>%
                            filter( st_geometry_type(.) == 'POINT') , group = "Facility" ,
          radius = 3 ,
          color = "blue" ,
          layerId = ~name,
          stroke = FALSE, fillOpacity = .9,
          label = ~name
      ) %>%
        addCircleMarkers(
          data = selected_location ,
          radius = 3,
          color = "red",
          layerId = ~name,
          fill = TRUE,
          fillOpacity = 1,
          label = ~name
        )
    } else {
      
      leafletProxy("geoFeatures_map") %>%
        clearMarkers() %>%
        addCircleMarkers( data = gf %>%
                            filter( st_geometry_type(.) == 'POINT') , group = "Facility" ,
                          radius = 3 ,
                          color = "blue" ,
                          layerId = ~name,
                          stroke = FALSE, fillOpacity = .9,
                          label = ~name
        )
    }
  })

# Save geoFeatures to an rds file ####

  output$downloadGeoFeatures <- downloadHandler(

    filename = function() {
      paste0( dir() , "geoFeatures_", Sys.Date()  , ".rds"  )
    } ,

    content = function( file ) {

      saveRDS( geoFeatures() , file )
     }
  )
  

# Download  meta data to excel file ####
  output$downloadInfo <- downloadHandler(

    filename = function() {
      paste0( "MetaData_", Sys.Date()  ,".xlsx"  )
    } ,
    
    content = function( file ) {

      wb <- openxlsx::createWorkbook()

      sheet1  <- addWorksheet( wb, sheetName = "System")
      sheet2  <- addWorksheet( wb, sheetName = "MetadataSizes")
      sheet3  <- addWorksheet( wb, sheetName = "OrgUnitLevels")
      sheet4  <- addWorksheet( wb, sheetName = "OrgUnits")
      sheet5  <- addWorksheet( wb, sheetName = "DataElements")
      sheet6  <- addWorksheet( wb, sheetName = "Indicators")
      sheet7  <- addWorksheet( wb, sheetName = "DataSets")
      sheet8  <- addWorksheet( wb, sheetName = "Categories")
      sheet9  <- addWorksheet( wb, sheetName = "DataElementGroups")
      sheet10 <- addWorksheet( wb, sheetName = "orgUnitHierarchy")

      writeDataTable( wb, sheet1, systemInfo() , rowNames = FALSE )
      writeDataTable( wb, sheet2, meta_variables() , rowNames = FALSE )
      writeDataTable( wb, sheet3, orgUnitLevels() , rowNames = FALSE  )
      writeDataTable( wb, sheet4, orgUnits() , rowNames = FALSE )
      writeDataTable( wb, sheet5, dataElementDictionary() , rowNames = FALSE )
      writeDataTable( wb, sheet6, indicatorDictionary() , rowNames = FALSE )
      writeDataTable( wb, sheet7, dataSets.()  , rowNames = FALSE ) # %>% select( - dataSetElements )
      writeDataTable( wb, sheet8, categories() , rowNames = FALSE )
      writeDataTable( wb, sheet9, dataElementGroups() , rowNames = FALSE )
      writeDataTable( wb, sheet10, ousTree() , rowNames = FALSE )

      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )
      
      # also save as RDS
      meta = list( systemInfo = systemInfo ,
                   meta_variables =meta_variables() ,
                   orgUnitLevels = orgUnitLevels() ,
                   orgUnits = orgUnits() ,
                   dataElementDictionary = dataElementDictionary() ,
                   indicatorDictionary = indicatorDictionary() ,
                   dataSets. = dataSets.() ,
                   categories = categories() ,
                   dataElementGroups =  dataElementGroups() ,
                   ousTree = ousTree() ,
                   geoFeatures = geoFeatures() 
      ) 
      
      saveRDS( meta , paste0( dir() , "metadata.RDS" ) )
      
    }
    
  )



# Variable count table ####
  
 meta_variables = reactive({
    req( dataElementDictionary() )
    req( indicatorDictionary() )
    
    cat( "\n* metadata_widget: meta_variables")
    
    # testing
    # saveRDS( indicatorDictionary() , "indicatorDictionary.rds" )
    # saveRDS( categories() , "categories.rds" )
    # saveRDS( dataElementDictionary() , "dataElementDictionary.rds" )
    # saveRDS( dataSets() , "dataSets.rds" )
    # saveRDS( orgUnits() , "orgUnits.rds" )
  
    # if (  login() & loginFetch() ){ 
    
      mv = tibble(
  
        'Organizational units' = nrow( orgUnits() ) %>% scales::comma() ,
        # 
        'Data sets' = nrow( dataSets() ) %>% scales::comma() ,
        # 
        'Data elements' = nrow( dataElementDictionary() ) %>% scales::comma() ,
    
        'Categories' = nrow( categories() ) %>% scales::comma() ,
    
        # 'Category option combos' = nrow( categoryOptionCombos() ) %>% scales::comma() ,
    
        'Indicators' = nrow( indicatorDictionary() ) %>% scales::comma()
    
    
        )  %>% gather( 'Attribute', 'Number' )
      
      
      # } else {
      #   mv = NULL
      # }
      cat( "...done")
      return( mv )
      
  })
  
  output$variables = renderTable(
    meta_variables() 
    # extensions = 'Buttons' ,
    # rownames = FALSE,
    # options = list( autoWidth = TRUE , 
    #     scrollX = TRUE  ,
    #     lengthMenu = list( c( -1, 5, 10, 25, -1), list( 'All' , '5' , '10', '25') ) ,
    #     columnDefs = list( list( className = 'dt-right' , targets="_all" ) ) ,
    #     dom = 'tB' ,
    #     buttons = buttonList( 
    #       file_name = paste( instance() , '_variables_' , Sys.Date() ) )
    #     )
  )
  
# Upload metadata excel sheet data ####
  
  metadataFile <- reactive({
    
    req( input$input_metadataFile )
    
    inFile <- input$input_metadataFile
    
    cat( '\n metadataFile path: ' , inFile$datapath )
    inFile$datapath
  
  })
  
  uploaded_OrgUnitLevels = reactive({ read.xlsx( metadataFile() ,  "OrgUnitLevels" ) %>% as_tibble() })
  uploaded_OrgUnits = reactive({ read.xlsx( metadataFile() ,  "OrgUnits" )  %>% as_tibble() })
  uploaded_orgUnitHierarchy = reactive({ read.xlsx( metadataFile() ,  "orgUnitHierarchy" , guess_max = 1e6 )  %>% as_tibble() })
  
  uploaded_DataElements = reactive({ read.xlsx( metadataFile() ,  "DataElements" )  %>% as_tibble() })

  uploaded_DataElementGroups = reactive({ read.xlsx( metadataFile() ,  "DataElementGroups" )  %>% as_tibble() })
  uploaded_Categories = reactive({ read.xlsx( metadataFile() ,  "Categories" )  %>% as_tibble() })
  uploaded_DataSets = reactive({ read.xlsx( metadataFile() ,  "DataSets" )  %>% as_tibble() })
  uploaded_Indicators = reactive({ read.xlsx( metadataFile() ,  "Indicators" )  %>% as_tibble() })
  
  uploaded_dataDictionary = reactive({ read.xlsx( metadataFile() ,  "DataElements" )  %>% as_tibble() })
  
  # Testing - not rqd 
  observe({
    print( paste('uploaded_DataElements' , is_tibble( uploaded_DataElements() ) ) )
    print( paste('uploaded_DataSets' , is_tibble( uploaded_DataSets() ) ) )
  })
   

# Resources tab ####

  
  resources = reactive({
      
      if ( login() & loginFetch() ){
        # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
        # if available, use resources method
        cat( '\n **Resources'  )
        
        url = paste0( baseurl() , "api/resources.json" )
        resources =  get( source_url = url )[[1]]
        
        cat( '\n **class(resources)' , class(resources) , '\n')
        glimpse(resources)
        
        if ('data.frame' %in% class(resources) ){
          resources = resources %>%
          as_tibble() %>%
          select( displayName, href ) %>% 
          rename( Attribute = displayName ) %>%
          mutate( href = paste0( href , '?fields=:all&paging=false' ) ) %>%
          arrange( Attribute )
          
          cat( '*metadata_widget found', nrow(resources),'resources' )
        
          return( resources )
        } else {}
            
        
      }
      
    }) 
  
  conditionalPanel( condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  ) 
  
  output$resource_table = renderTable( 
    
      resources()  ,
        
        striped = TRUE , spacing = 's' 
  )
  


# DataElement tab
  
# Return ####
  return( list( 
                dataElements = dataElementDictionary ,
                dataElementGroups = dataElementGroups ,
                categories = categories , 
                orgUnitLevels = orgUnitLevels_with_counts ,
                orgUnits = orgUnits ,
                ousTree = ousTree ,
                geoFeatures = geoFeatures ,
                # uploaded_DataElements = uploaded_DataElements ,
                # uploaded_DataElementGroups = uploaded_DataElementGroups ,
                # uploaded_Categories = uploaded_Categories ,
                # uploaded_DataSets = uploaded_DataSets ,
                indicators = indicatorDictionary 
                # uploaded_dataDictionary = uploaded_dataDictionary
                
                ) )
    
    }
)}

