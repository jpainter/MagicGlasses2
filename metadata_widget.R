
metadata_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fillCol( height = 600, flex = c(NA ) ,
           
  tagList(
    
    add_busy_spinner(spin = "fading-circle", 
                   position = "top-right") ,


    tabsetPanel(type = "tabs",

                tabPanel( "systemInfo", 
                         
                          # Set up shinyjs
                          useShinyjs() , 
                          
                          fluidRow(
                            
                           column( 6 ,
                                   actionButton( ns("getMetadataButton") , 
                                                     "Request Metadata" , style='margin-top:25px' 
                                                 )
                           ) ,
                         
                           column( 6 ,
                                    downloadButton( ns( 'downloadInfo' ), 'Save metadata and system info') ,
                         
                           ) )
                           , 
                          br() ,
                         
                         fluidRow(
                           column( 6, DTOutput( ns('systemInfo') ) ) ,
                           column( 6 , tableOutput( ns('variables') ) )
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

                tabPanel("Map", style = "height:90vh;" ,
                         
                         # DTOutput( ns( 'geoFeaturesTable' ) ) ,
                         # fluidPage(
                          fluidRow( style = "height:80vh;",
                          column(6, 
                                 leafletOutput( ns( "geoFeatures_map" )  )
                          ) ,
                          column(6, 
                                 DTOutput( ns( 'geoFeaturesTable' ) ) 
                                 )
                          ) ,
                         br() ,
                         # 
                         downloadButton( ns( 'downloadGeoFeatures' ), 'Save geo features (.rds file)' )
                         
                         # fluidRow(
                           # column( 4, DTOutput( ns( 'geoFeaturesTable' )  )  ) ,
                           # column( 8 , leafletOutput( ns("geoFeatures_map") ) )
                           # )
                ) ,
                
                tabPanel("API Resources", 
                         
                         h4( "The table below lists a link to retrieve metadata (not the data) for each DHIS2 attribute.") ,
                         
                         h5("Simply paste the link into a new brower window.  This is the mechanism used to retrieve all the information displayed in this app." ) ,
                         
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
      
      x = get( url )[[1]] 
      
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
      
      dataElementGroups =  get( url )[[1]] %>% select( !!cols ) %>%
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
      
      x = get( url )[[1]]
      
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
      
      x = get( url )[[1]]
      
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
    
    cols = c( 'id', 'name' )
    
    url <- paste0( baseurl() ,"api/categoryOptionCombos.json?fields=" ,
                     paste(cols, collapse = ",") , 
                     "&paging=false")
          
    x = get( url )[[1]]
      
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
    cat( '\n *reactive categories \n') 

      cc = categoryCombos()
      coc = categoryOptionCombos()
      
      cat( "- collating categories  \n" )
      showModal(
        modalDialog( title = "Collating categories", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
      
      cat( "- cc.coc  \n" )
      cc.coc = cc %>% select( id, name, categoryOptionCombos ) %>%  
        rename( categoryCombo.id = id , categoryCombo = name ) %>%
        unnest( categoryOptionCombos ) %>% 
        left_join( coc , by = "id" ) %>%
        rename( categoryOptionCombo.id = id , categoryOptionCombo = name )
      
      cat( "- categories  \n" )
      categories = cc.coc %>%
        group_by( categoryCombo.id, categoryCombo ) %>%
        summarise(
          n_categoryOptions = n() ,
          Categories = paste( categoryOptionCombo , collapse = ' ;\n '  ) ,
          categoryOptionCombo.ids = paste( categoryOptionCombo.id , collapse = ' ;\n '  )
        )
      
      removeModal()
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
      
    cat( '\nglimpse(dsde):\n') ; glimpse(dsde)
    
    # Base Dictionary Line List (with categories collapsed)
    cat( '\n -creating dictionary..' )
   
    dictionary = de  %>%  rename( dataElement.id = id ) %>%
      
      left_join( dsde , by = 'dataElement.id' ) %>%
      
      rename(  
              dataElement = name ,
              dataSet.id = dataSet  ) %>%
      
      left_join( ds  , by = 'dataSet.id' ) %>%
      
      left_join( deg , by = 'dataElement.id' ) %>%
      
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
        dom = 'tirp' ) ,
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
    options = DToptions_no_buttons()
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
      
      # if available, use resources method
      url<-paste0( baseurl() ,"api/indicators.json?fields=:all&paging=false")
      
      cols = c( 'id', 'name', 'displayName', 
                # 'description' , # col not available in Guinea Feb 2022
                'numerator' , 'denominator' ,
                'annualized'
      )
      
      indicators =  get( url )[[1]]  %>% select( !!cols ) 
    
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

          ) %>%

      select( name, 
              # description,  # col not available in Guinea Fev 2022
              numerator, denominator, annualized,
              id, displayName, numerator.ids , denominator.ids )
    
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
    options = DToptions_no_buttons()
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
      
      print('orgUnit URL'); print(url)
      
      ousLevels =  get( url )[[1]]  %>% 
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
      
      ous =  get( url )[[1]] 
      
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
    
    if (  login() & loginFetch() ){
      
      cat( '\n - geoFeatures...')
 
      showModal(
        modalDialog( title = "Downloading list of geoFeatures", 
                     easyClose = TRUE ,
                     size = 'm' ,
                     footer=NULL
                     )
        )
 
      levels = orgUnitLevels()$level %>% unique 
      cat( '\n - geoFeatures has levels:' , length( levels ) , '\n')

      geosf = list()
      
        # login_status = try( loginDHIS2( baseurl() , username(), password() ) )
        # print( paste( 'try loginDHIS2 is' , login_status , 
        #               baseurl()  
        #               # , username(), password()  
        #               ))
        
      # pb = progress_estimated( length( levels ) )
      
      for ( l in levels ){
        
        cat( '\n -geoFeatures download level-' , l , '\n' )
        
        # xx= data.frame()
        xx =  geoFeatures_download( level = l  )
        # glimpse( x )
        if ( "sf" %in% class(xx) ){  
          cat('\n - this level is SF \n')
          geosf[[ l ]] = xx  
        } else { next }
      }
      
      # print( 'geosf[[ l ]]') ; print(  glimpse( geosf[[ l ]] ) )
      
 
      # saveRDS( geosf, 'pre_bind_geosf.rds')
      
      # before binding, find common col
      geo_nonzero_rows = map_dbl( geosf, ~ifelse( !is_empty(.x) , nrow(.x), 0  )) > 0
      geo_names_in_common = map(  geosf[geo_nonzero_rows], names ) %>% Reduce(intersect, .)
      geosf. = lapply( geosf[geo_nonzero_rows] , "[", geo_names_in_common ) 
      
      geosf. <- do.call( rbind , geosf.)

      cat( '\n - names geosf: ' ,  names( geosf. ) )
        # 
      # ous = ous %>% select( id, geometry ) 
      # ous = orgUnits()
      cat( '\n - geoFeatures:' , nrow( geosf. ) , 'rows \n' )
      
      cat( '\n - join ous with orgUnits()')
   
      geosf. = geosf. %>%
        right_join( orgUnits() %>%
                     # filter( ! is.na( code ) ) %>%
                     select( id, levelName, leaf, parent ) %>%
                      rename( parentName = parent ),
                   by = 'id' )
      
      cat( "\n - rows with ous linked to orgUnits" , nrow( geosf. ) , '\n')
      
      # test
      # saveRDS( geosf. , 'geosf.rds')
      
      filename = paste0( dir() , "geoFeatures_", Sys.Date()  , ".rds"  )
      saveRDS( geosf. , filename )
      
      # TODO: impute location of missing facilities/admin areas 
      
      # glimpse( ous )
      cat( '\n - missing geometry for' , sum( is.na( geosf.$geometry )), '\n' )
      
      # test
      
      # saveRDS( ous , 'geometry.rds')
      
      removeModal()

    } else {
    
        file = paste0( dir(), geofeatures.files()[1] )
        cat('\n - looking for metadata file:' , file )
      
        if ( file.exists( file ) & !dir.exists( file )){
          
          cat('\n- reading from'  )
          geosf. = readRDS( file )
          cat('\n- geofeatures have' , nrow(geosf.) , "rows" ) 
        } else {
            return()
        }
    }
    
    return( geosf. )
    
  })
  
  output$geoFeaturesTable = DT::renderDT(DT::datatable(

    # shared_geofeatures %>% as_tibble() %>% select( -geometry ),
    geoFeatures.ous() %>% as_tibble() %>% select( -geometry ),
    
    rownames = FALSE, 
    filter = 'top' ,
    options = DToptions_no_buttons()

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
  shared_geofeatures <- SharedData$new(  geoFeatures.ous )
  
  gf.map = reactive({
    req( geoFeatures.ous() )

    # req( orgUnitLevels() )
    cat( '\n * gf.map():')
    
    cat( '\n - library(mapview) - ')
    library( mapview )
    cat( '- loaded \n ')
    
    gf = geoFeatures.ous()
    gf. = shared_geofeatures
    
    
    cat( '\n - split geofeatures')
    split_geofeatures = split( geoFeatures.ous() , f = gf[['levelName']]  )

    levels = names( split_geofeatures )
    cat( '\n geoFeatures map for:' , levels , '\n')

    # levels = gf %>% as_tibble %>% count( level, levelName ) %>% 
    #   filter( !is.na( level ) ) %>%
    #   arrange(level) %>% pull(levelName)
    
    # levels = ouLevels %>% pull( levelName )
    
    # reorder levels
    split_geofeatures = split_geofeatures[ levels ]


    # match( levels, orgUnitLevels() , )]

    # test for empty geometry
    not_all_empty_geo = map_lgl( split_geofeatures , ~!all(is.na(st_dimension(.x))) )
    # print( paste( 'not_all_empty_geo: ', not_all_empty_geo ) )

    n_levels = sum( not_all_empty_geo ) # length(split_geofeatures) # 

    cat( paste('geoFeatures split into' , n_levels , 'levels' ,
                 paste( names( split_geofeatures ), collapse = ',' ), sep = " " ) , '\n')

    colors = RColorBrewer::brewer.pal(n_levels, 'Set2')
    names( colors ) = levels[ not_all_empty_geo ]

    # Set option to display points (https://stackoverflow.com/questions/65485747/mapview-points-not-showing-in-r)
    mapviewOptions(fgb = FALSE)

    split_gf = split_geofeatures[ not_all_empty_geo ]
    
    gf.map = mapView( 
            split_gf ,
            # gf. , 
                 color = colors[ levels[ not_all_empty_geo]  ] ,
                 col.regions = colors[ levels[ not_all_empty_geo]  ] ,
                 alpha.regions = 0, cex = 1 ,
                 burst = TRUE, hide = TRUE
    )
    
    # Testing 
    
    # library( leafdown )
    # cat( '\n - LEAFDOWN new' )
    # adm = gf %>% filter( st_geometry_type(.) %in% c('POLYGON', 'MULTIPOLYGON') )
    # save( gf, adm , file = 'gf.rda' )
    # 
    # levels = unique(adm$level)
    # split_adm = split( adm , f = adm[['levelName']]  )[ order( levels )]
    # split_adm = map( split_adm , ~ as( .x , "Spatial" ) ) 
    # levelNames = names( split_adm )
    # joinCols = rep( "'name' = 'parentName'" , length( levels) - 1) 
    # names( joinCols ) = levelNames[ 1:length( levels) - 1 ]
    # 
    # my_leafdown = Leafdown$new( split_adm  ,
    #                             join_map_levels_by =  joinCols ,
    #                             map_output_id = "geoFeatures_map" , 
    #                             input = input )
    # 
    # cat( '\n - LEAFDOWN add_data')
    # my_leafdown$add_data( gf )
    # 
    # cat( '\n - LEAFDOWN map')
    # gf.map = my_leafdown$draw_leafdown(
    #   fillColor = ~ colors[ levels ] ) 
    # 
    # cat( '\n - LEAFDOWN add_legend')
    # gf.map = gf.map %>%
    #   addLegend( pal = colors[ levels ] )
    # 
    # cat( '\n - LEAFDOWN save')
    # saveRDS( gf.map , 'gf.map.rds')
    
    cat('\n**geoFeatures Map prepared for output-\n')

    return( gf.map@map )  # return leaflet slot of mapview object https://github.com/r-spatial/mapview/issues/58
    
    # mapview not working, try tmap
    # tmap_options(check.and.fix = TRUE)
    # chiefdom = split_geofeatures[ "Chiefdom" ][[1]] %>% tm_shape + tm_borders(col = "red")
    # district = split_geofeatures[ "District" ][[1]] %>% tm_shape + tm_borders(col = "blue")
    # chiefdom + district
    # mapview( zcol = "name", split_geofeatures[ "District" ][[1]] , burst = TRUE )
    
    #     facets = c("coffee_production_2016", "coffee_production_2017")
    # tm_shape(world_coffee) + tm_polygons(facets) + 
    #   tm_facets(nrow = 1, sync = TRUE) 
    
    # Leaflet???
    
  # leaflet.map =
  #   leaflet( ) %>% 
  #     addTiles(group = "OSM (default)") %>%
  #     addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  #     addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  #   
  #   addPolygons( data = split_geofeatures[ "District" ][[1]]  , group ="District" ,
  #                color = "black", weight = 1, smoothFactor = 0.5,
  #                 opacity = 1.0, fillOpacity = 0 , fillColor = "lightblue" ,
  #                highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                    bringToFront = TRUE) 
  #                ) %>%
  #   addPolygons( data = split_geofeatures[ "Chiefdom" ][[1]]  , group = "Chiefdom" ,
  #                color = "brown", weight = .5 , smoothFactor = 0.5, opacity = 1.0, 
  #                fillOpacity = 0 , fillColor = "blue" ,
  #                highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                    bringToFront = TRUE) 
  #                ) %>%
  #   addCircleMarkers( data = split_geofeatures[ "Facility" ][[1]] %>%
  #                       filter( st_geometry_type(.) == 'POINT'), group = "Facility" ,
  #     radius = 1 ,
  #     color = "grey",
  #     stroke = FALSE, fillOpacity = .9
  # ) %>%
  # # Layers control
  # addLayersControl( 
  #   baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  #   overlayGroups = c("District", "Chiefdom", "Facility"),
  #   options = layersControlOptions(collapsed = FALSE)
  # ) 
  #   
  # #   addPopups(-122.327298, 47.597131, content,
  # #   options = popupOptions(closeButton = FALSE)
  # # )
  # 
  # leaflet.map %>% hideGroup( c("Chiefdom" , "Facility") )
  
  })
  
  # test map:
  output$geoFeatures_map <- renderLeaflet({
    gf.map()
    # leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 18)
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
      paste0( dir() , "MetaData_", Sys.Date()  ,".xlsx"  )
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
      
      saveRDS( meta , "metadata.RDS" )
      
    }
    
  )



# Variable count table ####
  
 meta_variables = reactive({
    req( dataElementDictionary() )
    # req( indicatorDictionary() )
   
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
        resources =  get( url )[[1]]
        
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
                orgUnitLevels = orgUnitLevels ,
                orgUnits = orgUnits ,
                ousTree = ousTree ,
                # uploaded_DataElements = uploaded_DataElements ,
                # uploaded_DataElementGroups = uploaded_DataElementGroups ,
                # uploaded_Categories = uploaded_Categories ,
                # uploaded_DataSets = uploaded_DataSets ,
                indicators = indicatorDictionary 
                # uploaded_dataDictionary = uploaded_dataDictionary
                
                ) )
    
    }
)}

