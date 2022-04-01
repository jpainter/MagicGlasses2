
login_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # fillCol( height = 600, flex = c(NA ) ,
  tagList(

    # tabsetPanel(type = "tabs",
                
                # tabPanel("Login", 
                         
                         useShinyjs() ,  # Set up shinyjs
                         
                         h3( 'Login to DHIS2 server*') ,
        
         
                        fluidRow( 
                           # column( 6 , checkboxInput( ns("demo") , 
                           #                label = "Click to choose from one of the DHIS2 demo instances", 
                           #                FALSE )
                           # ) , 
                           
                           column( 12, 
                                   
                                   h4("Choose from list of DHIS2 Instances**:") ,
                                
                                   
                                   selectInput( ns('instance') , NULL , choices = NULL ) ,
      
                                   tags$blockquote("*Once data has been downloaded and saved to the directory (at left), 
                                                   the app is fully functionaly without being connected to the DHIS2 server") ,
                                   
                                   br()  
                           ) ,
                                   
                                   tags$blockquote("**the app provides links to the demo versions of DHIS2.  
                                                   To add other instances, copy the excel file Instances.xlsx and save it as
                                                   _Instances.xlsx within the MagicGlasses2 folder.  You can insert rows into this 
                                                   spreadsheet following the same format as the demo instances.
                                                   [if not saved with '_' prefix, it will be overwritten during the next GIT pull]"
                           )
                           
                           
                         ) ,
                         
                         fluidRow( 
                           
                           column( 12, 
                           
                           h4('Or enter address and credentials:') ,
                           
                           inputPanel( 
                               textInput( ns("baseurl") , label = "DHIS2 URL:", NULL ), # "https://play.dhis2.org/2.33.1/"
                               
                               textInput( ns("username") , label = "User name:", NULL ), # "admin"
                               
                               passwordInput( ns("password") , label = "Password:", NULL )
                           )
                           ) 
                           ) ,
       
                         
                          fluidRow( 
                           column( 12, h2( textOutput( ns("Status") ) ) ) ) ,
            
                         
                         fluidRow(
                           column( 12, tableOutput( ns('systemInfo') ) ) 
                           )
                           
    #             ) 
    # )
    
    
  # )
        ) # end fillColl
          
          } # ui

# Server function ####
login_widget_server <- function( id ){
  moduleServer(
    id , 
    function( input, output, session, 
                        org_Units ,
                        data_elements , 
                        malariaDataElements ) {
      
  add_busy_spinner(spin = "fading-circle", 
                   position = "top-right")

  # reactives to toggle login status
  login = reactiveVal( FALSE )

  
# hide instance choice list unless demo checked ####
  shinyjs::hideElement( id = "instance" , asis = TRUE )
  # shinyjs::hideElement( id = "instancesFile" )
  
  observe({
      # req( input$demo )
      cat( '\n observe demo and shinyjs hide/show' )
      
      if ( !is.null( input$demo ) && input$demo ){
        
        cat("\n ** Show Instances\n")
        showElement( "instance" , asis = TRUE )
        # shinyjs::showElement( "instancesFile" )
        
      } else {
        cat("\n ** Hide Instances\n")
        hideElement( "instance" , asis = TRUE )
        # shinyjs::hideElement( "instancesFile" )
        
        }
  })
  
# update credentials after selecting instance ####

   observe({
     req( instances()  )
     req( input$instance )

      if (  nchar( input$instance ) > 0 ){ 
          cat("\n ** DEMO\n")
        
          # cat( '\n instances()$Instance:' , instances()$Instance )
          cat( '\n input$instance:' , input$instance, '\n' )
      
          i_row = instances()$Instance %in% input$instance 

          cat( '\n i_row:' , i_row )
          cat( '\n input$demo:' , input$demo == TRUE )
          ins = instances()[ i_row ,]
          # print( ins )

          updateTextInput( session, "baseurl" , value = ins$IPaddress )
          updateTextInput( session, "username" , value = ins$UserName )
          updateTextInput( session, "password" , value = ins$Password )
          
      } else {
        
        cat("\n ** NO DEMO\n")
        updateTextInput( session, "baseurl" , value = "" )
        updateTextInput( session, "username" , value = "" )
        updateTextInput( session, "password" , value = "" )
     
      } 
   } )
   
  
 # Instances tibble
 instances = reactive({
   cat('reactive instances \n')
   iFile = "Instances.xlsx"
   
  if ( file.exists( '_Instances.xlsx') ){
    
      iFilePrivate = '_Instances.xlsx'
      i = read_excel( iFilePrivate ) 
      
    } else {
      
      i = read_excel( iFile )
    }

     cat('END: reactive instances \n')
     return( i ) 
 })
  
 # Update instance choices
   observe({
    req( instances() )
     
    updateSelectInput( session, "instance" , choices = c( "" , instances()$Instance ) )
      
  })
   

# Instance--selection ####
   # instance
  instance = reactive({

    cat('\n* instances:')
    if( nchar( input$instance ) > 0 ){
   
          i_row = which( instances()$Instance %in% input$instance )

          cat( 'Instances:\n' , instances()$Instance, '\n'  )
          cat( 'i_row:' , i_row , '\n')
         
          Instance = instances()$Instance[ i_row ]
          cat( 'Instance:' , Instance , '\n')
 
          
    } else {
       Instance = input$baseurl
     }
      
    cat('\n - done ')    
    return( Instance )

   })
   
  baseurl = reactive({
    req( input$baseurl )
    # if url is from login or dashboard url, trimto get baseurl
    # possible.suffixes:
    suffix.part = "dhis-web"
    
    strsplit( input$baseurl, suffix.part)[[1]][1]
    
  })
  
  username = reactive({ input$username })
  
  password = reactive({ input$password })
  
  

  credentialsProvided <- reactive({
    
    req( baseurl()  )
    req( input$username )
    
    credentialsProvided = !is_empty( baseurl() ) && !is_empty( input$username ) && !is_empty( input$password ) 
    
    print( paste( 'toLogin' , credentialsProvided ))
    
    return( credentialsProvided )
    })
  
  
  # Login Status
  observeEvent( credentialsProvided() , {

    print( paste( 'login' ,  baseurl() , input$username, input$password  ))
    
    if ( is_empty( baseurl() ) | is_empty( input$username ) | is_empty( input$password ) ){

      login( FALSE )
    }

    l = try( loginDHIS2( baseurl() , input$username, input$password) )

    print( paste( 'try loginDHIS2 is' , l , baseurl() , input$username, input$password  ))
    
    if ( class( l ) == "logical" ) {

      login( TRUE )

    } else {

      login( FALSE )
    }

    print( paste( 'observe event input$password, login() is' , login() ))
  })
  
  # Update logged in status
  observeEvent( login() , {
    if ( login() ){
      output$Status = renderText( paste( 'Logged in' , instance() ) )
    } else {
      output$Status = renderText( 'Not logged in' )
      }
  })
  
 
# system info  ####
    system.info = reactive({
    
    req( login() )
    if ( login() ){
      cat( '\n *login_widget system.info')
     
      url = paste0( baseurl() , "api/system/info" )
      getInfo = GET( url )
      getInfo.content =  content( getInfo , "text")
      
      info =   jsonlite::fromJSON( getInfo.content ) 
      
      info[ map_dbl( info , length ) == 1 ] %>% 
        as_tibble() %>%
        select( 
          systemName ,
          version , 
          lastAnalyticsTableSuccess	,
          intervalSinceLastAnalyticsTableSuccess	,
          lastAnalyticsTableRuntime ,
          buildTime ,
          serverDate ,
          contextPath  ,
          calendar ,
          dateFormat 
          
        ) %>% 
        gather( Attribute, Value ) 
      
    } else { 
      
      tibble( connection = "Waiting for login" ) 
      
      }
  }) 
 

# Status/connection  ####
  output$connection = renderText({  
    
    req( baseurl() ) 
    # req( login() )
    paste0( baseurl() , "api/system/info"  )
    
  })
 
# Sytem Info Table #### 
  conditionalPanel( condition="$('html').hasClass('shiny-busy')",
                    tags$div( "Loading...", id="loadmessage" )
  ) 
  
  output$systemInfo =  renderTable(
    
    if( is.null(system.info()) ){ 
      
    } else { 
      system.info()
    }
  
  )
  
  
### output to Login tab  ####
  
  output$connection = renderText({  
    
    req( baseurl() ) 
    # req( login() )
    paste0( baseurl() , "api/system/info"  )
    
  })
  
  conditionalPanel( condition="$('html').hasClass('shiny-busy')",
                    tags$div( "Loading...", id="loadmessage" )
  ) 
  
  output$systemInfo =  renderTable(
    
    if( is.null(system.info()) ){ 
      
    } else { 
      system.info()
    }
  
  )
  
# Return ####
  return( list( 
                login = login ,
                baseurl = baseurl , 
                username = username ,   
                password = password , 
                instance = instance ,
                system.info = system.info
                # uploaded_OrgUnitLevels = uploaded_OrgUnitLevels ,
                # uploaded_OrgUnits = uploaded_OrgUnits ,
                # uploaded_DataElements = uploaded_DataElements ,
                # uploaded_DataElementGroups = uploaded_DataElementGroups ,
                # uploaded_Categories = uploaded_Categories ,
                # uploaded_DataSets = uploaded_DataSets ,
                # uploaded_Indicators = uploaded_Indicators ,
                # uploaded_dataDictionary = uploaded_dataDictionary
                
                ) )
    
    }
)}

