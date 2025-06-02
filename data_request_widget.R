data_request_widget_ui = function ( id ) {
  ns <- NS(id)
  
 tagList(
      
          shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-right'
            # , margins = c(70, 1200)
          ) ,
          
          
  fluidPage(

        div(
        h5( "Request data from DHIS2 instance"), # Header text for the top of the page
        style = "font-weight: bold; text-align: center; text-decoration: underline;" # Center alignment and spacing
      ),
      
      fluidRow( 
        
        column( 2,  h2("Formula: ") ) ,
        column( 10, h2( textOutput( ns("formulaName")  ) ) )
      
        ) ,
      
      p("\n") ,
      
      fluidRow( 
       
        column( 6, offset=0,
        selectInput( ns( "level" ) , label = "OrgUnit Levels:" ,
                     width = '90%',
                     choices = "Load metadata to get values" ,
                     selected = 1,
                     multiple = FALSE ,
                     selectize = FALSE,
                     size = 4  ##needed for `selected = FALSE` to work )
                    )
        ) ,
        
        column( 3,  offset=0,

          selectInput( ns( "period" ) , label = "Period:" ,
                       width = '90%',
                       choices =  c('Monthly', 'Weekly')  ,
                       selected = 1 ,
                       multiple = FALSE ,
                       selectize = FALSE,
                       # size = 4  ##needed for `selected = FALSE` to work )
          )
          )  ,

        column( 2, offset=0,
               selectInput( ns( "years" ) , label = "Years:" ,
                       width = '90%',
                       choices =  1:20  ,
                       selected = 1 ,
                       multiple = FALSE ,
                       selectize = FALSE,
                       size = 4  ##needed for `selected = FALSE` to work )
          ) )

       ) ,
      
    fluidRow( # height = '25%' ,
       
       column( 2 , actionButton( ns( "requestDataButton" ) , height = "10%" ,
                              "Request data"   , style='margin-top:25px'
                              ) ,
              ) ,
       column( 10, 
               div( 
                 p("**After download complete, use refresh button (Formula tab) and then \nre-select the formula to see the download file." ),
                 style = "font-weight: bold; text-align: center;" )
               ) 
      )
) )
}
        
data_request_widget_server <- function( id , 
                                    loginDetails = list() , 
                                    dataDirectory = NULL ,
                                    metadata_widget_output = NULL,
                                    data_widget_output = NULL,
                                    regions_widget_output = NULL){
  moduleServer(
    id ,
    function( input, output, session 
              # loginInfo = loginDetails ,
              # data.folder = dataDirectory,
              # formula.details = data_widget_output ,
              # metadata = metadata_widget_output
              ) {
      
      login = reactive({ loginDetails$login() })
      baseurl = reactive({ loginDetails$baseurl() })
      username = reactive({ loginDetails$username() })
      password = reactive({ loginDetails$password() })
      data.folder = reactive({ dataDirectory$directory() })
      indicator = reactive({ data_widget_output$indicator() })
      formulas = reactive({ data_widget_output$formulas() })
      formulaName = reactive({ data_widget_output$formulaName() })
      formula_elements = reactive({ data_widget_output$formula_elements() })
      dataset.file = reactive({ data_widget_output$dataset.file() })
      dataset = reactive({ data_widget_output$dataset() })
      orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
      orgUnits = reactive({ metadata_widget_output$orgUnits() })
      selected_regions =  reactive({ regions_widget_output$selected_regions() })
  
  
      # updata formula
      observeEvent( !is.null( formulaName() ) , {
        cat( "\n* data_request_widget sees formula: ", formulaName() )
        
        output$formulaName <- renderText({ formulaName()  })
      } )
      
      # Update level names
      observeEvent( !is.null( orgUnitLevels() ) , {
        cat( '\n* data_request_widget: updating levels' )
            
        # testing
        # saveRDS( orgUnitLevels(), 'orgUnitLevels.rds')
        
        oulvls = orgUnitLevels() %>% pull( levelName )
              # oulvls = c( 'All-levels' , oulvls )
              ## disable choice of other levels
              oulvls = c( 'All-levels'  )
              updateSelectInput( session, 'level' ,
                               choices = oulvls,
                               selected = 'All-levels'  )
        cat( "...done")
          } )
      
      # Update period: choose largest value of month or week
      observe({
            cat( '\n* updating period' )
            if ( !is.null( formula_elements() )){
              p = min( formula_elements()$periodType , na.rm = T ) 
              updateSelectInput( session, 'period' , selected = p )
            }
          } )
      
# Request data ####
    request = reactiveVal( FALSE )
   
    observeEvent( input$requestDataButton  , {
      
     
      if ( login()  ){
          request( TRUE )       
          cat('\n * data_request_widget requestData Button' , request(), '\n')
          
          x = formula.request()
  
        } else {
  
          request( FALSE )
          
          showModal(
          modalDialog( title = "Please logon before requesting data", 
                       easyClose = TRUE ,
                       size = 'm' ,
                       footer=NULL
                       )
          )  
          
        }
  })
      
    orgUnitRequest = reactive({
      req( input$level )
      cat( '\n* data_request_widget orgUnits reactive')
      
      # Update Feb 20225
      # if all_levels (national), then null and api will pull from all child values below highest level.
      # if Regions has selection and sub-national selected, then return the selected  ou 
      
      # .orgUnitLevels = orgUnitLevels()
      
      # ou = case_when(
      #   
      #   input$level %in% 'All-levels' ~ 
      #     list( .orgUnitLevels %>% arrange( level ) %>% pull( level ) %>%
      #     paste0( "LEVEL-" , .  ) )  ,
      # 
      #   # input$level %in% 'Leaf-only' ~ 
      #   #   list( 
      #   #   # split orgunit ids into chunks of 100
      #   #   orgUnits  %>% 
      #   #     filter( leaf == TRUE ) %>% pull( id ) %>%
      #   #           split( . , ceiling(seq_along( . )/100) ) %>%
      #   #           map_chr( . , ~paste( .x , collapse = ";" ) )
      #   #   )  , 
      # 
      #   TRUE ~ list(
      #     .orgUnitLevels %>% 
      #       filter( levelName %in% input$level ) %>%
      #       pull( level ) %>%  paste0( "LEVEL-" , .  )  ) 
      # 
      # ) %>% unlist #nb: each case evaluated as list, otherwise always returns vector of max length
      # 
      # cat( '\n - data_request_widget orgUnits:' , ou )
      
      # if regions selected...
      cat( "\n - selected_regions:"  )
      # selected_regions = selected_regions()
      sr = selected_regions()
      # sr = list( level2 = selected_regions$level2 ,
      #          level3 = selected_regions$level3 ,
      #          level4 = selected_regions$level4 ,
      #          level5 = selected_regions$level5 )
            
      cat( "\n - selected_regions:" , unlist( sr ) )
      # cat( "\n - observer(selected_regions):" , unlist( isolate(selected_regions() ) ) )
      
      if ( !is.null( sr[[1]] ) ){
        cat( "\n - data_request_widget orgUnitRequest selected_regions" )

        level = find_lowest_nonnull( sr )
        # ou = paste( sr[[ level ]] , collapse = ";" )
        ou = sr[[ level ]] 
        cat( "\n\n -- ", level, ou )
        
      } else { ou = NULL }
      
      return( ou )
      })
    
    completedRequest = reactiveVal( 0 )
    
    formula.request = reactive({
    
      # cat( '\n Are orgUnitLevels() available?:' ,  !is_empty( orgUnitLevels() ) )
      
     if ( is_empty( orgUnitLevels() ) ){
       showModal(
                  modalDialog( title = "Please load metadata before requesting data", 
                       easyClose = TRUE ,
                       size = 'm' ,
                       footer=NULL
                       )
       )
       return()
     } 
      
    if ( login() & request() ){
        cat( '\n* formula.request reactive')
          
          .dir = data.folder()
          .baseurl = baseurl() 
          .username = username() 
          .password = password()
          .period = input$period
          .years = input$years
          .level = input$level
          # .orgUnitLevels = orgUnitLevels()
          # .orgUnits = orgUnitRequest()
          .formula.name = indicator()
  
          
          # To use jim Grace api/dataSets, need to set .orgunits to the highest level
          # If orgUnits specified, then use them 
          
          # Testing
          # saveRDS( orgUnits(), 'orgUnits.rds')
          # saveRDS( orgUnitRequest() , "orgUnitRequest.rds" )
            
          if ( is.null( orgUnitRequest() ) ){
            cat( "\n - National data request")
            .orgUnits = orgUnits() %>% 
              filter( level %in% min( orgUnits()$level, na.rm = T)  ) %>%
              pull( id )
            
          } else {
            cat( "\n - subNational data request")
            
            .orgUnits = orgUnits() %>% 
              filter( name %in% orgUnitRequest()  ) %>%
              pull( id ) 
          }
          
          
          # If categoryOption is NA, then omit from request.  This will return 'total' with no categories.  
          # NB this happens if any category is missing 
          # TODO: revise so there can be a mix of categoryOptionCombo and no categoryOptionCombo
          
            #        .elements = formula_elements() %>% 
            # mutate( )
            # unite( id , dataElement.id, categoryOptionCombo.ids , sep="." ) %>%
            # unite( name , dataElement, Categories , sep="." ) %>%
            # select( id , name )
          
          if ( any(!is.na( formula_elements()$categoryOptionCombo.ids )) ){
                .elements = formula_elements() %>% 
                  filter( ! dataElement.id == "aaaaaaaaaaa" ) %>%
                    mutate( 
                      categoryOptionCombo.ids = ifelse( is.na( categoryCombo.id ) , NA , categoryOptionCombo.ids ) ,
                      Categories = ifelse( is.na( categoryCombo.id ) , NA , Categories ) 
                      ) %>%
                    unite( id , dataElement.id, categoryOptionCombo.ids , sep="." , na.rm = TRUE )  %>%
                    unite( name , dataElement, Categories , sep="." , na.rm = TRUE ) %>%
                    select( id , name )
            } 
          else {
                .elements = formula_elements() %>% 
                          unite( id , dataElement.id,  sep="." )  %>%
                          unite( name , dataElement,  sep="." ) %>%
                          select( id , name )
          }
          
          # TEsting 
          # saveRDS( .elements, "dataRequestElements.rds" )
          
          .level1.id = orgUnits() %>% filter( level == 1 ) %>% pull( id )
          
          cat( '\n - .level1.id:' , .level1.id )  
          
          cat( '\n - data_request elements:' , length( .elements$name ) , ':\n' ,
               .elements$name )
          

          cat( '\n - dataset():' , dataset.file() )
          file =  paste0( data.folder() , dataset.file() )
          
          
          if ( file_test("-f", file )  ){
            .previous_dataset_file = file 
            .update = TRUE 
            cat( '\n - previous file exists:' ,  file  ) 
           # Previous dataset file: 
          .dataset = dataset() 

          } else {
            cat( '\n - no previous dataset file:'  )
            .update = FALSE 
            .previous_dataset_file = ''
            .dataset = NA 
          }
          
          
          cat( "\n - .period is" , input$period  )
          # If missing, assume period is monthly
          if ( is.null( .period ) ) .period = "Monthly"
          
          x  = api_data( 
                         update = .update , 
                         baseurl = .baseurl , 
                         username = .username , 
                         password = .password ,
                         elements = .elements, 
                         orgUnits = .orgUnits ,
                         # periods = .periods , 
                         periodType = .period ,
                         YrsPrevious = as.integer( .years ) ,
                         formula = .formula.name ,
                         previous_dataset_file = .previous_dataset_file ,
                         prev.data =  .dataset ,
                         level1.id = .level1.id ,
                         dir =  data.folder() ,
                         shinyApp = TRUE,
                         parallel = FALSE,
                         childOnly = TRUE )
          
         .periods = paste0( .years, 'yrs' )
          
          saveAs = paste0( .dir, .formula.name , "_" ,
                           .level ,"_", .periods ,"_", Sys.Date() , ".rds")

          cat( '\n saving formula.request as', saveAs )
          
        showModal(
          modalDialog( title = "Finished downloading.  Now saving the file", 
                       easyClose = TRUE ,
                       size = 's' ,
                       footer=  cat( nrow(x), 'records downloaded.Of these, there was no value for' , sum( is.na(x$SUM) ), 'records' )
 
                       )
          )  
                    
          saveRDS( x , saveAs , compress = FALSE )
          removeModal()
          
          showModal(
            modalDialog( title = "New file is saved", 
                         easyClose = TRUE ,
                         size = 's' ,
                         footer= '(To refresh data in the app: use the refresh button (to left) and then re-select formula file and formula. '
                         )
          )  
          cat( '\n* finished downloading' , .formula.name , '\n') 
          completedRequest( completedRequest() + 1 )

      }
          
        })
    
     
     return( list( 
          completedRequest = reactive({ completedRequest })
            )
            )
    
    }) #end moduleServer
} #end server

 


