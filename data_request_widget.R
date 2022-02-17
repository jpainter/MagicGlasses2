data_request_widget_ui = function ( id ) 
{
    ns <- NS(id)  
  
    add_busy_spinner(spin = "fading-circle", 
                   position = "top-right") 
  
    fluidRow(
       column( 5, 
         selectInput( ns("level") , label = "OrgUnit Levels:" , 
                      width = '90%',
                      choices = "Load metadata to get values" , 
                      selected = 1,
                      multiple = FALSE ,
                      selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     )
         ) ,
       column( 5, 
          selectInput( ns("period") , label = "Period:" , 
                        width = '90%',
                        choices = c('months_last_year','months_last_2_years','months_last_3_years','months_last_4_years','months_last_5_years') , 
                        selected = 1 ,
                        multiple = FALSE ,
                        selectize = FALSE, 
                        size = 4  ##needed for `selected = FALSE` to work ) 
                       ) 
        ) ,
      column( 2,         
        actionButton( ns("requestDataButton") , 
                      "Request data" , style='margin-top:25px' 
                      )
      )
    )
        } 
        
data_request_widget_server <- function( id , 
                                    loginDetails = list() , 
                                    dataDirectory = NULL ,
                                    metadata_widget_output = NULL,
                                    data_widget_output = NULL ){
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
      formula_elements = reactive({ data_widget_output$formula_elements() })
      dataset.file = reactive({ data_widget_output$dataset.file() })
      dataset = reactive({ data_widget_output$dataset() })
      orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })
      orgUnits = reactive({ metadata_widget_output$orgUnits() })
      
  
      formula.names = reactive({ 
          req( formulas() ) 
          cat( '\n formula columns:', names(formulas()) ,'\n')
      
          formulas()$formulaName 
          })
      
      # Update level names
      observe({
            cat( '\n* updating levels' )
            if ( !is.null(orgUnitLevels() )){
              oulvls = orgUnitLevels() %>% pull( levelName )
              oulvls = c( 'All-levels' , oulvls )
              updateSelectInput( session, 'level' ,
                               choices = oulvls,
                               selected = 1 )
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
      .orgUnitLevels = orgUnitLevels()
      
      ou = case_when(
        
        input$level %in% 'All-levels' ~ 
          list( .orgUnitLevels %>% arrange( desc( level )) %>% pull( level ) %>%
          paste0( "LEVEL-" , .  ) )  ,
      
        # input$level %in% 'Leaf-only' ~ 
        #   list( 
        #   # split orgunit ids into chunks of 100
        #   orgUnits  %>% 
        #     filter( leaf == TRUE ) %>% pull( id ) %>%
        #           split( . , ceiling(seq_along( . )/100) ) %>%
        #           map_chr( . , ~paste( .x , collapse = ";" ) )
        #   )  , 
      
        TRUE ~ list(
          .orgUnitLevels %>% 
            filter( levelName %in% input$level ) %>%
            pull( level ) %>%  paste0( "LEVEL-" , .  )  ) 
      
      ) %>% unlist #nb: each case evaluated as list, otherwise alsways returns vector of max length
    
      cat( '\ndata_request_widget orgUnits:' , ou )
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
          .periods = input$period
          .level = input$level
          # .orgUnitLevels = orgUnitLevels()
          .orgUnits = orgUnitRequest()
          .formula.name = indicator()
          .elements = formula_elements() %>% 
            unite( id , dataElement.id, categoryOptionCombo.ids , sep="." ) %>%
            pull( id )
          .level1.id = orgUnits() %>% filter( level == 1 ) %>% pull( id )
            
          cat( '\n - formula.request elements:' , length( .elements ) , ':\n' ,
               .elements )
          
          # Previous dataset file: 
          .dataset = dataset() 

          cat( '\n - dataset():' , dataset.file() )
          file =  paste0( data.folder() , dataset.file() )
          
          if ( file_test("-f", file )  ){
            .previous_dataset_file = file 
            .update = TRUE 
            cat( '\n - previous file exists:' ,  file  ) 
          } else {
            cat( '\n - no previous dataset file:'  )
            .update = FALSE 
            .previous_dataset_file = ''
          }
          
          x  = api_data( 
                         update = .update , 
                         baseurl = .baseurl , 
                         username = .username , 
                         password = .password ,
                         elements = .elements, 
                         orgUnits = .orgUnits ,
                         periods = .periods , 
                         formula = .formula.name ,
                         previous_dataset_file = .previous_dataset_file ,
                         prev.data =  .dataset ,
                         level1.id = .level1.id ,
                         dir =  data.folder() ,
                         shinyApp = TRUE,
                         parallel = FALSE )
          
          saveAs = paste0( .dir, .formula.name , "_" , 
                           .level ,"_", .periods ,"_", Sys.Date() , ".rds")
          cat( '\nsaving formula.request as', saveAs )  
          
        showModal(
          modalDialog( title = "Finished downloading.  Now saving the file", 
                       easyClose = TRUE ,
                       size = 's' ,
                       footer=NULL
                       )
          )  
                    
          saveRDS( x , saveAs )
          removeModal()
          cat( '\n* finished downloading' , .formula.name , '\n') 
          completedRequest( completedRequest() + 1 )
          
          
      }
          
        })
    
     
     return( list( 
          completedRequest = completedRequest
            )
            )
    
    }) #end moduleServer
} #end server

 


