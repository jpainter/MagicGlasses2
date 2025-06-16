data_widget_ui = function ( id ) 
{
        ns <- NS(id)
        
  tagList( 

      shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-left',
            timeout = 100 ,
            onstart = FALSE 
            # , margins = c(70, 1200)
          ) ,
  
       div(
        # Header text for the top of the page
        p( "Create groupings (formulas) of data by subject (e.g. confirmed malaria cases) with a file that indicates which DHIS2 data elements or indicators are included"), 
        style = "font-weight: bold; text-align: left; margin-bottom: 20px;" # Center alignment and spacing 
      ),
      
        actionButton( ns("refresh"), "Refresh") ,
        
        selectInput( ns("formula.file") , 
                     label = "Formula Files:" ,
                      width = '95%',
                      choices = NULL , 
                      selected = FALSE,
                      multiple = FALSE ,
                      selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,
          
         selectizeInput( ns("formula") , 
                      label = "Select Formula:" , 
                      width = '95%',
                      choices = "" ,
                      options = list(create = TRUE) , 
                      selected = FALSE,
                      multiple = FALSE ,
                      # selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,

       div(
        selectInput( ns("dataset") , 
                     label = div( "Data previously downloaded from DHIS2:" ,
                                  style = "font-size: 100%"  ) ,
                    width = '95%',
                    choices = NULL , 
                    selected = FALSE,
                    multiple = FALSE ,
                    selectize = FALSE, 
                    size = 4  ##needed for `selected = FALSE` to work ) 
                   ) ,
        style = "overflow-y: auto; font-size: 66%; width: 100%" ) ,
      
      checkboxInput( ns("rescan") , "Rescan dataset for analysis" ) ,
      
      br()

)       
        # ) # end fillColl
          
          } # ui
        

data_widget_server <- function( id ,
                                metadata_widget_output = NULL,
                                directory_widget_output = NULL ,
                                data_request_output = NULL 
                                ) {
     moduleServer(
        id,
        function(input, output, session 
                 ) {
          
       # Reactive dependecies
      data.folder = reactive({ directory_widget_output$directory() })
      dataElements = reactive({ metadata_widget_output$dataElements() })
      dataSets = reactive({ metadata_widget_output$dataSets() })
      categories = reactive({ metadata_widget_output$categories() })
      ousTree = reactive({ metadata_widget_output$ousTree() })
      completedRequest = reactive({ data_request_output$completedRequest() })
      
      # completedRequest = reactive({ data_request_output$completedRequest() })
        
      formula.files = reactive({ 
          req( data.folder() )
          cat( '\n* data_widget looking for formula files in' , data.folder() , '\n')
        
          ff = files( search = 'Formulas_' , dir = data.folder() , type = 'xlsx|rds' )  
          if ( is_empty( ff ) ){
            cat( '\n - no forumula files in directory' )
            return( )
          } 
          
          # Arrange by modified date
          formula_file.mdate = file.info( paste0( data.folder() , ff  ) )$mtime
          ff = ff[ rev(order( formula_file.mdate )) ]
          
          # cat( '\n - formula.files:' , ff  )
          if ( !any(file.exists( paste0( data.folder() , ff  ) ) )) return()
          
          return( ff )
          })
        
        # trigger refresh after completed download
        observeEvent(input$refresh , {
          cat( '\n* Update data widget text boxes')
          
          aa = data.folder()
          
          cat( '\n - looking for formula files in' ,aa  , '\n')
          
          ff = files( search = 'Formulas_' , dir = aa , type = 'xlsx|rds' )  
          if ( is_empty( ff ) ){
            cat( '\n - no forumula files in directory' )
            return( )
          } 
          
          # Arrange by modified date
          formula_file.mdate = file.info( paste0( aa , ff  ) )$mtime
          ff = ff[ rev(order( formula_file.mdate )) ]
          # a = formula.files()
          
          cat( '\n - Update data formula files')
          updateSelectInput( session, 'formula.file' , 
                                      choices = ff , 
                                      selected = 1  ) 
        
         b =  formula.names()
         cat( '\n - Update data formula.names')
         updateSelectInput( session, 'formula' , 
                            choices =  ""  ,
                            selected = NULL )  
         
         cat( '\n - Update rds_data_file')
         updateSelectInput( session, 'dataset' ,
                                      choices = "" ,
                                      selected = NULL ) # rds_data_file()[1] )            }            
        })
        
        observeEvent( completedRequest() , { 
            cat('\n* data_widget completedRequest():' )
            a = formula.files()
        })
        
        observe({
          cat('\n* updating directory \n')
          # updateTextOutput( session, 'directory' , value = data.folder()  ) 
          output$directory = renderText({ data.folder() })
        })
        
        observe({
          cat('\n* updating formula.files input ')
          updateSelectInput( session, 'formula.file' , 
                                      choices = formula.files() , 
                                      selected = 1  ) 
        })
        
        formulas =  reactive({
          # req( input$formula.file )
          cat( '\n* formulas:' )
          
          if ( is.null( input$formula.file ) ) return( NULL )
          
          file = paste0( data.folder() , input$formula.file )
          cat( '\n - formula file:' , file )
          
          if ( !any( file.exists( file ) )) return( NULL )
          
          if ( grepl( fixed('.xlsx'), file )  ){
            cat( '\n - read xls file', file )
            formulas = read_excel( file , sheet = 'Formula') %>% 
              filter( !is.na(Formula.Name)) %>%
              arrange( desc( Formula.Name ) ) 
          
            } else {
            
            cat( '\n - reading formula rds file', file )
            formulas = readRDS( file )
          }
          
          cat( '\n - formula.Name:' , unique( formulas$Formula.Name ) )
          
          return( formulas )
        })
        
        formula.names = reactive({ formulas()$Formula.Name })
        
        all_formula_elements =  reactive({
          req( input$formula.file )
          cat( '\n* all_formula_elements:')
          
          file = paste0( data.folder() , input$formula.file )
          cat( '\n - formula file' , input$formula.file )
          
          if ( !any(file.exists( file ) )) return( NULL )
          
          if ( grepl( fixed('.xlsx'), file ) ){
            
            cat( '\n - read xls file' , file )
            formulas = read_excel( file , 
                                   sheet = 'Formula Elements' , 
                                   guess_max = 1e6 )  
          
            } else{
              
            cat( '\n - reading formula elements from formula rds file', file )
            formulas = readRDS( file ) 
            }
        
        })
        
        formula_elements =  reactive({
          req( all_formula_elements() )
          req( input$formula.file )
          req( input$formula )
          cat( '\n* formula_elements:')

          cat( '\n - selecting formula' )
          all_formula_elements()  %>%
            filter( Formula.Name %in% input$formula ) 
        })
        
        data.dir_files = reactive({ 
            # req( completedRequest() )
            req( input$formula.file )
            if ( !dir.exists( data.folder() ) ) return( NULL )
            # trigger   when there is a data request finishes
            #  = completedRequest() > 0  
            # cat( '\n data.dir_files completedRequest:' , completedRequest() )
            dir.files = list.files( data.folder()  )
            cat( "\n - number of dir.files :", length( dir.files ) ) 
            return( dir.files )
        })
        
        rds_data_file = reactive({
          req( data.dir_files() )
          req( data.folder() )
          req( input$formula )
            
          dir.files = data.dir_files()
        
          formula = input$formula # paste0( input$formula , "_" )
          cat( '\n* rds_data_file formula: ' , input$formula , '\n' )
          
          file.type = 'rds' # input$file.type 
          # file.other = ifelse( input$cleaned %in% "Cleaned" , '_Seasonal' , "" )  # input$file.other
          
          # file.keywords = input$file.keywords # '_formulaData|Seasonal|dts|rds'
          file.keywords = 'rds'
            
          data.files = dir.files[ 
                  # grepl( 'All levels' , dir.files ) &
                  grepl( file.type , dir.files, ignore.case = T ) &
                  # grepl( file.other, dir.files, fixed = TRUE  ) &
                  grepl( file.keywords, dir.files, ignore.case = T ) &
                  ! grepl( "Update_", dir.files, ignore.case = T )]
        
          cat('\n- all levels data files:' , length( data.files ) )
          
          f.formula = grepl( formula , data.files , fixed = TRUE )
          
          cat("\n- f.formula:" , sum( f.formula , na.rm = TRUE ) ) 
          
          if ( sum( f.formula ) == 0 ){
            cat( '\n - no data files for this formula' )
            return( "" )
          } 
          
          if ( !dir.exists( data.folder() )){
            cat( '\n - no folder matching data.folder()' )
            return( "" )
          } 
          
          data_files = data.files[f.formula] # %>% most_recent_file()
          
          # cat( '\n data_files are:\n' , data_files )
          
          # Arrange by modified date
          data_file.mdate = file.info( paste0( data.folder() , data_files ) )$mtime
          data_files = data_files[ rev(order( data_file.mdate )) ]
          
          cat( '\n - done:' , length( data_files ) , 'files')
          return( data_files )

})


        # update formulas 
        observeEvent(  input$formula.file , {  
            cat( '\n* updating formula list' )
            updateSelectInput( session, 'formula' , 
                                      choices =  formula.names() ,
                                      selected = 1 )  
            
            updateSelectInput( session, 'dataset' ,
                                      choices = NULL ,
                                      selected = NULL ) # rds_data_file()[1] )
})

        # Update list of data files
        observe({  
            cat( '\n* updating dataset list' )
              updateSelectInput( session, 'dataset' ,
                                      choices = rds_data_file() ,
                                      selected = NULL ) # rds_data_file()[1] )
            
          })
        
        dataset.file = reactive({
          req( input$dataset )
          req( data.folder() )
          
          cat('\n* data_widget  dataset.file():', input$dataset )
          
          data.folder = isolate( data.folder() )
          file = paste0( data.folder , input$dataset  )
          
          cat('\n - ', file )
          return( file )
          
      })
        
        dataset = reactive({ 
          # req( input$dataset ) # file name from data_widget (on Dictionary tab)
          cat('\n* data_widget  dataset():')
          
          req( input$dataset  )
  
          file  = isolate( dataset.file() )

          if ( !is.null( input$dataset ) && file_test("-f",  file) ){
            
            showModal(
                modalDialog( title = "Reading data", 
                             easyClose = TRUE ,
                             size = 's' ,
                             footer=NULL
                             )
                )
              
            cat( '\n - reading selected rds file', file )
            d = readRDS( file ) 
            
            removeModal()
            
            cat('\n - done: dataset has' , nrow(d),  'rows')
 
            return( d )
          
          } else {
            cat('\n - dataset.file() not selected or not found')
          }
      })
      
        data1 = reactive({
            req( dataset.file() )
            req( dataset() )
            req( formula_elements() )
            req( ousTree() )
            cat( '\n* data_widget data1')
            
            # Testing 
              # saveRDS( dataset() , 'dataset.rds' )
              # saveRDS( formula_elements() , 'formula_elements.rds' )
              # saveRDS( ousTree() , 'ousTree.rds' )
              
            cat( '\n -  data_widget data1() class( dataset() )', class( dataset() ))
          
            if ( ! 'COUNT' %in% names( dataset() )){
              
              showModal(
                modalDialog( title = "Data is the wrong type and will not be used", 
                             easyClose = TRUE ,
                             size = 's' ,
                             footer= '(click anywhere to continue)'
                             )
                )
              return()
            } 
  
              
            if ( !'effectiveLeaf' %in% names( dataset() ) | input$rescan  ){
              
              showModal(
                modalDialog( title = "Preparing raw data for analysis.  Just a moment...", 
                             easyClose = TRUE ,
                             size = 's' ,
                             footer= '(click anywhere to continue)'
                             )
                )
              
              cat( '\n -- preparing data1')
              
              if ( input$rescan ){
                
                # revert to original download
                data = dataset() %>% as_tibble %>% ungroup %>%
                  select( dataElement.id , categoryOptionCombo.ids , orgUnit , period ,  COUNT , SUM  ) %>%
                  rename( dataElement = dataElement.id , categoryOptionCombo = categoryOptionCombo.ids ) %>%
                  mutate( COUNT = round(COUNT, 0) , SUM = round(SUM, 0) ) %>%
                  mutate_all( as.character )
                
              } else {
                data = dataset() 
              }
              
              if ( 'categoryOptionCombo.ids' %in% names( data )) data = data %>% rename( categoryOptionCombo = categoryOptionCombo.ids )
              
              # when there is no category option combo, as when downloading totals with no dissagregations, 
              # the column does not exist in download and need to add it...
              if ( ! 'categoryOptionCombo' %in% names( data )) data = data %>% 
                mutate( categoryOptionCombo = NA )
             
              if ( 'dataElement.id' %in% names( data )) data = data %>% 
                select( -dataElement ) %>%
                rename( dataElement = dataElement.id )
              
              # data = data %>%
              #   select( dataElement, categoryOptionCombo, period, orgUnit, COUNT, SUM ) 
              
              # d1 = data_1( data , formula_elements() , ousTree()  )
              d1 = data_1( data , 
                           formula_elements = formula_elements() ,
                           dataSets = dataSets() , 
                           dataElements = dataElements() , 
                           categories = categories() , 
                           ousTree = ousTree()  )
              
              cat( '\n - data1 names:', names( d1 ))
              cat( '\n - data1 rows:', nrow( d1 ))
              
              #Testing 
              # saveRDS( d1, 'd1.rds', compress = FALSE )
              
              removeModal()
              
              #### April 2024 - since file will be saved after outlier scan, no need to save here
              
              # showModal(
              #   modalDialog( title = 'Saving prepared data....', 
              #                easyClose = TRUE ,
              #                size = 's' ,
              #                footer= '(click anywhere to continue)'
              #                )
              #   )
              # 
              # 
              # # Save prepared file
              # cat('\n - saving prepared file'  )
              # saveRDS( d1, file = dataset.file() , compress = FALSE )
              # removeModal()
            
            } else {
              cat( '\n -- data1 already prepared') 
              d1 = dataset()
            }
            
          # Testing
            # saveRDS( data1 , 'data1.rds' )
            cat( '\n - d1 class:', class( d1 ))
            
            # Add value column if missing (now added in data_1 function)
            if ( ! 'value' %in% names( d1 ) ){
              
              cat('\n - data_widget adding value column')
              d1 = d1 %>% mutate( value = !is.na( SUM ) )
              # data1 = setDT( data1 )[ , value := !is.na( SUM ) ] 
            }
            
            # removeModal()
            
            # keyvars = key_vars( dataset() )
            # indexvars = index2_var( dataset() )
            # cat( '\n - dataset() index and keyvars:\n  -- ' , indexvars , "\n  -- " , keyvars )
            # 
            # d1 = as_tsibble( d1, index = {{ indexvars }} , key = {{ keyvars }} )
        
            cat( "\n - end d1  class/cols:\n -- " , class( d1 ) , "\n"  )
            
            return( d1 )
      })
      
        # dt1 = reactive({
        #   req( data1() )
        #   cat( "\n* dt1 " )
        #   cat( "\n - data1() class:" , class( data1()  ) )
        #   d1 = data1()
        #   cat( "\n - dt1 class:" , class( d1 ) )
        #   return( d1 )
        # })
      
            

# Return ####
        return( list( 
          indicator = reactive({ input$formula }) ,
          formulas = formulas ,
          formulaName =  reactive({ input$formula }) ,
          all_formula_elements = all_formula_elements ,
          formula_elements = formula_elements ,
          dataset.file = reactive({ input$dataset }) ,
          dataset =  dataset ,
          data1 = data1 
          # dt1 = dt1
            )
            )
        })
    }  


