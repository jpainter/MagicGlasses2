data_widget_ui = function ( id ) 
{
        ns <- NS(id)  
        fillCol( height = 600, flex = c(NA ) , 
        
tagList( 

      shinybusy::add_busy_spinner(
            spin = "fading-circle" , # "self-building-square",
            position = 'bottom-left'
            # , margins = c(70, 1200)
          ) ,
  
        h5('The files here are in the directory specified in the setup section') ,
        
        textOutput( ns("directory"), 
                    # label = "",
                    #  value = NULL ,
                    #  width = '95%'
                     # , style = "font-size: 50%;"
                     ) ,
        
        selectInput( ns("formula.file") , 
                     label = "List of files with names like *Formula*.xls* ):" , 
                      width = '95%',
                      choices = NULL , 
                      selected = FALSE,
                      multiple = FALSE ,
                      selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,
          
         selectizeInput( ns("indicator") , 
                      label = "Indicator:" , 
                      width = '95%',
                      choices = "" ,
                      options = list(create = TRUE) , 
                      selected = FALSE,
                      multiple = FALSE ,
                      # selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,
        
        textInput( ns("file.keywords"), "key words for searching data files" ,
                   # value = '_formulaData|Seasonal|dts|rds' 
                   value = 'Seasonal|dts|rds' ) ,
          
        selectInput( ns("dataset") , 
                     label = "Data previously downloaded from DHIS2:" , 
              width = '95%',
              choices = NULL , 
              selected = FALSE,
              multiple = FALSE ,
              selectize = FALSE, 
              size = 4  ##needed for `selected = FALSE` to work ) 
             ) , 

)       
        ) # end fillColl
          
          } # ui
        

data_widget_server <- function( id ,
                                metadata_widget_output = NULL,
                                directory_widget_output = NULL
                                # , data_request_output = 0 
                                ) {
     moduleServer(
        id,
        function(input, output, session 
                 ) {
          
       # Reactive dependecies
      data.folder = reactive({ directory_widget_output$directory() })
      ousTree = reactive({ metadata_widget_output$ousTree() })
      
      # completedRequest = reactive({ data_request_output$completedRequest() })
        
        formula.files = reactive({ 
          req( data.folder() )
          cat( '\n* looking for formula files in' , data.folder() , '\n')
        
          ff = files( search = 'Formulas_' , dir = data.folder() , type = 'xlsx|rds' )  
          if ( is_empty( ff ) ){
            cat( '\n - no forumula files in directory' )
            return( )
          } 
          
          # Arrange by modified date
          formula_file.mdate = file.info( paste0( data.folder() , ff  ) )$mtime
          ff = ff[ rev(order( formula_file.mdate )) ]
          
          cat( '\n - formula.files:' , ff  )
          if ( !any(file.exists( paste0( data.folder() , ff  ) ) )) return()
          
          return( ff )
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
          req( input$formula.file )
          cat( '\n* formulas:' )
          
          file = paste0( data.folder() , input$formula.file )
          cat( '\n - formula file:' , file )
          
          if ( !any(file.exists( file ) )) return( NULL )
          
          if ( grepl( fixed('.xlsx'), file )  ){
            cat( '\n - read xls file')
            formulas = read_excel( file , sheet = 'Formula') %>% 
              filter( !is.na(Formula.Name)) %>%
              arrange( desc( Formula.Name ) ) 
          } else {
            cat( '\n - read rds file')
            formulas = readRDS( file )
          }
          
          cat( '\n - formula.Name:' , unique( formulas$Formula.Name ) )
          
          return( formulas )
        })
        
        formula.names = reactive({ formulas()$Formula.Name })
        
        formula_elements =  reactive({
          req( input$formula.file )
          req( input$indicator )
          cat( '\n* formula_elements:')
          
          file = paste0( data.folder() , input$formula.file )
          cat( '\n - formula file' , input$formula.file )
          
          if ( grepl( fixed('.xlsx'), file ) ){
          cat( '\n - read xls file' )
            formulas = read_excel( file , sheet = 'Formula Elements')  %>%
            filter( Formula.Name %in% input$indicator ) 
          
            } else{
            cat( '\n - read rds file')
            formulas = readRDS( file ) %>% filter( Formula.Name %in% input$indicator )
          }
        
        })
        
        data.dir_files = reactive({ 
            # req( completedRequest() )
            req( input$formula.file )
            if ( !dir.exists( data.folder() ) ) return( NULL )
            # trigger  update when there is a data request finishes
            # update = completedRequest() > 0  
            # cat( '\n data.dir_files completedRequest:' , completedRequest() )
            dir.files = list.files( data.folder()  )
            cat( "\n number of dir.files :", length(dir.files) ) 
            return( dir.files )
        })
        
        rds_data_file = reactive({
          req( data.dir_files() )
          req( data.folder() )
          req( input$indicator )
            
          dir.files = data.dir_files()
        
          indicator = paste0( input$indicator , "_" )
          cat( '\n* rds_data_file indicator: ' , input$indicator , '\n' )
          
          file.type = 'rds' # input$file.type 
          # file.other = ifelse( input$cleaned %in% "Cleaned" , '_Seasonal' , "" )  # input$file.other
          
          file.keywords = input$file.keywords # '_formulaData|Seasonal|dts|rds'
          
          data.files = dir.files[ 
                  # grepl( 'All levels' , dir.files ) &
                  grepl( file.type , dir.files) &
                  # grepl( file.other, dir.files, fixed = TRUE  ) &
                  grepl( file.keywords, dir.files, ignore.case = T ) ]
        
          # cat('\nall levels data files:' , data.files )
          
          f.indicator = grepl( indicator , data.files , fixed = TRUE )
          
          cat("\n f.indicator:" , f.indicator ) 
          
          if ( sum( f.indicator ) == 0 ){
            cat( '\n - no data files for this indicator' )
            return( NULL )
          } 
          
          if ( !dir.exists( data.folder() )){
            cat( '\n - no folder matching data.folder()' )
            return( NULL )
          } 
          
          data_files = data.files[f.indicator] # %>% most_recent_file()
          
          # cat( '\n data_files are:\n' , data_files )
          
          # Arrange by modified date
          data_file.mdate = file.info( paste0( data.folder() , data_files ) )$mtime
          data_files = data_files[ rev(order( data_file.mdate )) ]
          
          cat( '\n - done:' , length( data_files ) , 'files')
          return( data_files )

})


        # update indicators 
        observe({  
            cat( '\n* updating indicator list' )
            updateSelectInput( session, 'indicator' , 
                                      choices =  formula.names() ,
                                      selected = 1 )  
            
            updateSelectInput( session, 'dataset' ,
                                      choices = NULL ,
                                      selected = NULL ) # rds_data_file()[1] )
})

        # Update list of data files
        observe({  
            cat( '\n updating dataset list' )
            if ( !is.null( rds_data_file() )){
              updateSelectInput( session, 'dataset' ,
                                      choices = rds_data_file() ,
                                      selected = NULL ) # rds_data_file()[1] )
            }
            
          })
        
      dataset = reactive({ 
          # req( input$dataset ) # file name from data_widget (on Dictionary tab)
          cat('\n* cleaning_widget  dataset():')
          
          file = paste0( data.folder() , input$dataset  )
          cat('\n - ', file )
          
          if ( file_test("-f",  file) ){
            d = readRDS( file ) 
            cat('\n - dataset has' , nrow(d),  'rows')
            
     
            # updated( updated() + 1 )
            return( d )
          } else {
            cat('\n - dataset.file() not selected or not found')
          }
      })
      
      data1 = reactive({
          req( dataset() )
          req( formula_elements() )
          req( ousTree() )
          
            cat( '\n* preparing data1')
            data1 = data_1( dataset() , formula_elements() , ousTree()  )
            cat( '\n - data1 names:', names( data1 ))
            cat( '\n - data1 rows:', nrow( data1 ))
       
            # Testing 
            saveRDS( data1 , 'data1.rds' )
      
            return( data1 )
      })
            

        return( list( 
          indicator = reactive({ input$indicator }) ,
          formulas = formulas ,
          formulaName =  reactive({ input$indicator }) ,
          formula_elements = formula_elements ,
          dataset.file = reactive({ input$dataset }) ,
          dataset =  dataset ,
          data1 = data1
            )
            )
        })
    }  


