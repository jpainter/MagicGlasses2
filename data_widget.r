data_widget_ui = function ( id ) 
{
        ns <- NS(id)  
        fillCol( height = 600, flex = c(NA ) , 

        h5('The files here are in the directory specified in the setup section') ,
        
        textOutput( ns("directory"), 
                    # label = "",
                    #  value = NULL ,
                    #  width = '95%'
                     # , style = "font-size: 50%;"
                     ) ,
        
        selectInput( ns("formula.file") , label = "List of files with names like *Formula*.xls* ):" , 
                      width = '95%',
                      choices = NULL , 
                      selected = FALSE,
                      multiple = FALSE ,
                      selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,
          
         selectInput( ns("indicator") , label = "Indicator:" , 
                      width = '95%',
                      choices = NULL , 
                      selected = FALSE,
                      multiple = FALSE ,
                      selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,
          
        selectInput( ns("dataset") , label = "Data previously downloaded from DHIS2:" , 
              width = '95%',
              choices = NULL , 
              selected = FALSE,
              multiple = FALSE ,
              selectize = FALSE, 
              size = 4  ##needed for `selected = FALSE` to work ) 
             ) , 

        
        ) # end fillColl
          
          } # ui
        

data_widget_server <- function( id ,
                                dataDir = NULL,
                                data_request_output = 0 ) {
     moduleServer(
        id,
        function(input, output, session ,
                 dataDirectory = dataDir ,
                 data_request_update = data_request_output 
                 ) {

        data.folder = reactive({ dataDirectory$directory() })
        completedRequest = reactive({ data_request_output$completedRequest() })
        
        formula.files = reactive({ 
          req( data.folder() )
          cat( '\n*looking for formula files in' , data.folder() , '\n')
        
          ff = files( search = 'Formula' , dir = data.folder() )  
          if ( is_empty( ff ) ){
            print( 'no forumula files in directory' )
            return( )
          } 
          
          # Arrange by modified date
          formula_file.mdate = file.info( paste0( data.folder() , ff  ) )$mtime
          ff = ff[ rev(order( formula_file.mdate )) ]
          
          print( 'formula.files:' ) ; print( ff )
          if ( !any(file.exists( paste0( data.folder() , ff  ) ) )) return()
          return( ff )
          })
        
        observe({
          cat('updating directory \n')
          # updateTextOutput( session, 'directory' , value = data.folder()  ) 
          output$directory = renderText({ data.folder() })
        })
        
        observe({
          cat('\n updating formula.files input ')
          updateSelectInput( session, 'formula.file' , 
                                      choices = formula.files() , 
                                      selected = 1  ) 
        })
        
        formulas =  reactive({
          req( input$formula.file )
        
          cat( '\n formula file:' , input$formula.file )
          
          file = paste0( data.folder() , input$formula.file )
          
          if ( !any(file.exists( file ) )) return( NULL )
          
          formulas = read_excel( file , sheet = 'Formula') %>% 
            filter( !is.na(Formula.Name)) %>%
            arrange( desc( Formula.Name ) )
          
          cat( '\n formula.Name:' , formulas$Formula.Name )
          
          return( formulas )
        })
        
        formula.names = reactive({ formulas()$Formula.Name })
        
        formula_elements =  reactive({
          req( input$formula.file )
          req( input$indicator )
          
          read_excel( paste0( data.folder() , input$formula.file ) , sheet = 'Formula Elements')  %>%
            filter( Formula.Name %in% input$indicator )
        
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
          cat( '\n rds_data_file indicator: ' , input$indicator , '\n' )
          
          file.type = 'rds' # input$file.type 
          # file.other = ifelse( input$cleaned %in% "Cleaned" , '_Seasonal' , "" )  # input$file.other
          file.label = '_formulaData' # '_formulaData|Seasonal|dts'
          
          data.files = dir.files[ 
                  # grepl( 'All levels' , dir.files ) &
                  grepl( file.type , dir.files) &
                  # grepl( file.other, dir.files, fixed = TRUE  ) &
                  !grepl( file.label, dir.files, ignore.case = T ) ]
        
          # cat('\nall levels data files:' , data.files )
          
          f.indicator = grepl( indicator , data.files , fixed = TRUE )
          
          cat("\n f.indicator:" , f.indicator ) 
          
          if ( sum( f.indicator ) == 0 ) return( NULL )
          
          if ( !dir.exists( data.folder() )) return( NULL )
          
          data_files = data.files[f.indicator] # %>% most_recent_file()
          
          cat( '\n data_files are:\n' , data_files )
          
          # Arrange by modified date
          data_file.mdate = file.info( paste0( data.folder() , data_files ) )$mtime
          data_files = data_files[ rev(order( data_file.mdate )) ]
          return( data_files )

})


        # update indicators 
        observe({  
            cat( '\n updating indicator list' )
            updateSelectInput( session, 'indicator' , 
                                      choices =  formula.names() ,
                                      selected = 1 ) } )

        # Update list of data files
        observe({  
            cat( '\n updating dataset list' )
            updateSelectInput( session, 'dataset' , 
                                      choices = rds_data_file() , 
                                      selected = rds_data_file()[1] ) 
          } )
            

        return( list( 
          dataset = reactive({ input$dataset }) ,
          indicator = reactive({ input$indicator }) ,
          formulas = formulas ,
          formula_elements = formula_elements 
            )
            )
        })
    }  


