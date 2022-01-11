directory_widget_ui = function ( id ) 
{
        ns <- NS(id)  
        
        fillCol( height = 600, flex = c(NA ) , 

         h4( 'Directory for storing/retrieving data files for this project:') ,
                 
         textInput( ns("data.directory"), label = NULL , 
                     value = "../HMIS/Formulas/" ,
                     width = '95%'
                     ) ,
         
         h4( 'Previously downloaded metadata:') ,
        
         selectInput( ns("metadataFiles") , label = NULL , 
                      width = '95%',
                      choices = NULL , 
                      selected = FALSE,
                      multiple = FALSE ,
                      selectize = FALSE, 
                      size = 4  ##needed for `selected = FALSE` to work ) 
                     ) ,
        
         h4( 'Previously downloaded data:') ,
  
         selectInput( ns("datasetFiles") , label = NULL , 
              width = '95%',
              choices = NULL , 
              selected = FALSE,
              multiple = FALSE ,
              selectize = FALSE, 
              size = 4  ##needed for `selected = FALSE` to work ) 
             ) 
        
        ) # end fillColl
          
          } # ui
        

directory_widget_server <- function( id ) {
     moduleServer(
        id,
        function(input, output, session) {

        data.folder = reactive({
                  cat( '\n**data.folder:\n' )
                  # req( input$country )
                  req( input$data.directory  )
                  data.dir = file.dir( # country = input$country , 
                                          dir.base = input$data.directory )
                  has.slash.at.end = str_locate_all( data.dir , "/") %>% 
                  unlist %in% nchar( data.dir) %>% any 
                
                  if ( !has.slash.at.end  ){ data.dir = paste0( data.dir , "/" ) }
                
                cat( '-data.folder is ', data.dir , '\n')
                return( data.dir )
        })
        
        data.dir.files = reactive({ 
            req( data.folder() )
            if ( !dir.exists( data.folder() )) return( NULL )
            dir.files = list.files( data.folder() )
            return( dir.files )
        })
        
        metadata.files = reactive({ 
          req( data.folder() )
          cat( '- looking for metadata in:' , data.folder() , '\n')
          
          dir.files = data.dir.files()
          
          file.type = '.xl' # input$file.type 
          file.other = "metadata"
          
          search.index = 
                  grepl( file.type , dir.files, ignore.case = TRUE ) &
                  grepl( file.other, dir.files, ignore.case = TRUE ) 
          
       
          if ( !any( search.index ) ){
            cat( '- no metadata files in directory \n' )
            return( NULL )
          } 
          
          mf = dir.files[ search.index ]
          
          cat( '-', length(mf) , 'metadata files \n')
           
          cat( 'mf:' , mf , '\n')
          
          return( mf )
          })
        
        rds_data_files = reactive({
          req( data.dir.files() )
          cat( '- looking for rds files in:' , data.folder() , '\n')
          
          dir.files = data.dir.files()
   
          file.type = 'rds' # input$file.type 
          file.other = "cleaned|seasonal|data"
          
          search.index = 
                  grepl( file.type , dir.files, ignore.case = TRUE ) &
                  grepl( file.other, dir.files, ignore.case = TRUE ) 
          
          data.files = paste0( data.folder(), dir.files[ search.index ] )
        
          if ( !any(file.exists( data.files ))) return( NULL )
        
          cat( "-data file exists: \n" )
          return( data.files )

})
        
        # Update list of data files
        observe({  
            cat( '-updating metadata file list \n' )
            updateSelectInput( session, 'metadataFiles' , 
                                      choices = metadata.files()  ) 
          } )
        
        observe({  
            cat( '-updating .rds file list \n' )
            updateSelectInput( session, 'datasetFiles' , 
                                      choices = rds_data_files()  ) 
          } )
        
        return( list( 
          directory = data.folder
            ))
        })
    }  


