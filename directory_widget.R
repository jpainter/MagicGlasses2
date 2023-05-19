directory_widget_ui = function ( id ) 
{
        ns <- NS(id)  
        
        fillCol( height = "100%", flex = c(NA ) , 

        
         
         # shinyDirButton( ns('folder') , 'Folder select', 'Please select a folder', FALSE) ,
         
         # br(),
         
         tags$blockquote(
           h4('MagicGlasses: an epidemiological look at DHIS2') ,
           br() , br() ,
           "The goal is to make the analysis of routine data accessible, transparent, and repeatable." ,
           br() , br() ,
           "The layout follows a research path using the pages (at top of page) to
           understanding which data are available " ,
           tags$b("(Metadata)"), 
           "requesting data", 
           tags$b( "(Data)" ), 
           "adjusting for reporting bias " ,
           tags$b( "(Reporting)" ) ,
           ", scanning for outliers " ,
           tags$b( "(Outliers)" ), 
           "applying time-series models and estimating intervention effectiveness ",
           tags$b( "(Evaluation)" ) 
           
  
         ) ,
         
         hr() ,
         
        h3( 'Directory for data files:') ,
          
         textInput( ns("data.directory"), label = NULL ,
                     value = path.expand("~") ,
                     width = '95%'
                     ) ,
         
         # selectInput(ns("data.directory"), label = NULL ,
         #             width = '95%',
         #              choices = NULL , 
         #              selected = NULL ,
         #              multiple = FALSE ,
         #              selectize = FALSE, 
         #              size = 4  ##needed for `selected = FALSE` to work ) 
         #             ) ,
         
         
         
         h4( 'Folder Contains:') ,
        
         # selectInput( ns("metadataFiles") , label = NULL , 
         #              width = '95%',
         #              choices = NULL , 
         #              selected = NULL ,
         #              multiple = FALSE ,
         #              selectize = FALSE, 
         #              size = 4  ##needed for `selected = FALSE` to work ) 
         #             ) ,
         
         # shiny::dataTableOutput( ns('folderInfo') ) , 
        tableOutput( ns('folderInfo') ) , 
         
         hr()
         
        
        ) # end fillColl
          
          } # ui
        

directory_widget_server <- function( id ) {
     moduleServer(
        id,
        function(input, output, session) {
          
        ns <- session$ns  # for shinyDirChoose (https://stackoverflow.com/questions/38747129/how-to-use-shinyfiles-package-within-shiny-modules-namespace-issue/)
  
        observe({
         isolate(
           if ( dir.exists( "../HMIS/Formulas/" ) ){
              cat( '\n directory_widget -setting JP data.directory to\n' ,
                   "../HMIS/Formulas/" )
              updateTextInput( session, "data.directory" ,
                               value = "../HMIS/Formulas/" )
             
             # updateSelectInput( session, "data.directory" ,
             #                    choices  = "../HMIS/Formulas/" ,
             #                    selected  = "../HMIS/Formulas/"
             #                    )
           }
         )
        })
          
        observe({
            isolate(
              if ( dir.exists( "~/_Malaria/Projects/HMIS/Formulas" )){
                cat( '\n directory_widget -setting JP data.directory to:\n' ,
                     "~/OneDrive - CDC/_Malaria/Projects/HMIS/Formulas" )
                updateTextInput( session, "data.directory" ,
                                 value = "~/_Malaria/Projects/HMIS/Formulas/" )
              }
            )
          } )
       
        shinyDirChoose( input , id=ns("folder"), session = session ,
                          roots = c( home = path.expand("~") )  ,
                          filetypes=c('', 'txt')
                          )
        
         
       data.folder = reactive({
                  cat( '\n* data.folder:\n' )
                  # req( input$country )
                  req( input$data.directory  )
                  data.dir = file.dir( # country = input$country ,
                                          dir.base = input$data.directory )
                  has.slash.at.end = str_locate_all( data.dir , "/") %>%
                  unlist %in% nchar( data.dir) %>% any

                  if ( !has.slash.at.end  ){
                    OS <- .Platform$OS.type
                    cat('\n - OS is' , OS )
                    if (OS == "unix"){
                      data.dir = paste0( data.dir , "/" )
                    } else if (OS == "windows"){
                      data.dir = paste0( data.dir , "/" )
                    } else {
                      message("ERROR: OS could not be identified")
                    }
                }

              cat( '\n - data.folder is ', data.dir , '\n')
              return( data.dir )
        })
        
        data.dir.files = reactive({ 
            req( data.folder() )
            if ( !dir.exists( data.folder() )) return( NULL )
            dir.files = list.files( data.folder() )
            return( dir.files )
        })
        
    folderInfo = reactive({
    
        req( data.dir.files() )
          # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
          # if available, use resources method
          cat( '\n folder info : \n')
          
         finf <- file.info( dir(data.folder()) , extra_cols = FALSE)
        
         metadata.files = grepl( "metadata" , data.dir.files() , ignore.case = TRUE ) 
         formula.files = grepl( "formulas" , data.dir.files()  , ignore.case = TRUE ) 
         data.files = grepl( "rds" , data.dir.files()  , ignore.case = TRUE ) 
         
         info = tibble( `File type:` = c("Metadata" , 'Formula', 'Data') ,
                        Number = c( sum(metadata.files), sum(formula.files) , sum(data.files) ) ,
                        `Most Recent` = c( max( finf$mtime[ metadata.files ], na.rm = T ) ,
                                           max( finf$mtime[ formula.files ] , na.rm = T) ,
                                           max( finf$mtime[ data.files ] , na.rm = T )
                        )
                        )
          
          cat( '\n directory widget folderInfo completed')
          return( info )
          
    
      }) 
    
      output$folderInfo =  renderTable( folderInfo() ) 
    
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
          
           # Arrange by modified date
          mf.mdate = file.info( paste0( data.folder() , mf ) )$mtime
          mf = mf[ rev(order( mf.mdate )) ]
          
          cat( '-', length(mf) , 'metadata files \n')
           
          cat( 'mf:' , mf , '\n')
          
          return( mf )
          })
        
        geofeatures.files = reactive({ 
          req( data.folder() )
          cat( '\n* geofeatures.files: looking for geoFeatures in:' , data.folder() , '\n')
          
          dir.files = data.dir.files()
          
          file.type = '.rds' # input$file.type 
          file.other = "geofeatures"
          
          search.index = 
                  grepl( file.type , dir.files, ignore.case = TRUE ) &
                  grepl( file.other, dir.files, ignore.case = TRUE ) 
          
       
          if ( !any( search.index ) ){
            cat( '- no geoFeatures files in directory \n' )
            return( NULL )
          } 
          
          gf = dir.files[ search.index ]
          
          # Arrange by modified date
          gf.mdate = file.info( paste0( data.folder() , gf ) )$mtime
          gf = gf[ rev(order( gf.mdate )) ]
          
          cat( '-', length(gf) , 'geofeatures files \n')
           
          cat( 'gf:' , gf , '\n')
          
          return( gf )
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
            cat( '\n directory_widget -updating metadata file list \n' )
            updateSelectInput( session, 
                               'metadataFiles' ,
                               choices = metadata.files() ,
                               selected = 1 ) 
          } )
        
        observe({  
            cat( '\n directory_widget -updating geofeatures file list \n' )
            updateSelectInput( session, 'geofeturesFiles' , 
                                      choices = geofeatures.files()  ) 
          } )
        
        observe({  
            cat( '\n directory_widget -updating .rds file list \n' )
            updateSelectInput( session, 'datasetFiles' , 
                                      choices = rds_data_files()  ) 
          } )
        
        return( list( 
          directory = data.folder ,
          metadata.files = metadata.files ,
          geofeatures.files = geofeatures.files
            ))
        })
    }  


