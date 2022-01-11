cleaning_widget_ui = function ( id ) 
{
        ns <- NS(id)  
        fillCol( height = 600, flex = c(NA  ) , 

        
        tableOutput( ns("dqaTable") )
        
        ) # end fillColl
          
          } # ui
        

cleaning_widget_server <- function( id, data  ) {
    moduleServer(
        id,
        function(input, output, session) {

    
  # print('Dataset:'); print( class( data ) )

#   observeEvent( d() , { output$dqaTable = renderTable( d() ) } ,
#                 ignoreNULL = TRUE , ignoreInit = TRUE
# )
  output$dqaTable = renderTable( head( data() ) )
        })
    }  


