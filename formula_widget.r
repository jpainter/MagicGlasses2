
formula_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fillCol( height = 600, flex = c(NA ) ,
           
  tagList(
    
    add_busy_spinner(spin = "fading-circle", 
                   position = "top-right") ,


    tabsetPanel(type = "tabs",


                tabPanel("dataElements", 
                         wellPanel(
                           
                           verbatimTextOutput( ns("selected") ),
                           
                           DTOutput( ns('dataElementDictionaryTable') ) 
                         )
                         ) 


                ) 
    )
    
    
  )  # end fillColl
          
          } # ui

# Server function ####
formula_widget_server <- function( id , 
                                    metadata_widget_output = NULL  ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {
      
  # cat('\n **** Starting formula_widget_server \n')
      
  # reactives to toggle login status
  dataElementDictionary = reactive({ metadata_widget_output$dataElements() })
  categories = reactive({ metadata_widget_output$categories() })


## data Elements ####

  output$dataElementDictionaryTable = 
    DT::renderDT(DT::datatable(
   
    dataElementDictionary()   ,
    selection = 'multiple' ,
    rownames = FALSE, 
    filter = 'top' ,
    options = DToptions_no_buttons()
  ))
  
  output$selected = renderPrint({
    r = input$dataElementDictionaryTable_rows_selected
    if (length(r)) {
      cat('These rows were selected:' )
      cat(r , sep = ', ')
      cat('\n') 
      cat( dataElementDictionary() %>% 
             filter( row_number() %in% r ) %>% 
             pull( dataElement ), 
           sep = ', ')
    }
    
  })
  


# Return ####
  return( list( 
                formulaElements = dataElementDictionary 
                ) )
    
    }
)}

