
formula_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fillCol( height = 600, flex = c(NA ) ,
           
  tagList(
    
    add_busy_spinner(spin = "fading-circle", 
                   position = "top-right") ,


    tabsetPanel(type = "tabs",


                tabPanel("Formula Elements", 
                         actionButton( ns("removeSelected") , 
                                                     "Remove Selected Elements" , style='margin-top:25px' 
                                                 ) ,
                         wellPanel(
                           
                           verbatimTextOutput( ns("formulaName") ),
                           
                           DTOutput( ns('forumlaDictionaryTable') ) 
                         )
                         ) ,
                
                tabPanel("All Elements", 
                         actionButton( ns("addSelected") , 
                                                     "Add Selected Elements" , style='margin-top:25px' 
                                                 ) ,
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
                                    metadata_widget_output = NULL ,
                                   data_Widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {
      
  # cat('\n **** Starting formula_widget_server \n')
      
  # reactives to toggle login status
  dataElementDictionary = reactive({ metadata_widget_output$dataElements() })
  categories = reactive({ metadata_widget_output$categories() })
  formula_elements = reactive({ data_Widget_output$formula_elements() })
  formulaName = reactive({ data_Widget_output$formulaName() })


## All data Elements ####
  

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
  
  selected_elements = reactive({
    
    row_selected = input$dataElementDictionaryTable_rows_selected 
    
    if (length(row_selected)) {
      cat('These rows were selected:' )
      cat( row_selected , sep = ', ')
      selected = dataElementDictionary() %>% 
             filter( row_number() %in% row_selected ) 
      return( selected )
    } else {
      return()
    }
    
  })
  
  
  ## Formula data Elements ####
  
  
  
  updated_formula_elements = reactive({
    req( formula_elements )
    cat( '\n* updated_formula_elements starting with', nrow(formula_elements()) , 'elements')
    
    ufe = formula_elements()
    
    if ( !is_empty( selected_elements() ) ){
      
      selected_categories = selected_elements %>%
        separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
        mutate( Categories = Categories %>% str_trim  ,
                categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim )
      
      cat('\n - adding' , nrow( selected_categories ), 'selected elements' )
      
      ufe = bind_rows( ufe , selected_categories ) %>%
        arrange( dataElement )
      
    }
    
    cat( '\n - done')
    return( ufe )
    
  })
  
  output$forumlaDictionaryTable = 
    DT::renderDT(DT::datatable(
      
      updated_formula_elements()   ,
      rownames = FALSE, 
      filter = 'top' ,
      options = DToptions_no_buttons()
    ))
  
  output$formula = renderPrint({
    r = input$formulaName
    if (length(r)) {
      cat('Formula name:' )
    }
    
  })
  
  
  
  


# Return ####
  return( list( 
    updated_formula_elements = updated_formula_elements
                
                ) )
    
    }
)}

