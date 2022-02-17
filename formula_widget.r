
formula_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fillCol( height = 600, flex = c(NA ) ,
           
  tagList(
    
    add_busy_spinner(spin = "fading-circle", 
                   position = "top-right") ,


    tabsetPanel(type = "tabs",


                tabPanel("Formula Elements", 
                         
                      fluidRow(
                        column( 5 , 
                         actionButton( ns("removeSelected") , 
                                                     "Remove Selected Elements" , style='margin-top:25px' 
                                                 ) 
                         ) ,
                        column( 5 , 
                         downloadButton( ns( 'saveFormula' ), 'Save Formula') 
                        )
                        ) ,
 
                           DTOutput( ns('forumlaDictionaryTable') ) 
                         ) ,
                
                tabPanel("All Elements", 
                         # actionButton( ns("addSelected") , 
                         #                             "Add Selected Elements" , style='margin-top:25px' 
                         #                         ) ,
                         
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
                                   data_Widget_output = NULL ,
                                   directory_widget_output = NULL ){
  moduleServer(
    id ,
    function( input, output, session 
              ) {
      
  # cat('\n **** Starting formula_widget_server \n')
      
  # reactives to toggle login status
  dataElementDictionary = reactive({ metadata_widget_output$dataElements() })
  categories = reactive({ metadata_widget_output$categories() })
  formulas = reactive({ data_Widget_output$formulas() })
  formula_elements = reactive({ data_Widget_output$formula_elements() })
  formulaName = reactive({ data_Widget_output$formulaName() })
  dir = reactive({ directory_widget_output$directory() })

## All data Elements ####
  

  output$dataElementDictionaryTable = 
    DT::renderDT( DT::datatable(
   
      dataElementDictionary()   ,
      selection = 'multiple' ,
      rownames = FALSE, 
      filter = 'top' ,
      options = DToptions_no_buttons()
    )
    )
  
  selected_elements = reactive({
    # req( input$dataElementDictionaryTable_rows_selected  )
    
    # cat('\n* formula_widget selected_elements():')
    row_selected = input$dataElementDictionaryTable_rows_selected 
    # cat('\n - row number selected is' ,  row_selected )
    
    # if ( length(row_selected) ) {ÇÇ
    selected = dataElementDictionary() %>% 
               filter( row_number() %in% row_selected ) 
      
    
    return( selected )
    # } else {
    #   return()
    # }
    
  })
  
  selectedElementNames = reactive({
    # cat('\n* selectedElementNames')
    
    selected_categories = selected_elements() %>%
        separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
        mutate( Categories = Categories %>% str_trim ,
                dataElement = dataElement %>% str_trim )
    
    
    a = selected_categories  %>% pull( dataElement ) 
    b = selected_categories  %>% pull( Categories ) 
    
    a. = paste0("[", format(unlist(a)),"]")
    b. = paste0("[", format(unlist(b)),"]")
    
    x = paste( a. , b. , sep = '.', collapse = ' + ') 
    
    # cat('\n - done ')
    return( x )
  })
  
  output$selected = renderPrint({
      # req( selectedElementNames() )
      cat('Selected elements:\n' , selectedElementNames() )
  })
  
  
  
## Formula data Elements ####
  hasFormula = reactiveValues( formulas = FALSE )
  
  observeEvent( formula_elements() , { hasFormula$formulas <- TRUE })
  
  originalFormula = reactive({  # the formula as read from disc
    cat( '\n* originalFormula:',   )
    
    fe =  if( is_empty( formulas() )  ){
      
        dataElementDictionary()[0, ] 

    } else {
      formulas()
    }
    
    cat( '\n - ' , unique( fe ) , 'elements')
    return( fe )
    })
    
  updated_formula_elements = reactive({
    # req( formula_elements )
    cat( '\n* updated_formula_elements starting with formula:',  hasFormula$formulas )
    
    ufe =  if( !hasFormula$formulas  ){
      dataElementDictionary()[0, ] 
    } else {
      formula_elements()
    }
    
    cat('\n - updated_formula_elements has' , nrow( ufe ) , 'rows')
    
    if ( nrow( selected_elements() ) > 0 ){

      selected_categories = selected_elements() %>%
        separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
        mutate( Categories = Categories %>% str_trim  ,
                categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim ,
                Formula.Name = formulaName()
                # ,zeroIsSignificant = as.logical( zeroIsSignificant )
                )

      cat('\n - adding' , nrow( selected_categories ), 'selected elements' )
   
    
      ufe = bind_rows( ufe , selected_categories ) %>%
        arrange( dataElement ) %>%
        select( Formula.Name, everything() )

      # save changes
      saveRDS( ufe %>% fill( Formula.Name, .direction =  "downup" )  ,
               paste0( 'Formula_' , Sys.Date() , ".rds" )  )

    }
    
    cat( '\n - done')
    return( ufe )
    
  })
  
  output$forumlaDictionaryTable = 
    DT::renderDT( DT::datatable(
      
      updated_formula_elements()   ,
      rownames = FALSE, 
      filter = 'top' ,
      options = DToptions_no_buttons()
    ))
  
  output$formulaName = renderPrint({ formulaName() })

# Save Formula ####
  output$saveFormula <- downloadHandler(

    filename = function() {
      paste0( dir() , "Formulas_", Sys.Date()  ,".xlsx"  )
    } ,
    
    content = function( file ) {

      wb <- openxlsx::createWorkbook()

      sheet1  <- addWorksheet( wb, sheetName = "Formula")
      sheet2  <- addWorksheet( wb, sheetName = "Formula Elements")

      writeDataTable( wb, sheet1, tibble( Formula.Name = formulaName() ,
                                             Elements = selectedElementNames()
                                             ) , rowNames = FALSE )
      writeDataTable( wb, sheet2, updated_formula_elements() , rowNames = FALSE )

      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )
     }
  )
  
# Return ####
  return( list( 
    updated_formula_elements = updated_formula_elements
                
                ) )
    
    }
)}

