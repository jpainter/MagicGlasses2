
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
                        # column( 5 , 
                        #  actionButton( ns("removeSelected") , 
                        #                              "Remove Selected Elements" , style='margin-top:25px' 
                        #                          ) 
                        #  ) ,
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
                         
                         # wellPanel(
                           h5("selected elements:") ,
                           verbatimTextOutput( ns("selected") ),
                           
                           DTOutput( ns('dataElementDictionaryTable') ) 
                         # )
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
  formulaElements = reactive({ data_Widget_output$formulaElements() })
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
      # options = DToptions_no_buttons()
      options = list(
        # bPaginate = FALSE, 
        scrollY = "40vh"  ,
        searching = TRUE, 
        info = FALSE,
        lengthMenu = list( c( -1, 1, 5, 10, 25, 100 ), 
                           list( 'All' , '1', '5' , '10', '25', '100') ) ,
        server = TRUE ),
      fillContainer = FALSE
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
      cat( selectedElementNames() )
  })
  
  
  
## Formula data Elements ####
  hasFormula = reactiveValues( formulas = FALSE )
  
  observeEvent( formula_elements() , { 
    cat( '\n* update hasFormula' )
    if ( nrow( formula_elements() ) > 0  ){
      cat('\n - nrow( formula_elements() )', nrow( formula_elements() ))
      hasFormula$formulas <- TRUE 
      }
    } )
  
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
    
    if( !hasFormula$formulas  ){
      cat( '\n- !hasFormula$formulas')
      saveRDS( dataElementDictionary()[0, ]  , 'emptyDataDictionary.rds')
      ufe = dataElementDictionary()[0, ] 
      
    } else {
      cat( '\n- hasFormula$formulas')
      saveRDS( formula_elements()  , 'formula_elements.rds')
      ufe =  formula_elements()
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
   
    
      if ( nrow( ufe ) == 0 ){
        cat( '\n- ufe == 0')
        ufe = selected_categories  %>%
        arrange( dataElement ) %>%
        select( Formula.Name, everything() )
                
      } else {
        cat( '\n- ufe > 0')
        ufe = bind_rows( ufe , selected_categories ) %>%
        arrange( dataElement ) %>%
        select( Formula.Name, everything() )
      }
      

      # TESTING save changes
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
      # filter = 'top' ,
      options = list(
        # bPaginate = FALSE, 
        scrollY = "50vh"  ,
        info = TRUE ,
        lengthMenu = list( c( -1, 1, 5, 10, 25, 100 ), 
                           list( 'All' , '1', '5' , '10', '25', '100') ) ,
        server = TRUE ),
      fillContainer = TRUE)
      # options = DToptions_no_buttons()
    )
  
  output$formulaName = renderPrint({ formulaName() })

# Save Formula ####
  output$saveFormula <- downloadHandler(
   
    filename = function() {
      paste0( dir() , "Formulas_", Sys.Date()  ,".xlsx"  )
    } ,
    
    content = function( file ) {
      cat('\n* saving formula')
 
      wb <- openxlsx::createWorkbook()

      sheet1  <- addWorksheet( wb, sheetName = "Formula" )
      sheet2  <- addWorksheet( wb, sheetName = "Formula Elements" )
      
      if ( ! hasFormula$formulas ){
        
        cat('\n - preparing new formula')
        new.Formula.Name = formulaName() 
        new.Formula = selectedElementNames() 
        new.formula_elements = updated_formula_elements() 
        
      } else {
        
        cat('\n - adding new formula')
        origninal_formula = formulas() %>%
            filter( ! Formula.Name %in%  formulaName() )
      
        cat('\n* orignal formulas had', nrow( formulas()), 'formulas')
        cat('\n - new formula in original?', formulaName() %in% formulas()$Formula.Name )
      
        orginal_formula_elements = formulaElements() %>%
        filter( ! Formula.Name %in%  formulaName() )
        cat('\n* orignal formulas elements had', nrow( formulaElements()), 'rows')
      
        new.Formula.Name = c( formulaName() , origninal_formula$Formula.Name )
        new.Formula = c( selectedElementNames() , origninal_formula$Formula  )
        new.formula_elements = rbind( updated_formula_elements() ,
                                      orginal_formula_elements 
                                        ) 
      }
      
      cat('\n - writing sheet1' )
      writeDataTable( wb, sheet1, tibble( Formula.Name = new.Formula.Name ,
                                          Elements = new.Formula
                                             ) , rowNames = FALSE )
      
      cat('\n - writing sheet2' )
      writeDataTable( wb, sheet2, new.formula_elements , 
                      rowNames = FALSE )

      openxlsx::saveWorkbook( wb , file , overwrite = TRUE )
     }
  )
  
# Return ####
  return( list( 
    updated_formula_elements = updated_formula_elements
                
                ) )
    
    }
)}

