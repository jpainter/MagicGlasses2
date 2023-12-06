
formula_widget_ui <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # fillCol( height = 600, flex = c(NA ) ,
           
  tagList(
    
    # add_busy_spinner(spin = "fading-circle", 
    #                position = "top-right") ,
    
    fluidPage(


    tabsetPanel(type = "tabs",


                tabPanel("List of Selected Elements", 
                         
                      # fluidRow(
                        # column( 5 , 
                        #  actionButton( ns("removeSelected") , 
                        #                              "Remove Selected Elements" , style='margin-top:25px' 
                        #                          ) 
                        #  ) ,
                        # column( 3 , 
                         downloadButton( ns( 'saveFormula' ), 'Save Formula') 
                        # ) 
                        ,
                        
                        # column( 3 , 
                         actionButton( ns( 'deleteRows' ), 'Delete Selected Rows') 
                        # )
                        # ) 
                      ,
 
                           # DTOutput( ns('forumlaDictionaryTable') ) 
                      
                      div(DT::dataTableOutput( ns('forumlaDictionaryTable') ), 
                          style = "font-size: 75%; width: 100%"
                      )
                         ) ,
                
                tabPanel("Select from all Elements/Indicators", 
                         # actionButton( ns("addSelected") , 
                         #                             "Add Selected Elements" , style='margin-top:25px' 
                         #                         ) ,
                         
                         # wellPanel(
                           h5("selected elements:") ,
                         
                           verbatimTextOutput( ns("selected") ),
                           
                           # DTOutput( ns('dataElementDictionaryTable') ) 
                         
                          selectInput( ns('element_indicator_choice'), "" ,
                                       choices = c( 'data elements' , 'indicators' ),
                                       selected = 'data elements'
                                       ) ,
                         
                           div(DT::dataTableOutput(  ns('dataElementDictionaryTable') ), 
                               style = "font-size: 65%; width: 100%"
                               )
                         
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
  indicators = reactive({ metadata_widget_output$indicators() })
  categories = reactive({ metadata_widget_output$categories() })
  formulas = reactive({ data_Widget_output$formulas() })
  all_formula_elements = reactive({ data_Widget_output$all_formula_elements() })
  formula_elements = reactive({ data_Widget_output$formula_elements() })
  formulaName = reactive({ data_Widget_output$formulaName() })
  dir = reactive({ directory_widget_output$directory() })

## Browse and Select Elements/Indicators Here ####
  formulaChoices = reactive({
    req(  dataElementDictionary()  )
    
    cat( '\n * formulaChoices ')
    
    if ( input$element_indicator_choice %in%  'data elements' ){ 
      
      return( dataElementDictionary() ) 
      
    } else {
      # testing:
      # saveRDS( indicators()[1:5, ], "indicators.rds" )
      # saveRDS( dataElementDictionary()[0, ] , "dataElementDictionary.rds" )
      
      # indicators() %>% bind_cols( dataElementDictionary()[0,  c( "Categories" , "categoryOptionCombo.ids" ) ] )
      
      indicators() %>% 
          mutate( dataElement.id = id , dataElement = name ) %>% 
          full_join( dataElementDictionary()[0,  ] , by = c("dataElement.id", "dataElement" , "displayName") )
      
    }
      
  })

  output$dataElementDictionaryTable = 
    
        DT::renderDT( DT::datatable(
       
          formulaChoices()  ,
          
          selection = 'multiple' ,
          rownames = FALSE, 
          filter = 'top' ,
          # options = DToptions_no_buttons()
          options = list(
            # bPaginate = FALSE, 
            autoWidth = TRUE ,
            scrollY = "55vh"  ,
            scrollX = TRUE ,
            scrollCollapse = TRUE ,
            paging = TRUE ,
            searching = TRUE , 
            info = TRUE ,
            lengthMenu = list( c(  10, 25, 100, -1 ) , 
                               list( '10', '25', '100', 'All' ) ) ,
            pageLength = 10 ,
            server = TRUE ,
            dom = 'tirp' ) ,
          fillContainer = TRUE
        ))
      
  
  selected_elements = reactive({
    req( input$dataElementDictionaryTable_rows_selected  )
    
    cat('\n* formula_widget selected_elements():')
    row_selected = input$dataElementDictionaryTable_rows_selected 
    
    cat('\n - row number selected is' ,  row_selected )
    
    # if ( length(row_selected) ) {
    selected = formulaChoices() %>% 
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
  

## Selected ELements ####
  
  
  
  updated_formula_elements = reactiveValues( df = tibble() )
  
  observeEvent( formula_elements() , {
    
    cat( "\n* updated_formula_elements = formula_elements()")
    
    if ( nrow( formula_elements() ) == 0  ){
      # Testing
      # saveRDS( dataElementDictionary()[0, ]  , 'emptyDataDictionary.rds')
      
      cat( "\n - formula_elements() is empty")
      
      updated_formula_elements$df = dataElementDictionary()[0, ] 
      
    } else {
      
      cat( "\n - setting formula_elements()")
      
      updated_formula_elements$df = formula_elements()
    }
    
  })
  
  observeEvent( selected_elements() , {
    
       cat('\n* observe selected_elements() '  ) 
      
        selected_categories = selected_elements() %>%
          separate_rows( Categories , categoryOptionCombo.ids, sep = ";" ) %>%
          mutate( Categories = Categories %>% str_trim  ,
                  categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim ,
                  Formula.Name = formulaName()
                  # ,zeroIsSignificant = as.logical( zeroIsSignificant )
          )
      
      
      if ( nrow( updated_formula_elements$df ) == 0 ){
        
        cat( '\n- updated_formula_elements == 0')
        
        updated_formula_elements$df = selected_categories  %>%
          arrange( dataElement ) %>%
          select( Formula.Name, everything() ) %>%
          distinct()
        
      } else {
        cat('\n - adding' , nrow( selected_categories ), 'selected elements' )
        
        updated_formula_elements$df = bind_rows( updated_formula_elements$df , selected_categories ) %>%
          arrange( dataElement ) %>%
          select( Formula.Name, everything() ) %>%
          distinct()
      }
    
  })
  
  observeEvent( input$deleteRows , {
    
    cat( "\n* delete row:")
    
    if ( ! is.null( input$forumlaDictionaryTable_rows_selected  ) ){
      cat( "\n - delete row:", input$forumlaDictionaryTable_rows_selected)
      
      updated_formula_elements$df = updated_formula_elements$df[ -as.numeric( input$forumlaDictionaryTable_rows_selected  ) , ]
    } 
  })
  

  output$forumlaDictionaryTable = 
    DT::renderDT( DT::datatable(
      
      updated_formula_elements$df ,
      
      rownames = FALSE, 
      # filter = 'top' ,
      options = list(
        autoWidth = TRUE ,
        scrollY = "65vh"  ,
        scrollX = TRUE ,
        scrollCollapse = FALSE ,
        paging = FALSE ,
        searching = TRUE , 
        info = TRUE ,
        server = TRUE ,
        pageLength = -1 ,
        dom = 'tirp'),
      fillContainer = TRUE
      
      
)
      # options = DToptions_no_buttons()
    )
  
  output$formulaName = renderPrint({ formulaName() })

# Save Formula ####
  output$saveFormula <- downloadHandler(
   
    filename = function() {
      paste0( "Formulas_", Sys.Date()  ,".xlsx"  )
    } ,
    
    content = function( file ) {
      cat('\n* saving formula')
 
      wb <- openxlsx::createWorkbook()

      cat( "\n - create empty excelfile" )
      
      sheet1  <- addWorksheet( wb, sheetName = "Formula" )
      sheet2  <- addWorksheet( wb, sheetName = "Formula Elements" )
      
      no_existing_formulas = (  is.null( formulas() ) || nrow( formulas() ) == 0   )
      
      cat( "\n - test for existing formulas:" , ! no_existing_formulas )
      
      if (  no_existing_formulas  ){

        cat('\n - preparing new formula')
        new.Formula.Name = formulaName()
        new.Formula = selectedElementNames()

        new.formula_elements = updated_formula_elements$df

      } else {
        
        cat('\n - adding a formula to existing formulas')
        
        origninal_formula = formulas() %>%
            filter( ! Formula.Name %in%  formulaName() )
      
        cat('\n - orignal formulas have', nrow( formulas() ), 'formulas')
        
        cat('\n - new formula in original?', formulaName() %in% formulas()$Formula.Name )
      
        orginal_formula_elements = all_formula_elements() %>%
          filter( ! Formula.Name %in%  formulaName() )
        
        cat('\n - orignal formulas elements have', nrow( all_formula_elements()), 'rows')
      
        new.Formula.Name = c( formulaName() , origninal_formula$Formula.Name )
        
        new.Formula = c( selectedElementNames() , origninal_formula$Formula  )
        
        new.formula_elements = rbind( updated_formula_elements$df ,
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

