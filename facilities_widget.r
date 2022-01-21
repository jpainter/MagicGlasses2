
Facilities 
====================================

checkboxInput( "bootstrap" , label ='Bootstrap prediction',
               value = TRUE  ) 

selectInput("Reps", label = "Iterations" , 
              choices = c(100, 1000, 5000, 10000) , 
              selected = 100  ) 

                    
DTOutput( 'dataTable'   ) 



tableData = reactive({
  req( data.total() )
  
  .period = period()
  key.cols = setdiff( group_by_cols() , .period )
  print( "tableData key.cols: ")
  print( key.cols )
  
  print( 'data for table' )
  print( names( data.total() ))
  
  td = data.total() %>% as_tibble() %>%  
    ungroup %>% 
    # select(-Month)  %>% 
    group_by( across( all_of(  {{key.cols}} ) ) ) %>%
    # group_by( orgUnit, dataSet, Zone, District, Facility  ) %>%
    summarise( n = n() )
    
  return( td )
  
})

output$dataTable <- DT::renderDT(
    datatable(
      tableData() , 
      filter = 'top' ,
      extensions = 'Buttons', 
      options = list(
        initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#0894e5', 
      'color': '#000000'});",
            "}"),
      pageLength = 100,
      scrollY = "400px" ,
      dom = 'Bfrtip',
      buttons = 
        list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ))

  )
          )
)

