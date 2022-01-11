

# DT table options... ####
buttonList = function( file_name = paste( 'downloaded_' , Sys.Date() ) ){
  list( 'copy', 'print', 
        list(
          extend = 'collection', 
          buttons = list( 
            list( extend = 'csv'  , filename = file_name) , 
            list( extend = 'excel'  , filename = file_name) ,
            list( extend = 'pdf' , filename = file_name)  
          ) ,
          text = 'Download' 
        )
  )
}

DToptions_with_buttons = function(...){
  list( autoWidth = TRUE , 
        scrollX = TRUE  ,
        scrollY = "100vh" ,
        scrollCollapse = FALSE ,
        lengthMenu = list( c( -1, 1 , 5, 10, 25, 100 ), list( 'All' , '1', '5' , '10', '25', '100') ) ,
        pageLength = 100,
        columnDefs = list( list( className = 'dt-right' , targets="_all" ) ) ,
        dom = 'l<"col-sm-6"B>fiprt' ,
        buttons = buttonList(...) ,
        fillContainer = TRUE
  )
}


DToptions_no_buttons = function(...){
  list( autoWidth = TRUE , 
        scrollX = TRUE ,
        dom = 'l<"col-sm-6"i>fprt' ,
        scrollY = "100vh"  ,
        scrollCollapse = FALSE ,
        lengthMenu = list( c( -1, 1, 5, 10, 25, 100 ), list( 'All' , '1', '5' , '10', '25', '100') ) ,
        pageLength = 100,
        columnDefs = list( list(className = 'dt-right', targets="_all" ) ) ,
        rownames = FALSE , 
        server = TRUE , 
        escape = FALSE , 
        selection = list( mode='single' ) ,
        fillContainer = TRUE
  )
}