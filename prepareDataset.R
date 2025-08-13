
# Match dataElements and category combos from data_element formula, which may not have 
# updated category combo if a dataset was revised with new categories  
translate_dataset = function( data , formula_elements ){
  cat('\n* translate_dataset:')
  
  # Data may contain mix of elements with and without categories. 
  if ( any( is.na( data$categoryOptionCombo )) ){
    
    element_match = match( paste( data$dataElement, data$categoryOptionCombo ) , 
                           
                       paste( formula_elements$dataElement.id , 
                              
                              ifelse( is.na( formula_elements$n_categoryOptions ) , 
                                      NA ,  
                                      formula_elements$categoryOptionCombo.ids ) 
                              )
    )
  } else {
    
    element_match = match( paste( data$dataElement , data$categoryOptionCombo ) , 
                       paste( formula_elements$dataElement.id , 
                              formula_elements$categoryOptionCombo.ids )
    )
  }
  
  
  formula_data = formula_elements[ element_match , ] %>% select( - dataElement.id, - categoryOptionCombo.ids )

  dd = data %>% 
      rename( 
        dataElement.id = dataElement , 
        categoryOptionCombo.ids = categoryOptionCombo 
        ) %>%
      bind_cols( formula_data )
          
  # dd = data %>% 
  #       rename( 
  #         dataElement.id = dataElement , 
  #         categoryOptionCombo.ids = categoryOptionCombo 
  #         ) %>%
  #       left_join( formula_elements %>% 
  #                    select( dataSet, periodType, zeroIsSignificant, 
  #                            dataElement.id, categoryOptionCombo.ids, dataElement, Categories ) , 
  #                  by = c("dataElement.id", "categoryOptionCombo.ids")
  #                  )
  
  # d.ts = df_pre_ts( dd )
  
  return( dd )
}

# revised function matching dataElements and category combos from separate lists
translate_dataset_2 = function( data , dataElements, categories  ){
  
  cat('\n* translate_dataset_2:')
  

  #Testing 
  # saveRDS( categories, 'categories.rds')
  
  # Expand to one row per linked pair
  cat( "\n- expanding categories table")
  
  categories_expanded <- categories %>%
    mutate(
      cat_list = str_split( Categories, " ;\\n "),
      opt_list = str_split( categoryOptionCombo.ids, " ;\\n ")
    ) %>%
    mutate(
      paired = map2( cat_list, opt_list, ~ tibble(
        Category = trimws(.x),
        categoryOptionCombo.id = trimws(.y)
      ))
    ) %>%
    select(categoryCombo.id, categoryCombo, paired) %>%
    unnest(paired)
  

  # Data may contain mix of elements with and without categories. 
  cat( "\n- matching dataElement with dataElelemnt.id and 
       categoryOptionCombo with categoryOptionCombo.id")

    dataElement_match = match(  data$dataElement  , 
                                dataElements$dataElement.id )
   
    categories_match = match(  data$categoryOptionCombo  , 
                               categories_expanded$categoryOptionCombo.id )
  
  
    dd = data %>% 
      rename( 
        dataElement.id = dataElement , 
        categoryOptionCombo.ids = categoryOptionCombo 
        ) %>%
      bind_cols( 
        dataElements[ dataElement_match , "dataElement" ] , 
        categories_expanded[ categories_match , "Category" ] 
        )
     
  
  return( dd )
}

ous_tree = function( ous , ouLevels ){
  
    cat('\n* PrepareDataset.R ous_tree:')
  
    # TESTING
    save( ous, ouLevels, file = 'ousTree.rda' )
  
    # Special screening for benin!
    ous2.bad = ous %>% filter( parent %in% 'UO_supprimé') %>% pull( id ) #; length( ous2.bad )
    ous3.bad = ous %>% filter( parent.id %in% ous2.bad ) %>% pull( id ) #; length( ous3.bad )
    ous4.bad = ous %>% filter( parent.id %in% ous3.bad ) %>% pull( id ) #; length( ous4.bad )
    ous5.bad = ous %>% filter( parent.id %in% ous4.bad ) %>% pull( id ) #; length( ous5.bad )
    
    if ( length( ous2.bad ) > 0 ){
      cat('\n - removing ous.bad ')
      ous = ous %>% filter( ! id %in% c( ous2.bad , ous3.bad , ous4.bad , ous5.bad))
    }
  
    if ( 'data.frame' %in% class( ous$parent ) ){ # parent is a df column
      cat('\n - ous$parent is a data.frame that needs to be processed')
      p = ous$parent %>% pull(id) 
      ous.id_parent = ous %>% 
        mutate( parent = p ) %>% 
        filter( !is.na( parent ) ) %>%
        select( id, parent )
      
    } else {
      
      # ous$parent.id[is.na(ous$parent.id)] <- "tree_root"
      
      ous.id_parent = ous %>% 
        filter( !is.na( parent ) , # Removes top level (no parent)
              ) %>% 
        arrange( level ) %>% 
        mutate( parent = parent.id ) %>%
        select( id, parent  ) 
    }
  
    # glimpse( ous.id_parent )
    
    ous.tree = FromDataFrameNetwork( ous.id_parent  )
    
    dti = data.tree::as.igraph.Node( ous.tree )
    nodes = V(dti)
    node.attributes = nodes %>% attributes()
    ids = node.attributes$names
    
    # FAST, but needs proper level names...
    
    dft = ToDataFrameTree( ous.tree , 
                    orgUnit = ids ,
                    lvl1 = function(x) x$path[1],
                    lvl2 = function(x) x$path[2],
                    lvl3 = function(x) x$path[3],
                    lvl4 = function(x) x$path[4],
                    lvl5 = function(x) x$path[5],
                    lvl6 = function(x) x$path[6],
                    lvl7 = function(x) x$path[7],
                    lvl8 = function(x) x$path[8],
                    level = function(x) as.integer( x$level) )[,-1] %>%
      as_tibble()
    
    cols = ouLevels$levelName
    keep.col.numbers = c( 1:eval( length( cols ) + 1) , 10 ) 
    dft = dft %>% select( all_of( keep.col.numbers ) ) %>%
      setnames( c( 'orgUnit', cols , 'level' ) )
    # glimpse(dft)
    
    cat('\n - preparing dft.translated ')
    dft.translated = dft %>%
      pivot_longer( cols = c(-orgUnit,-level) , names_to = 'Level') %>%
      left_join( ous %>% dplyr::select( id, name ) , by = c( 'value' = 'id') ) %>%
      pivot_wider( -value , names_from = Level, values_from = name ) %>%
      left_join( ous %>% dplyr::select( id, name , leaf ) , by = c( 'orgUnit' = 'id') ) %>%
      rename( orgUnitName = name ) %>%
      arrange( level ) %>%
      select( orgUnit, orgUnitName ,  everything() )
      
    cat('\n - done')
    return( dft.translated )
}

data_leaves = function( d ){
  
  cat('\n* determining effective leaf')
  data.leaves = d  %>%
    as_tibble() %>%
    group_by( orgUnit, dataElement.id, leaf ) %>%
    # group_by( orgUnit, dataElement.id ) %>%
    summarise( 
      n = ifelse( !all(is.na( COUNT ) ) , 
                  max( as.integer( COUNT ) , na.rm = TRUE ) , # as integer because character value may be "1" or "1.0"
                  0 ) , 
      .groups = 'keep'
         ) %>%
    mutate( 
      effectiveLeaf = ifelse( n == 1, TRUE, FALSE ) ) %>%
    mutate( effectiveLeaf = ifelse( n == 1 | leaf == TRUE , TRUE, FALSE ) ) %>%
    # select( orgUnit , dataElement.id , leaf , effectiveLeaf ) %>%
    ungroup()
  
   data.leaves = d  %>%
    as_tibble() %>%
    group_by( orgUnit, dataElement.id, leaf ) %>%
    summarise( n = max( COUNT , na.rm = TRUE ) ) %>%
    mutate( effectiveLeaf = ifelse( n == 1 | leaf == TRUE , TRUE, FALSE ) ) %>%
    select( orgUnit , dataElement.id , leaf, effectiveLeaf ) 
  
   
  return( data.leaves )
}


# Wrapper function to prepare data before testing for outliers
# data can be used with reporting_widget
# data_1 = function( data , formula_elements , ousTree   ){
#   cat('\n* preparing data_1:')
#   
#   # TESTING
#   saveRDS( data , 'data.rds' )
#   saveRDS( formula_elements , 'formula_elements.rds' )
#   saveRDS( ousTree , 'ousTree.rds' )
#   
#   if ( ! 'COUNT' %in% names( data )) return()
# 
#   
#   cat('\n - translate_dataset:')
#   dd = translate_dataset( data , formula_elements )
#   
#   cat('\n* - df_pre_ts:')
#   d = df_pre_ts( dd )
#   
#   cat('\n - df_ts:')
#   d.ts = df_ts( d )
# 
#   cat('\n - data_leaves')
#   data.leaves = data_leaves( d )
#   
#   cat('\n - d.')
#   d. = d.ts %>% 
#   left_join( data.leaves , by = 'orgUnit') %>%
#   left_join( ousTree , by = 'orgUnit')  %>%
#   mutate( original = SUM , value = !is.na( SUM ))
#   # left_join( ouLevels %>% select( level, levelName) , by = 'level' )
# # glimpse( d. )
#   return( d. )
# }

is_null_or_empty <- function(x) {
  is.null(x) || length(x) == 0 || (is.character(x) && x == "")
}

data_1 = function( data , 
                   dataSets = NULL ,
                   formula_elements = NULL , 
                   dataElements = NULL, 
                   categories = NULL, 
                   ousTree = NULL , 
                   timing = FALSE  ){
  cat('\n* prepareDataset.R preparing data_1:')
  
  #   # TESTING
  # saveRDS( data , 'data.rds' )
  # saveRDS( dataSets , 'dataSets.rds' )
  # saveRDS( formula_elements , 'formula_elements.rds' )
  # saveRDS( dataElements , 'dataElements.rds' )
  # saveRDS( categories , 'categories.rds' )
  # saveRDS( ousTree , 'ousTree.rds' )
  
  if ( ! 'COUNT' %in% names( data )) return()
  
  # if periodType missing, assume Monthly
  if ( ! "periodType" %in% names( data ) ) formula_elements$periodType = "Monthly"
  ptype = min( formula_elements$periodType , na.rm = T)
  if ( is_null_or_empty( ptype ) ) ptype = "Monthly" # if missing, assume Monthly 
  cat( '\n - ptype is' , ptype )
  
  if ( grepl( "weekly" , ptype , ignore.case = T) ){
    p = "Week"
  } else {
    if ( grepl( "monthly" , ptype , ignore.case = T)  ) p = "Month"
  }
  
  # cat( '\n - periodType is' , p )
  
  cat('\n - translate_dataset:')
  
  if (! 'categoryOptionCombo' %in% names( data )){
    
    cat( "\n - 'categoryOptionCombo' %in% names( data ): FALSE ")
    data = data %>% mutate( categoryOptionCombo = NA )
    
  } 
  
  # prepare list of datasets associated with each dataElement
  dataSetElements = dataSets %>% 
      unnest( dataSetElements.id, names_sep = "_" ) %>% 
      select( c(1:3,5) ) %>% 
      rename( dataElement.id = dataSetElements.id_dataElement ) %>%
      # ensure that dataSet and dataElements are atomic vectors
      mutate(
        dataElement.id = as.character( dataElement.id$id )
      ) %>%
      group_by( dataElement.id ) %>%
      summarise( 
            n_datasets = n() ,
            dataSet.ids = paste( dataSet.id ,  collapse = " ;\n"),
            dataSet = paste( dataSet ,  collapse = " ;\n")
          )  %>% arrange( - n_datasets  )

  if ( timing ) tic()
    
  d. =  data  %>%
    as_tibble %>% 
    filter( !is.na( SUM ) ) %>%
    # select( dataElement.id , categoryOptionCombo.ids , orgUnit , period ,  COUNT , SUM  ) %>%
    # rename( dataElement = dataElement.id , categoryOptionCombo = categoryOptionCombo.ids ) %>%
    # translate_dataset( . , formula_elements ) %>%
    translate_dataset_2( . ,  dataElements , categories ) %>%
    left_join( dataSetElements , by = 'dataElement.id') %>%
    left_join( ousTree , by = 'orgUnit') 
    
    data.leaves = data_leaves( d. ) 
    

  if ( timing ) cat( "\n - d." , toc()$callback_msg )
    
  if ( timing ) tic()
  d.. = 
    setDT(d.) %>%
    table.express::left_join( setDT( data.leaves ) , orgUnit , dataElement.id  )  %>%
    # table.express::left_join( setDT( ousTree ) , orgUnit )  %>%
    as_tibble() %>%
    select( - i.leaf ) %>%
    df_pre_ts( . , period = p  ) %>%
    df_ts( . , period = p ) %>%
    mutate( original = SUM , value = !is.na( SUM )) 
    
  if ( timing ) cat( "\n - " , toc()$callback_msg )
  
  return( d.. )
}

# Outlier detection-flag



