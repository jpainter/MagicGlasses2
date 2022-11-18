
translate_dataset = function( data , formula_elements ){
  cat('\n* translate_dataset:')
  
  # Data may contain mix of elements with and without categories. 
  if ( any( is.na( data$categoryOptionCombo ))){
    
    element_match = match( paste( data$dataElement, data$categoryOptionCombo ) , 
                       paste( formula_elements$dataElement.id , 
                              ifelse( is.na( formula_elements$n_categoryOptions ) , 
                                      NA ,  
                                      formula_elements$categoryOptionCombo.ids ) 
                              )
    )
  } else {
    
    element_match = match( paste( data$dataElement, data$categoryOptionCombo ) , 
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

ous_tree = function( ous , ouLevels ){
  
    cat('\n* ous_tree:')
  
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
      left_join( ous %>% dplyr::select( id, name ) , by = c( 'orgUnit' = 'id') ) %>%
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
    group_by( orgUnit, dataElement.id ) %>%
    summarise( 
      n = ifelse( !all(is.na( COUNT ) ) , max( COUNT , na.rm = TRUE ) , 0 ) , .groups = 'keep'
         ) %>%
    mutate( 
      effectiveLeaf = ifelse( n == 1, TRUE, FALSE ) ) %>%
    select( orgUnit , dataElement.id , effectiveLeaf ) 
  
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

data_1 = function( data , formula_elements , ousTree , timing = FALSE  ){
  cat('\n* preparing data_1:')
  
  #   # TESTING
  # saveRDS( data , 'data.rds' )
  # saveRDS( formula_elements , 'formula_elements.rds' )
  # saveRDS( ousTree , 'ousTree.rds' )
  
  if ( ! 'COUNT' %in% names( data )) return()
  
  ptype = min( formula_elements$periodType , na.rm = T)
  if ( ptype == "Weekly") p = "Week"
  if ( ptype == "Monthly") p = "Month"
  cat( '\n - periodType is' , p )
  
  cat('\n - translate_dataset:')
  
  if (! 'categoryOptionCombo' %in% names( data )) data = data %>% mutate( categoryOptionCombo = NA )
  
  # if ( timing ) tic()
  # dd = translate_dataset( data , formula_elements )
  # if ( timing ) toc() 
  # 
  # cat('\n - df_pre_ts:')
  # if ( timing ) tic()
  # d = df_pre_ts( dd , period = p  )
  # if ( timing ) toc()
  # 
  # cat('\n - df_ts:')
  # if ( timing ) tic()
  # d.ts = df_ts( d , period = p ) 
  # if ( timing ) toc()
  # 
  # cat('\n - data_leaves')
  # 
  # cat('\n - combining data.leaves, outTree, and ouLevels')
  # if ( timing ) tic()
  # d. = d.ts %>%
  # left_join( data.leaves , by = c( 'orgUnit' , 'dataElement.id' ) )  %>%
  # left_join( ousTree , by = 'orgUnit')  %>%
  # mutate( original = SUM , value = !is.na( SUM ))
  
  
  # %>%
  # left_join( ouLevels %>% select( level, levelName) , by = 'level' )
# glimpse( d. )
  if ( timing ) tic()
    
  d. =  data  %>%
    as_tibble %>% 
    filter( !is.na( SUM ) ) %>%
    # select( dataElement.id , categoryOptionCombo.ids , orgUnit , period ,  COUNT , SUM  ) %>%
    # rename( dataElement = dataElement.id , categoryOptionCombo = categoryOptionCombo.ids ) %>%
    translate_dataset( . , formula_elements ) 
    
    data.leaves = data_leaves( d. )

  if ( timing ) toc() 
    
  if ( timing ) tic()
  d.. = 
    setDT(d.) %>%
    table.express::left_join( setDT( data.leaves ) , orgUnit , dataElement.id  )  %>%
    table.express::left_join( setDT( ousTree ) , orgUnit )  %>%
    mutate( original = SUM , value = !is.na( SUM )) %>%
    as_tibble() %>%
    df_pre_ts( . , period = p  ) %>%
    df_ts( . , period = p ) 
    
  if ( timing ) toc()
  
  return( d.. )
}

# Outlier detection-flag



