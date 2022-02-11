
translate_dataset = function( data , formula_elements ){
  cat('\n* prepare dataset:')
  dd = data %>% 
        rename( dataElement.id = dataElement , categoryOptionCombo.ids = categoryOptionCombo ) %>%
        left_join( formula_elements %>% 
                     select( dataSet, periodType, zeroIsSignificant, 
                             dataElement.id, categoryOptionCombo.ids, dataElement, Categories ) , 
                   by = c("dataElement.id", "categoryOptionCombo.ids")
                   )
  
  d.ts = df_pre_ts( dd )
  
  return( d.ts )
}

ous_tree = function( ous , ouLevels ){
  
    cat('\n* ous_tree:')
  
    ous.id_parent = ous %>% arrange( level ) %>% select( id, parent ) 
    # Remove top value if it has missing parent.
    ous.id_parent = ous.id_parent %>% filter( !is.na( parent ) )
    # glimpse( ous.id_parent )
    
    # TESTING
    # saveRDS( ous , 'ous_tree.ous.rds' )
    
    # parent is a df column
    if ( !'data.frame' %in% class( ous$parent ) ){
      cat('\n - ous$parent is not a data.frame')
      
      ous.id_parent = ous %>% 
      filter( !is.na( parent ) ) %>%
      mutate( parent = parent.id ) %>%
      select( id, parent )
      
    } else {
      
      cat('\n - ous$parent IS a data.frame')
      p = ous$parent %>% pull(id) 
      ous.id_parent = ous %>% 
      mutate( parent = p ) %>% 
      filter( !is.na( parent ) ) %>%
      select( id, parent )
    }
    
    ous.tree = FromDataFrameNetwork( ous.id_parent )
    
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
    dft = dft %>% select(  keep.col.numbers ) %>%
      setnames( c( 'orgUnit', cols , 'level' ) )
    # glimpse(dft)
    
    cat('\n - preparing dft.translated ')
    dft.translated = dft %>%
      pivot_longer( cols = c(-orgUnit,-level) , names_to = 'Level') %>%
      left_join( ous %>% dplyr::select( id, name ) , by = c( 'value' = 'id') ) %>%
      pivot_wider( -value , names_from = Level, values_from = name ) %>%
      left_join( ous %>% dplyr::select( id, name ) , by = c( 'orgUnit' = 'id') ) %>%
      rename( orgUnitName = name ) %>%
      select( orgUnit, orgUnitName ,  everything() )
      
    return( dft.translated )
}

data_leaves = function( d ){
  cat('\n* determining effective leaf')
  data.leaves = d  %>%
    as_tibble() %>%
    group_by( orgUnit ) %>%
    summarise( n = max( COUNT , na.rm = TRUE ) ) %>%
    mutate( effectiveLeaf = ifelse( n == 1, TRUE, FALSE ) ) %>%
    select( orgUnit , effectiveLeaf ) 
  
  return( data.leaves )
}

# Wrapper function to prepare data before testing for outliers
# data can be used with reporting_widget
data_1 = function( data , formula_elements , ousTree   ){
  cat('\n* preparing data_1:')
  
  # TESTING
  saveRDS( data , 'data.rds' )
  saveRDS( formula_elements , 'formula_elements.rds' )
  saveRDS( ousTree , 'ousTree.rds' )
  
  cat('\n - translate_dataset:')
  dd = translate_dataset( data , formula_elements )
  
  cat('\n* - df_pre_ts:')
  d = df_pre_ts( dd )
  
  cat('\n - df_ts:')
  d.ts = df_ts( d )

  cat('\n - data_leaves')
  data.leaves = data_leaves( d )
  
  cat('\n - d.')
  d. = d.ts %>% 
  left_join( data.leaves , by = 'orgUnit') %>%
  left_join( ousTree , by = 'orgUnit')  %>%
  mutate( original = SUM )
  # left_join( ouLevels %>% select( level, levelName) , by = 'level' )
# glimpse( d. )
  return( d. )
}

