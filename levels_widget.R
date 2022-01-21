
reporting_widget_ui = function ( id ) 
{
        ns <- NS(id)  
        fillCol( height = 600, flex = c(NA ) , 
        
        add_busy_spinner(spin = "fading-circle", position = "bottom-right") ,

        
      inputPanel(
        
        
        selectInput("level2", label = "OrgUnit Level2" , 
                    choices = NULL, 
                    selected = NULL ,
                    multiple = TRUE ) ,
      
        selectInput("level3", label = "OrgUnit Level3" ,
                    choices = NULL,
                    selected = NULL ,
                    multiple = TRUE ) ,
      
        selectInput("level4", label = "OrgUnit Level4" ,
                    choices = NULL,
                    selected = NULL  ,
                    multiple = TRUE  ) ,
      
        selectInput("level5", label = "OrgUnit Level5" ,
                    choices = NULL,
                    selected = NULL  ,
                    multiple = TRUE  ) ,
      
        
        selectInput("source", label = "Original/Cleaned" , 
                    choices = c( 'Original', 'Cleaned' ) , 
                    selected = 'Original' ) ,
        
        selectInput("split", label = "Split Data By:" , 
                    choices = "None" , 
                    selected = "None" ) , 
        
        checkboxInput( "count.any", label ='Count any categories', value = FALSE ) ,
        
        checkboxInput( "mostReports", label ='Most frequently reporting facilities', value = TRUE ) ,
        
        selectInput( "startingMonth", label = "begining with", 
                     choices = NULL ,
                     selected = NULL ) ,
        selectInput( "endingMonth", label = "ending with", 
                     choices = NULL , 
                     selected = NULL ) ,
        
        actionButton( "screenshot" , "Screenshot" ) ,
        
        selectInput("shotSelection", label = "Screenshot Chart:" , 
                    choices = c(NULL, 
                                'plot_reporting_by_month', 
                                'plot_reports_in_a_year', 
                                'map' , 
                                'plot_values' ,
                                'plot_trends'),
                    selected = NULL ) 
        # checkboxInput( 'calendar_year', "Calendar Year" , value = TRUE )
      )
      
        )
        }