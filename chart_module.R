# chart_module.R

chartModuleUI <- function(id, label = "Chart") {
  ns <- NS(id)
  tagList(
    # CSS to make buttons smaller
    tags$style(HTML("
      .compact-button .btn {
        font-size: 14px;
        padding: 2px 8px;
        height: 30px;
        line-height: 1.2;
      }
    ")),
    
    div(
      style = "text-align: center;",
      div(
        style = "display: inline-block; margin-right: 20px;",
        h3(style = "font-size: 16px; margin: 0;", label)
      ),
      div(
        class = "compact-button",
        style = "display: inline-block; margin-right: 10px;",
        actionButton(ns("open_modal"), "Customize")
      ),
      div(
        class = "compact-button",
        style = "display: inline-block;",
        downloadButton(ns("download_plot"), label = NULL)
      )
    ),
    
    plotOutput(ns("plot"), height = "90%" )
  )
}


chartModuleServer <- function(id, plot_expr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues( 
      
      filename = "chart"  ,
      title = NULL ,
      subtitle = NULL ,
      caption = NULL ,
      base.size = 14, 
      legend.position = "bottom" ,
      format = "png" ,
      width = 6 ,
      height = 4 ,
      dpi = 300 , 
      transparent = TRUE  ,
      theme = "Minimal" , 
      ready = TRUE 
      
      )
    
    # Show modal
    observeEvent(input$open_modal, {
      showModal(modalDialog(
        title = "Download Options",
        textInput(ns("filename"), "Filename (no extension)", "filename"),
        textInput(ns("title"), "Plot title:", ""),
        textInput(ns("subtitle"), "Subtitle:", ""),
        textInput(ns("caption"), "Caption:", ""),
        numericInput(ns("base.size"), "Font size:", 14),
        selectInput(ns("legend.position"), "Legend position:", choices = c("none", "bottom", "top", "right")),
        selectInput(ns("format"), "Format:", choices = c("png", "pdf", "svg")),
        numericInput(ns("width"), "Width (inches):", 6),
        numericInput(ns("height"), "Height (inches):", 4),
        numericInput(ns("dpi"), "DPI:", 300),
        checkboxInput(ns("transparent"), "Transparent background (PNG/SVG only)", FALSE),
        selectInput( ns("theme") , "Plot theme:",
                    choices = c("Gray (default)" = "gray",
                                "Minimal" = "minimal",
                                "Classic" = "classic",
                                "Light" = "light",
                                "Dark" = "dark",
                                "Black & White" = "bw"),
                    selected = 1 ) ,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_modal"), "Apply")
        ),
        easyClose = TRUE
      ))
    })
    
    # Confirm and prepare download
    observeEvent(input$confirm_modal, {
      rv$filename <- input$filename
      rv$title <- input$title
      rv$subtitle <- input$subtitle
      rv$caption <- input$caption
      rv$base.size <- input$base.size
      rv$legend.position <- input$legend.position 
      rv$format <- input$format
      rv$width <- input$width
      rv$height <- input$height
      rv$dpi <- input$dpi
      rv$transparent <- input$transparent
      rv$theme <- input$theme
      rv$ready <- TRUE
      removeModal()
    })
    

    # Theme selector
    get_theme <- function(theme_key='default') {
      switch(
        theme_key,
        "minimal" = theme_minimal(),
        "classic" = theme_classic(),
        "light"   = theme_light(),
        "dark"    = theme_dark(),
        "bw"      = theme_bw(),
        theme_gray()
      )
    }
    
    # Display plot
    output$plot <- renderPlot({
      plot_expr() +
        labs(
          title = rv$title,
          subtitle = rv$subtitle ,
          caption = rv$caption
        ) +
        get_theme( rv$theme ) + 
        theme( text = element_text( size = rv$base.size ) ,
               legend.position = rv$legend.position ,
               strip.text = element_text(face="bold")
               )
        
    })
    
    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(rv$filename, "_", Sys.Date(), ".", rv$format)
      },
      content = function(file) {
        plot <- plot_expr() +
          labs(
            title = rv$title,
            subtitle = rv$subtitle,
            caption = rv$caption
          ) + 
        get_theme( rv$theme ) + 
        theme( text = element_text( size = rv$base.size ) ,
               legend.position = rv$legend.position 
               )
        
        ggsave(
          filename = file,
          plot = plot,
          device = rv$format,
          width = rv$width,
          height = rv$height,
          dpi = rv$dpi,
          bg = if (rv$transparent && rv$format %in% c("png", "svg")) "transparent" else "white"
        )
      }
    )
  })
}
