#' @export
MASTER_module_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  div(
    # Un solo card grande con las dos columnas dentro
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex align-items-center",
        style = "background-color: #ff9a3c; color: white; border-bottom: 1px solid #e67e22; padding-left: 10px;",
        tags$i(class = "fa fa-database me-2"),
        tags$b("Data Import")
      ),
      card_body(
        style = "padding: 15px; background-color: #fff3e6;",
        
        # Estructura de columnas dentro del card
        fluidRow(
          # Columna izquierda - Controles y configuración
          column(3, 
                 div(
                   style = "padding: 10px; border-right: 1px solid #ffe0b2;",
                   # Título de la sección
                   h4(
                     class = "mb-3",
                     style = "color: #e67e22;",
                     tags$i(class = "fa fa-cog me-2"),
                     "Import menu"
                   ),
                   
                   # Sección de fuente de datos
                   div(
                     class = "mb-3",
                     uiOutput(ns("box01_data_source"))
                   ),
                   
                   # Sección de configuración con separador visual
                   div(
                     class = "mb-3 pt-2",
                     style = "border-top: 1px solid #ffe0b2;",
                     br(),
                     module_import_02_settings_ui(id = ns("act001_s99_02"))
                   ),
                   
                   # Sección de visualización con separador visual
                   div(
                     class = "pt-2",
                     style = "border-top: 1px solid #ffe0b2;",
                     module_import_03_show_ui(id = ns("act001_s99_03"))
                   )
                 )
          ),
          
          # Columna derecha - Vista previa de datos
          column(9, 
                 div(
                   style = "padding: 10px;",
                   # Título de la sección
                   h4(
                     class = "mb-3",
                     style = "color: #e67e22;",
                     tags$i(class = "fa fa-table me-2"),
                     "Preview"
                   ),
                   
                   # Contenedor para la tabla con bordes y esquinas redondeadas
                   div(
                     # class = "border rounded mb-4",
                     # style = "overflow-x: auto; background-color: white;",
                     h5(
                       class = "m-3",
                       tags$b("First 5 rows from dataset:")
                     ),
                     div(
                       class = "border rounded", 
                       style = "overflow-x: auto; max-width: 100%; background-color: white;",
                       tableOutput(ns("data_table"))
                     )
                   ),
                   
                   # Información adicional
                   div(
                     class = "mt-3",
                     uiOutput(ns("info_text2"))
                   )
                 )
          )
        )
      )
    ), br(), br(), br(), br(), br(), br(), br()
  )
  
  
}




#' @export
MASTER_module_import_server <- function(id, sui_data_source, show_dev = FALSE){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      output$box01_data_source <- renderUI({
        
        vector_choices <- c("01 - xlsx files"       = "source_xlsx",
                            "02 - csv files"        = "source_csv",
                            "03 - Rscience examples"  = "source_Rscience",
                            "04 - R examples"       = "source_Rdata")
        
        vector_choices <- c("Select one..." = "", vector_choices)
        selected_pos <- length(vector_choices) #1
        shiny::selectInput(
          inputId = ns("sui_data_source"),
          label = "Data source:",
          choices = vector_choices,
          selected = vector_choices[selected_pos]
          
        )
        
        
        
        
        
      })
      
      # 1.1 - Eleccion de la fuente de datos - Iniciado una sola vez
      sui_data_source <- reactive({
        req(input$"sui_data_source")
        input$"sui_data_source"
        
      })
      
      # 1.2 - Settings from data source - Iniciado una sola vez
      list_sui_settings <- module_import_02_settings_server(id = "act001_s99_02", sui_data_source)
      
      # 1.3 - Imported data - Iniciado una sola vez
      output_list_database <- module_import_03_show_server(id = "act001_s99_03", list_sui_settings)
      
      
      output$show_dev <- renderUI({
        req(show_dev)
        div(
          br(),
          h4("Parametros del usuario"),
          verbatimTextOutput(ns("my_parameters"))
        )
        
      })
        
      # Mostrar las primeras filas de la tabla
      output$data_table <- renderTable({
        req(output_list_database())
        df_safe <- tryCatch(output_list_database()$"database", error = function(e) NULL)
        head(df_safe, 5)
      })
      
      output$my_parameters <- renderPrint({
        req(show_dev, output_list_database())
        list_safe <- tryCatch(output_list_database(), error = function(e) NULL)
        if (is.null(list_safe)) return("Sin datos.")
        str(list_safe)
      })
      
      
      output$info_text2 <- renderUI({
        
        req(output_list_database())
        req(output_list_database()$"database")
        
        list_safe <- tryCatch(output_list_database(), error = function(e) NULL)
        if (is.null(list_safe)) return("Sin datos.")
        
        data_source <- list_safe$data_source
        original_file_name <- list_safe$"original_file_name"
        value_ncol <- ncol(list_safe$"database")
        value_nrow <- nrow(list_safe$"database")
        
        # Contenedor principal
        div(
          class = "p-3 rounded shadow-sm",
          style = "background: linear-gradient(to right, #f8f9fa, #ffffff);",
          
          # Título principal
          h4(
            class = "mb-3 pb-2",
            style = "border-bottom: 2px solid #0d6efd; color: #0d6efd;",
            icon("info-circle"), 
            "Data Selection"
          ),
          
          fluidRow(
            column(6,       # Sección de datos
                   div(
                     class = "mb-3 p-2 rounded",
                     style = "background-color: rgba(13, 110, 253, 0.05); border-left: 4px solid #0d6efd;",
                     
                     h5(class = "text-primary", 
                        icon("database", 
                             style = "padding-left: 10px;", 
                             class = "me-2"), "Data details"),
                     
                     div(class = "d-flex flex-wrap",
                         div(class = "me-4 mb-2",
                             tags$b(style = "padding-left: 10px;", "Source: "),
                             span(data_source, style = "font-family: monospace;")),
                         
                         div(class = "me-4 mb-2",
                             tags$b(style = "padding-left: 10px;", "File: "),
                             span(original_file_name, style = "font-family: monospace;")),
                         
                         div(class = "me-4 mb-2",
                             tags$b(style = "padding-left: 10px;", "Dimensions: "),
                             span(paste0(value_nrow, " rows × ", value_ncol, " columns"), 
                                  style = "font-family: monospace;"))
                     )
                   )
                   
                   
                   
            )
            
                     )
                   )
         
      })
      
      
      # Devolver solo los datos confirmados
      return(reactive(output_list_database()))
      
    }
  )
}
