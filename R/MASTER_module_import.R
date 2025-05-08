#' @export
MASTER_module_import_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    #module_import_01_options_ui(id = ns("act001_s99_01")),
    uiOutput(ns("box01_data_source")),
    module_import_02_settings_ui(id = ns("act001_s99_02")),
    module_import_03_show_ui(id = ns("act001_s99_03")), 
    uiOutput(ns("show_dev"))
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
        
        
        shiny::selectInput(
          inputId = ns("sui_data_source"),
          label = "Qué fuente de datos prefieres?",
          choices = c("01 - xlsx files"       = "source_xlsx",
                      "02 - csv files"        = "source_csv",
                      "03 - RMedic examples"  = "source_Rscience",
                      "04 - R examples"       = "source_Rdata"),
          selected = "source_Rdata"
          
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
          # Tabla de datos
          h4("Primeras filas del conjunto de datos:"),
          tableOutput(ns("data_table")),
          br(),
          h4("Parametros del usuario"),
          verbatimTextOutput(ns("my_parameters"))
        )
        
      })
        
      # Mostrar las primeras filas de la tabla
      output$data_table <- renderTable({
        req(show_dev, output_list_database())
        df_safe <- tryCatch(output_list_database()$"database", error = function(e) NULL)
        head(df_safe, 5)
      })
      
      output$my_parameters <- renderPrint({
        req(show_dev, output_list_database())
        list_safe <- tryCatch(output_list_database(), error = function(e) NULL)
        if (is.null(list_safe)) return("Sin datos.")
        str(list_safe)
      })
      
      # Devolver solo los datos confirmados
      return(reactive(output_list_database()))
      
    }
  )
}
