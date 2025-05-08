#' @export
MASTER_module_import_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    module_import_01_options_ui(id = ns("act001_s99_01")),
    module_import_02_settings_ui(id = ns("act001_s99_02")),
    module_import_03_show_ui(id = ns("act001_s99_03")), 
    uiOutput(ns("show_dev"))
  )
}


#' @export
MASTER_module_import_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      # 1.1 - Eleccion de la fuente de datos - Iniciado una sola vez
      sui_data_source<- module_import_01_options_server(id = "act001_s99_01")
      
      # 1.2 - Settings from data source - Iniciado una sola vez
      list_sui_settings <- module_import_02_settings_server(id = "act001_s99_02", sui_data_source)
      
      # 1.3 - Imported data - Iniciado una sola vez
      output_list_database <- module_import_03_show_server(id = "act001_s99_03", list_sui_settings)
      
      
      output$show_dev <- renderUI({
        div(
          br(),
          # Tabla de datos
          h4("Primeras filas del conjunto de datos:"),
          tableOutput(ns("data_table"))
        )
        
      })
      # Mostrar las primeras filas de la tabla
      output$data_table <- renderTable({
        head(output_list_database()$"database", 5)
      })
      
      # Devolver solo los datos confirmados
      return(reactive(output_list_database()))
      
    }
  )
}
