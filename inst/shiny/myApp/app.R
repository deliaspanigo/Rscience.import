# Aplicación Shiny básica que usa mtcars
library(shiny)
library(purrr)
library(Rscience.import)
library(bslib)

# UI: Interfaz de usuario simple



 
ui <- fluidPage(
  title = "Selección de Base de Datos",

  
  MASTER_module_import_ui(id = "MASTER_import")
    
    
  
)

# Server: Lógica del servidor
server <- function(input, output) {
  
  output_list_database <- MASTER_module_import_server(id = "MASTER_import")
 
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
