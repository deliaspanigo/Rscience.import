library(shiny)
library(bslib)
library(datasets)

# Definir la interfaz de usuario
ui <- page_sidebar(
  title = "Selección de Base de Datos",
  sidebar = sidebar(
    # Controles de entrada
    h3("Opciones"),
    uiOutput("menu_source"),
    uiOutput("ui_general"),
    
    uiOutput("ui_button")
  ),
  
  # Panel principal
  card(
    card_header("Información"),
    verbatimTextOutput("info")
  ),
  
  card(
    card_header("Vista Previa"),
    tableOutput("data_preview")
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  output$menu_source <- renderUI({
  
    vector_opt <- c("source_xlsx", "source_csv", "source_Rscience", "source_Rdata")
    
    selectInput("data_source", "Elegir base de datos:", 
                choices = vector_opt,
                selected = vector_opt[1])
    
  })
  
  
  output$menu_Rscience <- renderUI({
    # Selector de base de datos
    selectInput("selected_science", "Elegir base de datos:", 
                choices = c("ToothGrowth", "CO2"),
                selected = "ToothGrowth")
    
  })
  
  
  output$menu_Rdata <- renderUI({
    # Selector de base de datos
    selectInput("selected_Rdata", "Elegir base de datos:", 
                choices = c("iris", "mtcars"),
                selected = "iris")
    
  })
  
  
  output$menu_xlsx <- renderUI({
    # Selector de base de datos
    # Selector de archivo Excel
    fileInput("excel_file", "Seleccionar archivo Excel:",
              multiple = FALSE,
              accept = c(".xlsx", ".xls"))
    
    
  })
  
  output$ui_general <- renderUI({
    
    switch(input$"data_source", 
            'source_xlsx' = uiOutput("menu_xlsx"),
            'source_Rscience' = uiOutput("menu_Rscience"),
            'source_Rdata' = uiOutput("menu_Rdata")
    )
    
    # conditionalPanel(condition = "'source_Rdata' == input.source_data" ,
    #                  uiOutput("menu_Rdata"))
  })
  ##############################################################################
  output$ui_button <- renderUI({
    # Botón para cargar
    actionButton("load_btn", "Cargar Base de Datos", 
                 class = "btn-primary", width = "100%")
  })
  ##############################################################################
  
  list_sentence_import <- list(
    "source_xlsx" = "openxlsx::read.xlsx(xlsxFile = '_my_choose_', sheet = 1)",
    "source_Rscience" = "get(Rscience.import::_my_choose_)",
    "source_RData" = "get('_my_choose_')",
  )
  
  list_default_import <- list(
    "data_source" = NA,
    "check_data_source" = FALSE,
    "selected_input_file" = NA,
    "temporal_file_path" = NA,
    "original_file_name" = NA,
    "check_file" = FALSE,
    
    "str_import_internal" = NA,
    "str_import_external" = NA,
    "info_extra" = NA,
    
    "database" = NA,
    "button_state" = "initial",
    "button_disabled" = TRUE,
    "button_clicked" = FALSE,
    "error_message" = NA
  )
  
  ._data_source <- reactiveVal(NULL)
  ._check_data_source <- reactiveVal(NULL)
  ._selected_input_file <- reactiveVal(NULL)
  ._temporal_file_path  <- reactiveVal(NULL)
  ._original_file_name  <- reactiveVal(NULL)
  ._str_import_internal <- reactiveVal(NULL)
  ._str_import_internal <- reactiveVal(NULL)
  
  ._check_file <- reactiveVal()
  
  observeEvent(input$data_source,{
    
    if(input$data_source == "source_Rdata"){
      
      INT_selected_input_file   <-  input$"selected_Rdata"
      INT_temporal_file_path    <-  ""
      INT_original_file_name    <-  input$"selected_Rdata"
      INT_str_import_internal   <-  list_sentence_import[[input$"selected_Rdata"]]
      INT_original_file_name    <-  list_sentence_import[[input$"selected_Rdata"]]
      
      INT_original_file_name   <- input$"selected_Rdata"
      
    }
    
    ._selected_input_file(INT_selected_input_file)
    
  })
  
  list_all <- reactive({
  
   
    lista_nueva <- list_default_import
    
    lista_nueva$"data_source" <- input$data_source
    lista_nueva$"check_data_source" <- input$data_source != "" && !is.null(input$data_source)
    if(!lista_nueva$"check_data_source") return(list_nueva)
    
    lista_nueva$"selected_input_file" <- selected_input_file()
    
  })
  
  
  

  # Variable reactiva para controlar si los datos se muestran
  show_data <- reactiveVal(FALSE)
  
  # Variable reactiva para almacenar los datos cargados
  data_storage <- reactiveVal(NULL)
  
  # Observar cambios en la selección de la base de datos y resetear
  observeEvent(input$dataset, {
    # Si cambia la base de datos, resetear la visualización
    show_data(FALSE)
    # También resetear los datos almacenados a NULL
    data_storage(NULL)
  })
  
  # Observar clics en el botón y actualizar
  observeEvent(input$load_btn, {
    # Cuando se hace clic en el botón, mostrar los datos
    show_data(TRUE)
    # Cargar los datos
    dataset_name <- input$dataset
    data_storage(get(dataset_name))
  })
  
  # Datos reactivos que dependen del estado de show_data
  loaded_data <- reactive({
    # Si show_data es FALSE, devolver NULL
    if (!show_data()) {
      return(NULL)
    }
    
    # Si no, devolver los datos almacenados
    return(data_storage())
  })
  
  observeEvent(loaded_data(), {
    list_activated_import$"database" <- loaded_data()
  })
  
  
  # Mostrar información sobre la base de datos cargada
  output$info <- renderPrint({
    # Si no se deben mostrar datos, mostrar mensaje
    if (!show_data()) {
      return("Por favor, selecciona una base de datos y haz clic en 'Cargar Base de Datos'.")
    }
    
    # Verificar que loaded_data() no sea NULL
    req(loaded_data())
    
    # Mostrar información sobre la base cargada
    cat("Base de datos cargada:", input$dataset, "\n")
    cat("Número de filas:", nrow(loaded_data()), "\n")
    cat("Número de columnas:", ncol(loaded_data()), "\n")
  })
  
  # Mostrar vista previa de la base de datos
  output$data_preview <- renderTable({
    head(list_activated_import$"database", 10)
    # # Verificar que loaded_data() no sea NULL
    # req(loaded_data())
    # 
    # # Mostrar las primeras filas de la base de datos
    # head(loaded_data(), 10)
  })
}

# Crear la aplicación Shiny
shinyApp(ui, server)
