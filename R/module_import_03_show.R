#' @export
module_import_03_show_ui <- function(id) {
  ns <- shiny::NS(id)
  ""
  # bslib::card(
  #   bslib::card_header("Importación de datos"),
  #   bslib::card_body(
  #     uiOutput(ns("salida_general"))
  #   ),
  #   full_screen = TRUE
  # )
}

#' @export
module_import_03_show_server <- function(id, list_sui_settings) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server
      ns <- session$ns
      
      # Validación de configuraciones
      check_ok <- reactive({
        req(list_sui_settings())
        TRUE
      })
      
      # Inicializar valores reactivos
      data_source        <- reactiveVal("No details")
      database <- reactiveVal(NULL)
      temporal_file_path <- reactiveVal("No details")
      original_file_name <- reactiveVal("No details")
      str_import   <- reactiveVal("No details")
      info_extra   <- reactiveVal("No details")
      error_message <- reactiveVal(NULL) # Para almacenar mensajes de error
      
      # Función para mostrar pop-up de error
      show_error_popup <- function(error_msg) {
        showModal(
          modalDialog(
            title = "Error de validación",
            HTML(error_msg),
            easyClose = TRUE,
            footer = modalButton("Cerrar"),
            size = "m",
            style = "color: #721c24; background-color: #f8d7da; border-color: #f5c6cb;"
          )
        )
        
        # También guardar el mensaje para mostrar en la UI principal
        error_message(error_msg)
      }
      
      # Observar cambios en la fuente de datos
      observe({
        req(check_ok(), list_sui_settings())
        
        # Resetear el mensaje de error cuando cambia la selección
        error_message(NULL)
        
        # Actualizar el valor reactivo de data_source
        data_source(list_sui_settings()$"data_source")
        
        selected_file <- list_sui_settings()$"selected_input_file"
        
        req(selected_file)
        
        # Lógica según la fuente de datos
        if(data_source() == "source_Rdata") {
          tryCatch({
            database(get(selected_file))
            original_file_name(selected_file)
            info_extra("No details")
          }, error = function(e) {
            error_msg <- paste("Error al cargar el archivo RData:", e$message)
            show_error_popup(error_msg)
          })
        } else if(data_source() == "source_Rscience") {
          tryCatch({
            
            
            if(TRUE) {
              datasets_info <- data(package = "Rscience.import")
              dataset_names_Rscience <- datasets_info$results[, "Item"]
#              print(selected_file)
 #             print()
              if(selected_file %in% dataset_names_Rscience) {
                mi_armado <- paste0("Rscience.import::", selected_file)
                database(eval(parse(text = mi_armado)))
                original_file_name(selected_file)
                info_extra("No details")
              } else {
                error_msg <- paste("El dataset", selected_file, "no existe en data_list_Rscience")
                show_error_popup(error_msg)
              }
            } else {
              error_msg <- "El objeto data_list_Rscience no existe en el entorno global"
              show_error_popup(error_msg)
            }
          }, error = function(e) {
            error_msg <- paste("Error al cargar el dataset Rscience:", e$message)
            show_error_popup(error_msg)
          })
        } else if(data_source() == "source_xlsx") {
          # Lógica para archivos Excel - SIN las validaciones que ahora están en el otro módulo
          tryCatch({
            # Configurar las rutas y los detalles de importación
            temporal_file_path(list_sui_settings()$list_extra$"temporal_file_path")
            original_file_name(list_sui_settings()$list_extra$"original_file_name")
            info_extra(list_sui_settings()$list_extra$"xlsx_file_details")
            
            # Ya no hay validaciones aquí, asumimos que el archivo ya fue validado
            
            # Configurar la importación
            str_import("readxl::read_excel(path = '_my_path_', sheet = 1)")
            
            # Construir y evaluar la expresión de lectura del archivo Excel
            my_text <- str_import()
            my_text <- sub(pattern = "_my_path_", replacement = temporal_file_path(), x = my_text)
            
            # Cargar los datos del archivo Excel con mejor manejo de errores
            withCallingHandlers({
              imported_data <- eval(parse(text=my_text))
              # Convertir a data.frame estándar (read_excel devuelve un tibble)
              database(as.data.frame(imported_data))
              
              # Notificación de éxito
              showNotification(
                paste("Archivo Excel importado correctamente:", original_file_name()),
                type = "message",
                duration = 5
              )
            }, warning = function(w) {
              showNotification(
                paste("Advertencia al importar Excel:", w$message),
                type = "warning",
                duration = 10
              )
              invokeRestart("muffleWarning")
            })
            
          }, error = function(e) {
            error_msg <- paste("Error al cargar el archivo Excel:", e$message)
            show_error_popup(error_msg)
          })
        }
      })
      
      # Mostrar información sobre los datos cargados o mensaje de error
      output$salida_general <- renderUI({
        # Si hay un mensaje de error, mostrar el mensaje de error
        if (!is.null(error_message())) {
          return(
            bslib::layout_column_wrap(
              width = 1,
              bslib::value_box(
                title = "Origen de datos",
                value = data_source(),
                showcase = bsicons::bs_icon("database")
              ),
              bslib::value_box(
                title = "Archivo seleccionado",
                value = original_file_name(),
                showcase = bsicons::bs_icon("file-earmark")
              ),
              bslib::card(
                bslib::card_header("Error de validación"),
                bslib::card_body(
                  tags$div(
                    class = "alert alert-danger",
                    style = "color: #721c24; background-color: #f8d7da; border-color: #f5c6cb; padding: 15px; border-radius: 5px;",
                    HTML(error_message())
                  )
                )
              )
            )
          )
        }
        
        # Si no hay error pero hay datos, mostrar la información normal
        #req(database())
        #
        # bslib::layout_column_wrap(
        #   width = 1,
        #   bslib::value_box(
        #     title = "Origen de datos",
        #     value = data_source(),
        #     showcase = bsicons::bs_icon("database")
        #   ),
        #   bslib::value_box(
        #     title = "Archivo seleccionado",
        #     value = original_file_name(),
        #     showcase = bsicons::bs_icon("file-earmark")
        #   ),
        #   bslib::card(
        #     bslib::card_header("Estructura de datos"),
        #     bslib::card_body(
        #       verbatimTextOutput(ns("str_output"))
        #     )
        #   )
        # )
      })
      
      # Mostrar la estructura de los datos
      output$str_output <- renderPrint({
        req(database())
        str(database())
      })
      
      # Construir la lista de salida
      output_list <- reactive({
        # No requerir database() aquí para permitir devolver aunque haya error
        list(
          "data_source" = data_source(),
          "temporal_file_path" = temporal_file_path(),
          "original_file_name" = original_file_name(),
          "str_import" = str_import(),
          "info_extra" = info_extra(),
          "database" = database(),
          "error_message" = error_message()  # Añadido error_message a la lista de salida
        )
      })
      
      # Devolver la lista de salida
      return(output_list)
    }
  )
}
