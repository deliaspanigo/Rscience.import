
#' @export
module_import_02_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    module_import_xlsx_ui(id =   ns("Aspace02_database_01")),
    module_import_csv_ui(id =    ns("Aspace02_database_02")),
    module_import_Rscience_ui(id = ns("Aspace02_database_03")),
    module_import_Rdata_ui(id =  ns("Aspace02_database_04"))
  )
  
}



#' @export
module_import_02_settings_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      # 1.2 - Settings para cada fuente de datos.
      the_01_xlsx_settings   <- module_import_xlsx_server(id =   "Aspace02_database_01", sui_data_source)
      the_02_csv_settings    <- module_import_csv_server(id =    "Aspace02_database_02", sui_data_source)
      the_03_Rscience_settings <- module_import_Rscience_server(id = "Aspace02_database_03", sui_data_source)
      the_04_Rdata_settings  <- module_import_Rdata_server(id =  "Aspace02_database_04", sui_data_source)
      
      # https://gallery.shinyapps.io/assistant/?_gl=1*slchuy*_ga*MTQ4NTM0MTYxMC4xNzQ0ODMzMDEy*_ga_2C0WZ1JHG0*czE3NDQ4OTg3NDEkbzIkZzEkdDE3NDQ4OTkwNTkkajAkbDAkaDA.#
      control_only_one_alive <- reactive({
        
        vector_status <- c(!is.null(the_01_xlsx_settings()),
                           !is.null(the_02_csv_settings()),
                           !is.null(the_03_Rscience_settings()),
                           !is.null(the_04_Rdata_settings()))
        
        check_only_one <- sum(vector_status) <= 1
        
        return(check_only_one)
        
      })  
      
      
      # Lista con el settings para realizar la importacion
      list_sui_settings <- reactive({
        req(sui_data_source(), control_only_one_alive())
        
        
        
        el_elegido <- switch(sui_data_source(),
                             "source_xlsx"   = the_01_xlsx_settings(),
                             "source_csv"    = the_02_csv_settings(),
                             "source_Rscience" = the_03_Rscience_settings(),
                             "source_Rdata"  = the_04_Rdata_settings(),
                             # Valor por defecto o manejo de error
                             stop("Fuente de datos no reconocida:", sui_data_source())
        )
        
        
        return(el_elegido)
        # NULL
      })
      
      
      return(list_sui_settings)
      
    }
  )
}



#######################################################################################
