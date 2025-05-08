#' @export
module_import_01_options_ui <- function(id){
  ns <- shiny::NS(id)
  
  
  
  div(
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "inst/estilos.css"),
      tags$style(HTML("
      .shiny-output-error-AVISO {
        color: #0000ff;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
      .shiny-output-error-ERROR {
        color: #ff0000;
        font-weight: bold;
      }
    ")),
      tags$style(HTML("
        .content-wrapper, .right-side {
          overflow-y: hidden !important;
        }
      "))
    ),
    
    
    
    id = ns("input-panel"),
    #shiny::h1("Selección de base de datos"),
    shiny::fluidRow(
      shiny::column(12,
                    
                    uiOutput(ns("box01_database")),
                    shiny::br(),
                    shiny::br()
                    
      )
    )
  ) # End div
}



#' @export
module_import_01_options_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      output$box01_database <- renderUI({
        
        
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
      
      sui_data_source <- reactive({
        req(input$sui_data_source)
        input$sui_data_source
        
      })
      
      return(sui_data_source)
    }
  )
}



#######################################################################################
