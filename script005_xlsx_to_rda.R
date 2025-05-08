# Cargar las librerías necesarias
library(readxl)
library(tools)
library(purrr)

# Función para convertir archivos Excel a RDA
excel_to_rda <- function(input_dir = "data-raw", output_dir = "data", compress = "xz") {
  # Asegurarse de que los directorios existen
  if (!dir.exists(input_dir)) {
    stop(paste("El directorio de entrada", input_dir, "no existe"))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(paste("Se ha creado el directorio de salida:", output_dir))
  }
  
  # Listar todos los archivos Excel en el directorio de entrada
  excel_files <- list.files(
    path = input_dir,
    pattern = "\\.xlsx$|\\.xls$",
    full.names = TRUE
  )
  
  if (length(excel_files) == 0) {
    message(paste("No se encontraron archivos Excel en", input_dir))
    return(invisible())
  }
  
  # Procesar cada archivo Excel
  purrr::walk(excel_files, function(file_path) {
    tryCatch({
      # Obtener el nombre base del archivo (sin extensión)
      file_name <- tools::file_path_sans_ext(basename(file_path))
      
      # Limpiar el nombre para que sea válido como objeto de R
      object_name <- gsub("[^a-zA-Z0-9_]", "_", file_name)
      
      # Leer el archivo Excel
      message(paste("Leyendo", file_path, "..."))
      data <- readxl::read_excel(file_path)
      
      # Asignar datos al objeto con el nombre deseado
      assign(object_name, data)
      
      # Ruta de salida para el archivo .rda
      rda_file <- file.path(output_dir, paste0(object_name, ".rda"))
      
      # Guardar el objeto como RDA
      save(
        list = object_name,
        file = rda_file,
        compress = compress
      )
      
      # Mensaje de éxito
      message(paste(
        "✓ Convertido:", file_path,
        "\n  → Guardado como:", rda_file,
        "\n  → Nombre del objeto:", object_name,
        "\n  → Dimensiones:", paste(dim(data), collapse = " x "),
        "\n  → Tamaño del archivo:", format(file.size(rda_file) / 1024, digits = 2), "KB"
      ))
    }, error = function(e) {
      warning(paste("Error al procesar", file_path, ":", e$message))
    })
  })
  
  message(paste("Conversión completada. Se procesaron", length(excel_files), "archivos."))
  
  # Devolver las rutas de los archivos procesados
  return(excel_files)
}

# ------------------------------------------------------
# Ejemplo de uso:
# ------------------------------------------------------

# Ejecutar la función para convertir todos los archivos Excel de data-raw a data
excel_to_rda()

# Si tus directorios tienen una ruta diferente, puedes especificarla:
# excel_to_rda(input_dir = "ruta/a/data-raw", output_dir = "ruta/a/data")

# Si deseas usar una compresión diferente:
# excel_to_rda(compress = "gzip") # Alternativas: "xz" (por defecto), "gzip", "bzip2", "none"
