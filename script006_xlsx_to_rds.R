# Cargar las librerías necesarias
library(readxl)
library(tools)
library(purrr)

# Función para convertir archivos Excel a RDS
excel_to_rds <- function(input_dir = "data-raw", output_dir = "data", compress = "xz") {
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
  processed_files <- purrr::map_chr(excel_files, function(file_path) {
    tryCatch({
      # Obtener el nombre base del archivo (sin extensión)
      file_name <- tools::file_path_sans_ext(basename(file_path))
      
      # Limpiar el nombre para que sea válido como objeto de R
      object_name <- gsub("[^a-zA-Z0-9_]", "_", file_name)
      
      # Leer el archivo Excel
      message(paste("Leyendo", file_path, "..."))
      data <- readxl::read_excel(file_path)
      
      # Ruta de salida para el archivo .rds
      rds_file <- file.path(output_dir, paste0(object_name, ".rds"))
      
      # Guardar el objeto como RDS
      saveRDS(
        object = data,
        file = rds_file,
        compress = compress
      )
      
      # Mensaje de éxito
      message(paste(
        "✓ Convertido:", file_path,
        "\n  → Guardado como:", rds_file,
        "\n  → Dimensiones:", paste(dim(data), collapse = " x "),
        "\n  → Tamaño del archivo:", format(file.size(rds_file) / 1024, digits = 2), "KB"
      ))
      
      # Devolver la ruta del archivo RDS creado
      return(rds_file)
    }, error = function(e) {
      warning(paste("Error al procesar", file_path, ":", e$message))
      return(NA_character_)
    })
  })
  
  # Filtrar archivos que no se procesaron correctamente
  processed_files <- processed_files[!is.na(processed_files)]
  
  message(paste("Conversión completada. Se procesaron", length(processed_files), "archivos."))
  
  # Devolver las rutas de los archivos procesados
  return(processed_files)
}

# ------------------------------------------------------
# Ejemplo de uso:
# ------------------------------------------------------

# Ejecutar la función para convertir todos los archivos Excel de data-raw a data
excel_to_rds()

# Si tus directorios tienen una ruta diferente, puedes especificarla:
# excel_to_rds(input_dir = "ruta/a/data-raw", output_dir = "ruta/a/data")

# Si deseas usar una compresión diferente:
# excel_to_rds(compress = "gzip") # Alternativas: "xz" (por defecto), "gzip", "bzip2", "none"

# Para cargar los datos RDS más tarde:
# mi_dataframe <- readRDS("data/nombre_archivo.rds")
