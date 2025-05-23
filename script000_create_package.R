

# Carga las librerías necesarias
library(devtools)
library(usethis)

# Crea la estructura básica del paquete (reemplaza "miPaquete" con el nombre de tu paquete)
create_package("Rscience2")


# Configura el uso de roxygen2 para documentación
usethis::use_roxygen_md()

# Configura testthat para testing
usethis::use_testthat()

# Configura el archivo README.md
usethis::use_readme_md()

# Configura la licencia (ejemplo: MIT)
usethis::use_mit_license()

# Configura la información de autor
usethis::use_package_doc()
