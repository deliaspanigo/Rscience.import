

devtools::load_all()
devtools::build()     # Arma el paquete para mi
devtools::document()  # Actualiza documentación
devtools::test()      # Ejecuta pruebas
devtools::check()     # Verifica el paquete
# Instalar el paquete
devtools::install()

######################################
# Desinstalar el paquete
remove.packages("Rscience.import")

# Limpiar el caché de devtools
devtools::clean_dll()

# Regenerar documentación
devtools::document()

# Instalar nuevamente
devtools::install()
########################################
library(Rscience.import)
Rscience.import::run_app()
