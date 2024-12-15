# Usar la base de datos de CRAN
cran_db <- tools::CRAN_package_db()

# Filtrar paquetes de interés
requested_packages <- c("sivirep", "vaccineff", "serofoi")
current_data <- cran_db %>%
  filter(Package %in% requested_packages) %>%
  select(Package, Version, Published)

# Mostrar resultados
print(current_data)
