# Cargar la base de datos de paquetes de CRAN
cran_db <- tools::CRAN_package_db()

# Filtrar los paquetes de interés
requested_packages <- c("sivirep", "vaccineff", "serofoi")
filtered_data <- cran_db[cran_db$Package %in% requested_packages, c("Package", "Version", "Published")]

# Mostrar los resultados
print(filtered_data)
