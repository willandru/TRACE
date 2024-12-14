# Cargar librería necesaria
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

# Función para obtener la información de todas las versiones de un paquete
get_package_versions <- function(package_name) {
  url <- paste0("https://crandb.r-pkg.org/", package_name, "/all")
  response <- tryCatch(
    {
      jsonlite::fromJSON(url)
    },
    error = function(e) {
      NULL
    },
    warning = function(w) {
      NULL
    }
  )
  
  if (!is.null(response) && !is.null(response$timeline)) {
    versions <- names(response$timeline)
    dates <- unlist(response$timeline)
    return(data.frame(Version = versions, Date = dates, stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

# Lista de paquetes a consultar
packages <- c("vaccineff", "serofoi", "sivirep")

# Consultar datos para cada paquete
for (package in packages) {
  cat("\nPaquete:", package, "\n")
  
  versions <- get_package_versions(package)
  if (!is.null(versions)) {
    apply(versions, 1, function(row) {
      cat("  Versión:", row["Version"], "\n")
      cat("  Fecha de publicación:", row["Date"], "\n")
    })
  } else {
    cat("  No se encontraron versiones publicadas.\n")
  }
}
