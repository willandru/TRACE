# Instalar paquetes si no están instalados
if (!requireNamespace("tools", quietly = TRUE)) {
  install.packages("tools")
}
if (!requireNamespace("cranlogs", quietly = TRUE)) {
  install.packages("cranlogs")
}

# Lista de paquetes a consultar
packages <- c("vaccineff", "serofoi", "sivirep")

# Función para consultar metadatos desde CRAN
get_package_metadata <- function(package_name) {
  url <- paste0("https://crandb.r-pkg.org/", package_name, "/all")
  response <- tryCatch(
    {
      jsonlite::fromJSON(url)
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (!is.null(response) && !is.null(response$timeline)) {
    versions <- names(response$timeline)
    dates <- unlist(response$timeline)
    return(data.frame(Version = versions, Date = dates, stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

# Consultar paquetes
for (pkg in packages) {
  cat("\nPaquete:", pkg, "\n")
  
  metadata <- get_package_metadata(pkg)
  
  if (!is.null(metadata)) {
    apply(metadata, 1, function(row) {
      cat("  Versión:", row["Version"], "\n")
      cat("  Fecha de publicación:", row["Date"], "\n")
    })
  } else {
    cat("  No se encontraron versiones publicadas o el paquete no existe en CRAN.\n")
  }
}
