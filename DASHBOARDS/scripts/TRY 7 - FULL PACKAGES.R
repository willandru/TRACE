# Instalar paquetes si no están instalados
if (!requireNamespace("tools", quietly = TRUE)) {
  install.packages("tools")
}
if (!requireNamespace("cranlogs", quietly = TRUE)) {
  install.packages("cranlogs")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

# Lista de paquetes a consultar
packages <- c(
  "readepi", "ColOpenData", "cleanepi", "simulist", "linelist", "epiparameter",
  "sivirep", "episoap", "cfr", "serofoi", "epiCo", "iraca", "epichains",
  "superspreading", "finalsize", "epidemics", "vaccineff"
)

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
    return(data.frame(Package = package_name, Version = versions, Date = dates, stringsAsFactors = FALSE))
  } else {
    return(data.frame(Package = package_name, Version = NA, Date = NA, stringsAsFactors = FALSE))
  }
}

# Función para obtener las descargas diarias
get_daily_downloads <- function(package_name, from = Sys.Date() - 7, to = Sys.Date()) {
  tryCatch(
    {
      cranlogs::cran_downloads(
        packages = package_name,
        from = from,
        to = to
      )
    },
    error = function(e) {
      data.frame(
        package = package_name,
        date = NA,
        count = NA
      )
    }
  )
}

# Obtener metadatos y descargas para cada paquete
all_metadata <- do.call(rbind, lapply(packages, get_package_metadata))
print("Tabla de versiones y fechas de publicación:")
print(all_metadata)

# Obtener y mostrar descargas diarias para los últimos 7 días
cat("\nDescargas diarias (últimos 7 días):\n")
for (pkg in packages) {
  cat("\nPaquete:", pkg, "\n")
  
  # Obtener las descargas diarias
  downloads <- get_daily_downloads(pkg, from = Sys.Date() - 7, to = Sys.Date())
  
  if (nrow(downloads) > 0 && !is.na(downloads$date[1])) {
    apply(downloads, 1, function(row) {
      cat("  Fecha:", row["date"], " - Descargas:", row["count"], "\n")
    })
  } else {
    cat("  No se encontraron descargas o el paquete no tiene descargas en los últimos 7 días.\n")
  }
}
