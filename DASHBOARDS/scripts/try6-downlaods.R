# Instalar y cargar cranlogs si no está disponible
if (!requireNamespace("cranlogs", quietly = TRUE)) {
  install.packages("cranlogs")
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

# Ejemplo de uso
packages <- c("vaccineff", "serofoi", "sivirep")
from_date <- "2024-11-01"
to_date <- "2024-12-13"

# Obtener descargas diarias para cada paquete
for (pkg in packages) {
  cat("\nPaquete:", pkg, "\n")
  downloads <- get_daily_downloads(pkg, from = from_date, to = to_date)
  
  if (nrow(downloads) > 0 && !is.na(downloads$date[1])) {
    apply(downloads, 1, function(row) {
      cat("  Fecha:", row["date"], " - Descargas:", row["count"], "\n")
    })
  } else {
    cat("  No se encontraron descargas o el paquete no existe.\n")
  }
}
