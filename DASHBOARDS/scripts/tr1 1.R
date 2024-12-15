# Función corregida para CRAN Archive
get_archive_dates <- function(package) {
  archive_url <- paste0("https://cran.r-project.org/src/contrib/Archive/", package, "/")
  
  tryCatch({
    page <- read_html(archive_url)
    files <- page %>%
      html_elements("a") %>%
      html_text()
    
    # Filtrar las versiones del paquete
    files <- grep(paste0("^", package, "_"), files, value = TRUE)
    if (length(files) == 0) return(data.frame(Package = package, Version = NA, Published = NA))
    
    dates <- page %>%
      html_elements("td:nth-child(2)") %>%  # Fecha modificada
      html_text(trim = TRUE)
    dates <- as.Date(dates, format = "%Y-%m-%d")
    
    data.frame(
      Package = package,
      Version = sub(paste0(package, "_([0-9.]+)\\.tar\\.gz"), "\\1", files),
      Published = dates
    )
  }, error = function(e) {
    message(paste("No archive found for package:", package))
    return(data.frame(Package = package, Version = NA, Published = NA))
  })
}

# Función corregida para CRAN principal
get_current_date <- function(package) {
  url <- paste0("https://cran.r-project.org/web/packages/", package, "/index.html")
  
  tryCatch({
    page <- read_html(url)
    published_date <- page %>%
      html_nodes(xpath = "//table//tr[th[text()='Published']]/td") %>%
      html_text(trim = TRUE)
    
    if (length(published_date) == 0) return(data.frame(Package = package, Published = NA))
    
    data.frame(Package = package, Version = NA, Published = as.Date(published_date))
  }, error = function(e) {
    message(paste("Error retrieving data for package:", package))
    return(data.frame(Package = package, Published = NA))
  })
}

# Obtener datos
archive_data <- do.call(rbind, lapply(requested_packages, get_archive_dates))
current_data <- do.call(rbind, lapply(requested_packages, get_current_date))

# Combinar y filtrar
combined_data <- bind_rows(archive_data, current_data) %>%
  group_by(Package) %>%
  filter(!is.na(Published)) %>%
  summarise(FirstPublished = min(Published, na.rm = TRUE), .groups = "drop")

# Mostrar resultados
print(combined_data)
