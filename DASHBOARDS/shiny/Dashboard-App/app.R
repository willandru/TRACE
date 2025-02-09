library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(jsonlite)
library(RColorBrewer)

# Función para obtener las fechas de publicación de todas las versiones de un paquete
get_release_dates <- function(package_name) {
  url <- paste0("https://crandb.r-pkg.org/", package_name, "/all")
  response <- tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(response) && !is.null(response$timeline)) {
    timeline <- as.data.frame(response$timeline)
    return(data.frame(
      version = rownames(timeline),
      date = as.Date(timeline[, 1]),
      package = package_name
    ))
  }
  return(data.frame(version = NA, date = NA, package = package_name)) # Retornar NA si falla
}


LINE_WIDTH <- 0.7


# Interfaz del Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Epiverse R Packages"),
  dashboardSidebar(
    collapsed = TRUE,
    width = 130, # Ajusta el ancho del menú lateral
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Sivirep", tabName = "sivirep", icon = icon("chart-bar")),
      menuItem("Serofoi", tabName = "serofoi", icon = icon("chart-pie")),
      menuItem("Vaccineff", tabName = "vaccineff", icon = icon("syringe"))
    )
  ),
  dashboardBody(
    tabItems(
      # Primera pestaña: Inicio
      tabItem(tabName = "inicio",
              h2("Epiverse TRACE"),
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Seleccionar Fechas",
                    width = 12,
                    dateInput("start_date", "Fecha de inicio:", value = "2024-10-01", max = Sys.Date()),
                    dateInput("end_date", "Fecha de fin:", value = Sys.Date(), max = Sys.Date())
                  ),
                  box(
                    title = "Seleccionar Paquetes",
                    width = 12,
                    textInput("packages", "Paquetes (separados por comas):", value = "vaccineff, sivirep, serofoi"),
                    actionButton("update", "Actualizar Gráfico")
                  )
                ),
                column(
                  width = 8,
                  box(
                    title = "Tendencia de Descargas",
                    width = 12,
                    plotOutput("trend_plot", height = "300px") %>% withSpinner(color = "#0dc5c1") # Indicador de carga
                  ),
                  box(
                    title = "Descargas Acumuladas",
                    width = 12,
                    plotOutput("cumulative_plot", height = "300px") %>% withSpinner(color = "#0dc5c1") # Indicador de carga
                  ),
                  box(
                    title = "Tasa de Crecimiento",
                    width = 12,
                    plotOutput("growth_rate_plot", height = "300px") %>% withSpinner(color = "#0dc5c1") # Indicador de carga
                  )
                )
              )
      ),
      # Segunda pestaña: Sivirep
      tabItem(tabName = "sivirep",
              h2("Sivirep"),
              p("Próximamente...")
      ),
      # Tercera pestaña: Serofoi
      tabItem(tabName = "serofoi",
              h2("Serofoi"),
              p("Próximamente...")
      ),
      # Cuarta pestaña: Vaccineff
      tabItem(tabName = "vaccineff",
              h2("Vaccineff"),
              p("Próximamente...")
      )
    )
  )
)

# Lógica del servidor
# Lógica del servidor
server <- function(input, output, session) {
  
  # Datos reactivos: descargas diarias, dependientes de las fechas
  download_data <- reactive({
    # Validar fechas
    if (input$start_date > input$end_date) {
      showModal(modalDialog(
        title = "Error en Fechas",
        "La fecha de inicio no puede ser mayor a la fecha de fin.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return(NULL)
    }
    
    # Separar paquetes seleccionados
    input$update # Reactivo al botón
    packages <- isolate({
      strsplit(input$packages, ",\\s*")[[1]] %>% trimws()
    })
    
    # Descargar datos para las fechas seleccionadas
    tryCatch(
      cranlogs::cran_downloads(packages = packages, from = input$start_date, to = input$end_date),
      error = function(e) {
        data.frame(package = packages, date = NA, count = NA) # Retornar datos vacíos si falla
      }
    )
  })
  
  # Datos reactivos: fechas de publicación (dependen del botón)
  release_dates <- reactive({
    input$update # Reactivo al botón
    isolate({
      packages <- strsplit(input$packages, ",\\s*")[[1]] %>% trimws()
      do.call(rbind, lapply(packages, get_release_dates))
    })
  })
  
  
  
  
  
  #PLOT 1 ---------------------------------------------
  output$trend_plot <- renderPlot({
    data <- download_data() # Actualización dinámica por fechas
    releases <- release_dates() # Actualización manual por botón
    req(data, releases, nrow(data) > 0)
    
    paquetes <- unique(data$package) # Paquetes en los datos
    
    # Ajustar colores para 1 o 2 paquetes
    if (length(paquetes) < 3) {
      colores <- c("#66C2A5", "#FC8D62", "#8DA0CB")[1:length(paquetes)] # Paleta fija
    } else {
      colores <- brewer.pal(min(length(paquetes), 8), "Set2")
    }
    
    # Preparar datos para el gráfico
    plot_data <- list()
    vertical_segments <- list() # Datos para los segmentos verticales
    
    for (pkg in paquetes) {
      pkg_data <- data %>% filter(package == pkg)
      pkg_release <- releases %>% filter(package == pkg) %>% arrange(date) %>% slice(1) # Primer release
      release_date <- pkg_release$date[1]
      
      if (is.na(release_date)) next # Saltar si no hay release
      
      # Punto del lanzamiento si está en el rango
      if (release_date >= input$start_date & release_date <= input$end_date) {
        release_value <- pkg_data %>% filter(date == release_date)
        if (nrow(release_value) == 1) {
          # Agregar segmento vertical y punto sólido
          vertical_segments[[pkg]] <- data.frame(
            date = release_date,
            start_count = 0, # Desde 0 en el eje Y
            end_count = release_value$count[1],
            package = pkg
          )
          
          plot_data[[pkg]] <- list(
            point = data.frame(date = release_date, count = 0, package = pkg) # Fijar y = 0 para el punto sólido
          )
        }
      }
      
      # Dividir los datos en antes y después del lanzamiento
      if (input$start_date < release_date) {
        before_release <- pkg_data %>% filter(date < release_date)
        after_release <- pkg_data %>% filter(date >= release_date)
        
        # Añadir punto de conexión al 0 en el lanzamiento
        connection_point <- data.frame(date = release_date, count = 0, package = pkg)
        before_release <- bind_rows(before_release, connection_point)
        
        plot_data[[pkg]]$before <- before_release
        plot_data[[pkg]]$after <- after_release
      } else {
        plot_data[[pkg]]$single <- pkg_data
      }
    }
    
    # Crear el gráfico
    p <- ggplot()
    for (pkg in names(plot_data)) {
      if (!is.null(plot_data[[pkg]]$point)) {
        # Dibujar el punto sólido del primer release en el eje X (y = 0)
        p <- p + geom_point(
          data = plot_data[[pkg]]$point, # Asegurar y = 0
          aes(x = date, y = count, color = package),
          size = 3,
          shape = 21,
          fill = "white"
        )
      }
      
      if (!is.null(plot_data[[pkg]]$before) && nrow(plot_data[[pkg]]$before) > 1) {
        p <- p + geom_line(
          data = plot_data[[pkg]]$before,
          aes(x = date, y = count, color = package),
          linewidth = LINE_WIDTH,
          alpha = 0 # Transparente
        )
      }
      
      if (!is.null(plot_data[[pkg]]$after) && nrow(plot_data[[pkg]]$after) > 1) {
        p <- p + geom_line(
          data = plot_data[[pkg]]$after,
          aes(x = date, y = count, color = package),
          linewidth = LINE_WIDTH
        )
      }
      
      if (!is.null(plot_data[[pkg]]$single) && nrow(plot_data[[pkg]]$single) > 1) {
        p <- p + geom_line(
          data = plot_data[[pkg]]$single,
          aes(x = date, y = count, color = package),
          linewidth = LINE_WIDTH
        )
      }
    }
    
    # Dibujar los segmentos verticales desde 0 hasta el valor inicial
    for (pkg in names(vertical_segments)) {
      vertical_data <- vertical_segments[[pkg]]
      p <- p + geom_segment(
        data = vertical_data,
        aes(x = date, xend = date, y = start_count, yend = end_count, color = package),
        linewidth = LINE_WIDTH, # Línea sólida
        linetype = "dashed" # Tipo de línea sólida
      )
    }
    
  
    
    # Ajustar escala del eje X con intervalos dinámicos
    p <- p + scale_x_date(
      limits = as.Date(c(input$start_date, input$end_date)) # Límites dinámicos
                                # Intervalos dinámicos
                                    # Formato dinámico
    )
    
    # Finalizar el gráfico
    p + labs(title = "Tendencia de Descargas Diarias",
             x = "Fecha",
             y = "Descargas",
             color = "Paquete") +
      scale_color_manual(values = setNames(colores, paquetes)) +
      theme_minimal()
  })
  
  
  
  
#PLOT 2

output$cumulative_plot <- renderPlot({
  data <- download_data() # Fetch data dynamically
  releases <- release_dates() # Fetch release dates
  req(data, releases, nrow(data) > 0)
  
  # Identify all unique packages from data and releases
  all_packages <- unique(c(data$package, releases$package))
  
  # Assign consistent colors for all packages
  color_palette <- if (length(all_packages) < 3) {
    c("#66C2A5", "#FC8D62", "#8DA0CB")[1:length(all_packages)]
  } else {
    brewer.pal(min(length(all_packages), 8), "Set2")
  }
  package_colors <- setNames(color_palette, all_packages)
  
  # Parse user-input packages and find valid CRAN packages
  input_packages <- strsplit(input$packages, ",\\s*")[[1]] %>% trimws()
  valid_packages <- intersect(input_packages, all_packages)
  
  # If no valid packages, return an empty plot
  if (length(valid_packages) == 0) {
    return(ggplot() +
             labs(
               title = "No valid packages found on CRAN",
               x = "Fecha",
               y = "Descargas Acumuladas"
             ))
  }
  
  # Filter data for valid packages and input date range
  data <- data %>%
    filter(package %in% valid_packages) %>%
    filter(date >= input$start_date & date <= input$end_date)
  
  # Handle packages with no data in the date range
  missing_packages <- setdiff(valid_packages, unique(data$package))
  if (length(missing_packages) > 0) {
    data_sin_datos <- do.call(rbind, lapply(missing_packages, function(pkg) {
      data.frame(
        date = seq(as.Date(input$start_date), as.Date(input$end_date), by = "day"),
        count = 0,
        cumulative_count = 0,
        package = pkg
      )
    }))
  } else {
    data_sin_datos <- NULL
  }
  
  # Calculate cumulative downloads for packages with data
  cumulative_data <- data %>%
    group_by(package) %>%
    arrange(date) %>%
    mutate(cumulative_count = cumsum(count)) %>%
    ungroup()
  
  # Combine cumulative data with missing data
  if (!is.null(data_sin_datos)) {
    cumulative_data <- bind_rows(cumulative_data, data_sin_datos)
  }
  
  # Remove packages with only zero cumulative_count
  cumulative_data <- cumulative_data %>%
    group_by(package) %>%
    filter(any(cumulative_count > 0)) %>%
    ungroup()
  
  # Prepare release date information for valid packages
  valid_releases <- releases %>%
    filter(package %in% unique(cumulative_data$package)) %>%
    filter(date >= input$start_date & date <= input$end_date)
  
  # Add transparency for lines before the first release
  cumulative_data <- cumulative_data %>%
    left_join(valid_releases, by = "package", suffix = c("", "_release")) %>%
    mutate(
      line_alpha = ifelse(date < date_release, 0, 1) # Transparency before release
    )
  
  # Ensure vertical lines have correct y-axis limits
  max_y <- max(cumulative_data$cumulative_count, na.rm = TRUE)
  
  # Create the plot
  ggplot(cumulative_data, aes(x = date, y = cumulative_count, color = package)) +
    geom_line(aes(alpha = line_alpha), linewidth = LINE_WIDTH) + # Use alpha for transparency
    geom_point( # Add points for release dates
      data = valid_releases,
      aes(x = date, y = 0, color = package),
      size = 3,
      shape = 21,
      fill = "white"
    ) +
    geom_segment( # Add vertical lines for release dates
      data = valid_releases,
      aes(x = date, xend = date, y = 0, yend = max_y, color = package),
      linewidth = LINE_WIDTH,
      linetype = "dashed", 
      alpha =0
    ) +
    labs(
      title = "Descargas Acumuladas",
      x = "Fecha",
      y = "Descargas Acumuladas",
      color = "Paquete"
    ) +
    scale_color_manual(values = package_colors) + # Apply consistent colors
    scale_alpha_identity() + # Use pre-defined alpha values
    theme_minimal()
})


  

  
  
  
  ##PLOT 3-----------------------------------------------------------------------
## PLOT 3 -----------------------------------------------------------------------
output$growth_rate_plot <- renderPlot({
  # Descargar datos y fechas de releases
  data <- download_data()
  releases <- release_dates()
  req(data, releases, nrow(data) > 0)
  
  paquetes <- unique(data$package) # Paquetes seleccionados
  
  # Filtrar fechas según input
  start_date <- input$start_date
  end_date <- input$end_date
  
  # Unir datos con la fecha de lanzamiento
  data <- data %>%
    left_join(releases, by = "package") %>%
    filter(!is.na(date.x) & !is.na(date.y)) %>% # Validar fechas
    mutate(
      days_in_cran = as.numeric(date.x - date.y), # Días desde el lanzamiento
      growth_rate = ifelse(days_in_cran >= 0, count / (days_in_cran + 1), NA) # Evitar división por 0
    ) %>%
    filter(date.x >= start_date & date.x <= end_date) # Filtrar por rango input
  
  # Filtrar fechas de primer release dentro del rango
  valid_releases <- releases %>%
    filter(date >= start_date & date <= end_date)
  
  # Datos para las líneas verticales (solo lanzamientos dentro del rango)
  vertical_segments <- data %>%
    filter(date.x == date.y & date.x >= start_date & date.x <= end_date) %>%
    group_by(package) %>%
    summarize(
      date = date.x[1],
      growth_rate = growth_rate[1]
    )
  
  # Manejo de colores
  colores <- if (length(paquetes) < 3) {
    c("#66C2A5", "#FC8D62", "#8DA0CB")[1:length(paquetes)]
  } else {
    brewer.pal(min(length(paquetes), 8), "Set2")
  }
  
  # Crear el gráfico
  ggplot(data, aes(x = date.x, y = growth_rate, color = package)) +
    geom_line(linewidth = LINE_WIDTH
              ) + # Línea de tasa de crecimiento
    geom_point( # Punto sólido en la fecha de primer release
      data = vertical_segments,
      aes(x = date, y = 0, color = package),
      size = 3,
      shape = 21,
      fill = "white"
    ) +
    geom_segment( # Línea vertical sólida desde y = 0 hasta la tasa de crecimiento
      data = vertical_segments,
      aes(x = date, xend = date, y = 0, yend = growth_rate, color = package),
      linewidth = LINE_WIDTH,
      linetype = "dashed",
    ) +
    labs(
      title = "Tasa de Crecimiento",
      x = "Fecha",
      y = "Tasa de Crecimiento (descargas/días en CRAN)",
      color = "Paquete"
    ) +
    scale_color_manual(values = setNames(colores, paquetes)) +
    theme_minimal() +
    scale_y_log10()
})
  
  
  
  
}

# Ejecutar la app
shinyApp(ui, server)
