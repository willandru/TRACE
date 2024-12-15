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
                    dateInput("start_date", "Fecha de inicio:", value = "2024-11-06", max = Sys.Date()),
                    dateInput("end_date", "Fecha de fin:", value = Sys.Date(), max = Sys.Date())
                  ),
                  box(
                    title = "Seleccionar Paquetes",
                    width = 12,
                    textInput("packages", "Paquetes (separados por comas):", value = "sivirep, vaccineff, serofoi"),
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
          linewidth = 1.2,
          alpha = 0.15 # Transparente
        )
      }
      
      if (!is.null(plot_data[[pkg]]$after) && nrow(plot_data[[pkg]]$after) > 1) {
        p <- p + geom_line(
          data = plot_data[[pkg]]$after,
          aes(x = date, y = count, color = package),
          linewidth = 1.2
        )
      }
      
      if (!is.null(plot_data[[pkg]]$single) && nrow(plot_data[[pkg]]$single) > 1) {
        p <- p + geom_line(
          data = plot_data[[pkg]]$single,
          aes(x = date, y = count, color = package),
          linewidth = 1.2
        )
      }
    }
    
    # Dibujar los segmentos verticales desde 0 hasta el valor inicial
    for (pkg in names(vertical_segments)) {
      vertical_data <- vertical_segments[[pkg]]
      p <- p + geom_segment(
        data = vertical_data,
        aes(x = date, xend = date, y = start_count, yend = end_count, color = package),
        linewidth = 1.2, # Línea sólida
        linetype = "solid" # Tipo de línea sólida
      )
    }
    
    # Finalizar el gráfico
    p + labs(title = "Tendencia de Descargas Diarias",
             x = "Fecha",
             y = "Descargas",
             color = "Paquete") +
      scale_color_manual(values = setNames(colores, paquetes)) +
      theme_minimal()
  })
  








  
  
  
  
  
  
  output$cumulative_plot <- renderPlot({
    data <- download_data() # Datos dinámicos basados en fechas
    releases <- release_dates() # Fechas de lanzamientos de los paquetes
    req(data, releases, nrow(data) > 0)
    
    paquetes <- unique(data$package) # Paquetes disponibles en los datos
    
    # Ajustar colores para 1 o 2 paquetes
    colores <- if (length(paquetes) < 3) {
        c("#66C2A5", "#FC8D62", "#8DA0CB")[1:length(paquetes)] # Paleta fija
    } else {
        brewer.pal(min(length(paquetes), 8), "Set2")
    }
    
    # Filtrar datos según las fechas del usuario
    data <- data %>%
        filter(date >= input$start_date & date <= input$end_date)
    
    # Manejo de paquetes sin datos en el rango
    paquetes_sin_datos <- setdiff(paquetes, unique(data$package))
    if (length(paquetes_sin_datos) > 0) {
        # Crear líneas con y = 0 para los paquetes sin datos
        data_sin_datos <- do.call(rbind, lapply(paquetes_sin_datos, function(pkg) {
            data.frame(
                date = seq(input$start_date, input$end_date, by = "day"),
                cumulative_count = 0,
                package = pkg
            )
        }))
    } else {
        data_sin_datos <- NULL
    }
    
    # Calcular descargas acumuladas solo para paquetes con datos
    cumulative_data <- data %>%
        group_by(package) %>%
        arrange(date) %>%
        mutate(cumulative_count = cumsum(count)) %>%
        ungroup()
    
    # Combinar datos acumulados con datos de paquetes faltantes
    cumulative_data <- bind_rows(cumulative_data, data_sin_datos)
    
    # Filtrar lanzamientos válidos según el rango de fechas del input
    valid_releases <- releases %>%
        filter(!is.na(date) & date >= input$start_date & date <= input$end_date)
    
    # Crear el gráfico
    ggplot(cumulative_data, aes(x = date, y = cumulative_count, color = package)) +
        geom_line(linewidth = 1.2) + # Líneas normales
        geom_point(
            data = valid_releases,
            aes(x = date, y = 0, color = package), # Puntos sólidos para lanzamientos
            size = 3,
            shape = 21,
            fill = "white"
        ) +
        labs(
            title = "Descargas Acumuladas",
            x = "Fecha",
            y = "Descargas Acumuladas",
            color = "Paquete"
        ) +
        scale_color_manual(values = setNames(colores, paquetes)) +
        theme_minimal()
})

  
  
  
  
}

# Ejecutar la app
shinyApp(ui, server)
