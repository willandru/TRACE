library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

# Interfaz del Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "R Packages"),
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
                    dateInput("start_date", "Fecha de inicio:", value = Sys.Date() - 30, max = Sys.Date()),
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
server <- function(input, output, session) {
  
  # Datos reactivos optimizados
  download_data <- reactive({
    input$update
    isolate({
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
      
      req(input$packages)
      packages <- strsplit(input$packages, ",\\s*")[[1]]
      packages <- trimws(packages) # Limpiar espacios
      
      # Validar paquetes vacíos
      if (any(packages == "")) {
        showModal(modalDialog(
          title = "Error en los Paquetes",
          "No se permiten espacios en blanco o nombres vacíos.",
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
        return(NULL)
      }
      
      # Validar existencia de los paquetes
      tryCatch({
        cranlogs::cran_downloads(packages = packages, 
                                 from = input$start_date, 
                                 to = input$end_date)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error en los Paquetes",
          paste("Uno o más paquetes no son válidos o no existen en CRAN:", e$message),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
        return(NULL)
      })
    })
  })
  
  # Gráfico de descargas diarias
  output$trend_plot <- renderPlot({
    data <- download_data()
    req(data, nrow(data) > 0)
    
    paquetes <- unique(data$package)
    colores <- grDevices::rainbow(length(paquetes)) # Generar colores dinámicos
    
    ggplot(data, aes(x = date, y = count, color = package)) +
      geom_line(size = 1.2) +
      labs(title = "Tendencia de Descargas Diarias",
           x = "Fecha",
           y = "Descargas",
           color = "Paquete") +
      scale_color_manual(values = setNames(colores, paquetes)) + # Asignar colores dinámicos
      theme_minimal()
  })
  
  # Gráfico de descargas acumuladas
  output$cumulative_plot <- renderPlot({
    data <- download_data()
    req(data, nrow(data) > 0)
    
    cumulative_data <- data %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate(cumulative_count = cumsum(count))
    
    paquetes <- unique(cumulative_data$package)
    colores <- grDevices::hcl.colors(length(paquetes)) # Generar colores dinámicos
    
    ggplot(cumulative_data, aes(x = date, y = cumulative_count, color = package)) +
      geom_line(size = 1.2) +
      labs(title = "Descargas Acumuladas",
           x = "Fecha",
           y = "Descargas Acumuladas",
           color = "Paquete") +
      scale_color_manual(values = setNames(colores, paquetes)) + # Asignar colores dinámicos
      theme_minimal()
  })
}

# Ejecutar la app
shinyApp(ui, server)
