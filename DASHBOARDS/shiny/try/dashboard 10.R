library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(dplyr)
library(viridis)


# Interfaz del Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "R Packages"),
  dashboardSidebar(
    collapsed = TRUE,
    width = 100, # Menú lateral más angosto
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
              #p("Usa el menú lateral para navegar."),
              fluidRow(
                column(
                  width = 4, # Menú a la izquierda
                  box(
                    title = "Seleccionar Fechas",
                    width = 9,
                    dateInput("start_date", "Fecha de inicio:", value = Sys.Date() - 30),
                    dateInput("end_date", "Fecha de fin:", value = Sys.Date())
                  ),
                  box(
                    title = "Seleccionar Paquetes",
                    width = 12,
                    textInput("packages", "Paquetes (separados por comas):", value = "ggplot2, dplyr"),
                    actionButton("update", "Actualizar Gráfico")
                  )
                ),
                column(
                  width = 8, # Gráficos a la derecha
                  box(
                    title = "Tendencia de Descargas",
                    width = 12,
                    plotOutput("trend_plot", height = "300px")
                  ),
                  box(
                    title = "Descargas Acumuladas",
                    width = 12,
                    plotOutput("cumulative_plot", height = "300px")
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
  
  # Datos reactivos
  download_data <- reactive({
    input$update
    isolate({
      # Obtener y limpiar la lista de paquetes
      req(input$packages)
      packages <- strsplit(input$packages, ",\\s*")[[1]]
      packages <- trimws(packages) # Eliminar espacios en blanco
      
      # Validar que no haya paquetes vacíos
      if (any(packages == "")) {
        showModal(modalDialog(
          title = "Error en los Paquetes",
          "No se permiten espacios en blanco o nombres vacíos.",
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
        return(NULL)
      }
      
      # Validar que los paquetes existan en CRAN
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
  
  # Generar gráfico de descargas diarias
  output$trend_plot <- renderPlot({
    # Obtener los datos reactivos
    data <- download_data()
    req(data, nrow(data) > 0) # Verificar que existan datos antes de continuar
    
    # Generar una paleta de colores dinámica basada en los paquetes únicos
    paquetes <- unique(data$package)
    num_paquetes <- length(paquetes)
    colores <- grDevices::rainbow(num_paquetes) # Usar la función base de R para generar colores
    
    # Crear el gráfico de tendencia de descargas
    ggplot(data, aes(x = date, y = count, color = package)) +
      geom_line(size = 1.2) +
      labs(title = "Tendencia de Descargas Diarias",
           x = "Fecha",
           y = "Descargas",
           color = "Paquete") +
      scale_color_manual(values = setNames(colores, paquetes)) + # Asignar colores dinámicos
      theme_bw()
  })
  
  # Generar gráfico de descargas acumuladas
  output$cumulative_plot <- renderPlot({
    # Obtener los datos reactivos
    data <- download_data()
    req(data, nrow(data) > 0) # Verificar que existan datos antes de continuar
    
    # Calcular las descargas acumuladas para cada paquete
    cumulative_data <- data %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate(cumulative_count = cumsum(count))
    
    # Generar una paleta de colores dinámica basada en los paquetes únicos
    paquetes <- unique(cumulative_data$package)
    num_paquetes <- length(paquetes)
    colores <- grDevices::rainbow(num_paquetes) # Usar la función base de R para generar colores
    
    # Crear el gráfico de descargas acumuladas
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
