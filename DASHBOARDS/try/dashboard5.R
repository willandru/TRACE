library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(dplyr)
library(shinyjs) # Para manejar el estado del spinner

# Interfaz del Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Básico"),
  dashboardSidebar(
    collapsed = TRUE,
    width = 130,
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Sivirep", tabName = "sivirep", icon = icon("chart-bar")),
      menuItem("Serofoi", tabName = "serofoi", icon = icon("chart-pie")),
      menuItem("Vaccineff", tabName = "vaccineff", icon = icon("syringe"))
    )
  ),
  dashboardBody(
    useShinyjs(), # Habilitar shinyjs para manejar el spinner
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # Cargar CSS personalizado
    ),
    tabItems(
      # Primera pestaña: Inicio
      tabItem(tabName = "inicio",
              h2("Bienvenido al Dashboard Básico"),
              p("Usa el menú lateral para navegar."),
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
                    textInput("packages", "Paquetes (separados por comas):", value = "ggplot2, dplyr"),
                    actionButton("update", "Actualizar Gráfico")
                  )
                ),
                column(
                  width = 8,
                  box(
                    title = "Tendencia de Descargas",
                    width = 12,
                    uiOutput("trend_plot") # Mostrar el gráfico con spinner
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
      # Validar inputs en el frontend
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
      packages <- trimws(packages) # Eliminar espacios adicionales
      
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
  
  # Mostrar gráfico con spinner
  output$trend_plot <- renderUI({
    data <- download_data()
    if (is.null(data)) {
      return(div(class = "simple-spinner"))
    } else {
      return(plotOutput("plot"))
    }
  })
  
  # Generar gráfico
  output$plot <- renderPlot({
    data <- download_data()
    req(data, nrow(data) > 0) # Validar que haya datos disponibles
    
    ggplot(data, aes(x = date, y = count, color = package)) +
      geom_line(size = 1.2) +
      labs(title = "Tendencia de Descargas Diarias",
           x = "Fecha",
           y = "Descargas",
           color = "Paquete") +
      theme_minimal()
  })
}

# Ejecutar la app
shinyApp(ui, server)
