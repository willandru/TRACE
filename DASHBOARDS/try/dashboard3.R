library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(dplyr)

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
    tabItems(
      # Primera pestaña: Inicio
      tabItem(tabName = "inicio",
              h2("Bienvenido al Dashboard Básico"),
              p("Usa el menú lateral para navegar."),
              fluidRow(
                box(
                  title = "Configuración de Gráfico",
                  width = 4,
                  dateInput("start_date", "Fecha de inicio:", value = Sys.Date() - 30),
                  dateInput("end_date", "Fecha de fin:", value = Sys.Date()),
                  textInput("packages", "Paquetes (separados por comas):", value = "sivirep, vaccineff, serofoi"),
                  actionButton("update", "Actualizar Gráfico")
                ),
                box(
                  title = "Tendencia de Descargas",
                  width = 8,
                  plotOutput("trend_plot")
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
server <- function(input, output) {
  
  # Datos reactivos
  download_data <- reactive({
    # Esperar al botón "Actualizar Gráfico"
    input$update
    isolate({
      req(input$packages) # Verifica que los paquetes estén definidos
      packages <- strsplit(input$packages, ",\\s*")[[1]] # Divide los nombres por comas
      start_date <- input$start_date
      end_date <- input$end_date
      
      # Obtener datos de descargas con cranlogs
      cranlogs::cran_downloads(packages = packages, 
                               from = start_date, 
                               to = end_date)
    })
  })
  
  # Generar gráfico
  output$trend_plot <- renderPlot({
    data <- download_data()
    req(nrow(data) > 0) # Asegurar que haya datos
    
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
