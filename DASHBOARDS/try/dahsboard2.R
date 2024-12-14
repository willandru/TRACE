
#shiny
#install.packages("shiny")
#install.packages("shinydashboard")


library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(dplyr)

# Interfaz del Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Tendencia de Descargas CRAN"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña principal
      tabItem(tabName = "inicio",
              fluidRow(
                box(
                  title = "Configuración de Gráfico",
                  width = 4,
                  dateInput("start_date", "Fecha de inicio:", value = Sys.Date() - 30),
                  dateInput("end_date", "Fecha de fin:", value = Sys.Date()),
                  textInput("packages", "Paquetes (separados por comas):", value = "ggplot2, dplyr"),
                  actionButton("update", "Actualizar Gráfico")
                ),
                box(
                  title = "Tendencia de Descargas",
                  width = 8,
                  plotOutput("trend_plot")
                )
              )
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

# Ejecutar la aplicación
shinyApp(ui, server)
