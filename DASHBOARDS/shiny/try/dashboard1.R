library(shiny)
library(shinydashboard)

# Definir la interfaz del dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Básico"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Primera pestaña: Inicio
      tabItem(tabName = "inicio",
              h2("Bienvenido al Dashboard Básico"),
              p("Usa el menú lateral para navegar.")
      ),
      # Segunda pestaña: Gráficos
      tabItem(tabName = "graficos",
              fluidRow(
                box(plotOutput("grafico1"), title = "Gráfico de Ejemplo", width = 6),
                box(verbatimTextOutput("summary"), title = "Resumen de Datos", width = 6)
              )
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  # Datos de ejemplo
  data <- reactive({ rnorm(100) })
  
  # Crear un gráfico
  output$grafico1 <- renderPlot({
    hist(data(), col = "skyblue", border = "white", main = "Histograma de Ejemplo")
  })
  
  # Mostrar resumen de los datos
  output$summary <- renderPrint({
    summary(data())
  })
}

# Ejecutar la app
shinyApp(ui, server)
