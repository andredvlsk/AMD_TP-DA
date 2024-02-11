library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("DiamondsApp"),
  p("Esta aplicación muestra datos del dataset Diamonds"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Seleccione una variable", 
                  choices = c("carat", "cut", "color", "clarity")),
      sliderInput("muestra", "Tamaño de la muestra",
                  min = 1000,
                  max = nrow(diamonds),
                  value = min(5000, nrow(diamonds)), step = 500, round = 0)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Grafico de dispersión", plotOutput("scatterplot")),
        tabPanel("Grafico de barras", plotOutput("bar")),
        tabPanel("Tabla", tableOutput("tabla"))
      )
    )
  )
)

server <- function(input, output){
  output$scatterplot <- renderPlot({
    ggplot(diamonds[sample(nrow(diamonds),input$muestra),],
           aes_string(x = input$variable, y = "price")) +
      geom_point()
  })
  
  output$bar <- renderPlot({
    ggplot(diamonds[sample(nrow(diamonds),input$muestra),],
           aes_string(x = input$variable)) +
      geom_bar()
    
  })
  output$tabla <- renderTable({
    diamonds[sample(nrow(diamonds),input$muestra),]
  
  })
  
}

shinyApp(ui = ui, server = server)

