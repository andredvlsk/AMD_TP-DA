library(shiny)
library(dplyr)

data(mtcars)

ui <- fluidPage(
  titlePanel("Datos de automóviles"),
  p("Empleamos datos del dataset mtcars"),
  sidebarLayout(
    sidebarPanel(
      selectInput("trans", "Transmisión (0 = AT, 1 = MA)", choices = unique(mtcars$am))
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

server <- function(input, output){
  output$scatterplot <- renderPlot({
    
    muestra <- mtcars %>% filter(am == input$trans)
    #muestra <- filter(mtcars, am == input$trans)
    plot(muestra$hp, muestra$mpg,
         xlab = "Potencia",
         ylab = "Consumo",
         main = paste("Relación entre potencia y consumo por tipo de transmisión",
                      input$trans))
  })
}

shinyApp(ui, server)
