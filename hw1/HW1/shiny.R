library(shiny)
library(binplot)

ui <- fluidPage(
  fileInput("file", "Choose Datasest File", multiple = FALSE, accept = NULL,
              width = NULL, buttonLabel = "Browse...",
              placeholder = "No file selected"),
  plotOutput("plot1",width = 500,height = 500)
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    binplot::binplot(input$file$datapath)
  })
}

shinyApp(ui = ui, server = server)
