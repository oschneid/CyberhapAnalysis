# Analysis for CyberHap Study 1 (run by GM, Nov-Dec 2015)
# Author: Oliver Schneider oschneid@cs.ubc.ca
# SHINY VERSION

require(ggplot2)
require(Exact)
require(MASS)
require(car)
require(shiny)

study1.data <- read.csv("CyberhapData-151210-GM.csv")

#Data cleanup - set types to discrete factors
study1.data <- study1.data[study1.data$Training==FALSE,]
study1.data$Spring.Pair <- factor(study1.data$Spring.Pair)
study1.data$PID <- factor(study1.data$PID)
study1.data$Condition.Number <- factor(study1.data$Condition.Number)

ui.facet.choices <- c('.', names(study1.data))

# Define the UI
ui <- bootstrapPage(fluidPage(
  titlePanel("Cyberhap Study 1 - Difficulty"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select.x", label = h3("X"), 
                  choices = ui.facet.choices, selected = 1),
      selectInput("select.facet1", label = h3("Facet 1"), 
                  choices = ui.facet.choices, selected = 1),
      selectInput("select.facet2", label = h3("Facet 2"), 
                  choices = ui.facet.choices, selected = 1)
    ),
    mainPanel(
      plotOutput('plot')
    )
  )));


# Define the server code
server <- function(input, output) {
  
  output$plot <- renderPlot({
    p <- ggplot(study1.data, aes(x=Difficulty..0.19., color=Spring.Pair));
    p <- p + geom_histogram();
    if (input$select.facet1 != '.' || input$select.facet2 != '.') {
      p <- p + facet_grid(as.formula(paste(input$select.facet1, "~", input$select.facet2)))
    }
    p
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)