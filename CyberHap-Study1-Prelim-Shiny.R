# Analysis for CyberHap Study 1 (run by GM, Nov-Dec 2015)
# Author: Oliver Schneider oschneid@cs.ubc.ca
# SHINY VERSION

require(ggplot2)
require(Exact)
require(MASS)
require(car)
require(shiny)

study1.data <- read.csv("CyberhapData-Oliver.csv")

#Data cleanup - set types to discrete factors
study1.data <- study1.data[study1.data$Training==FALSE,]
study1.data$Spring.Pair <- factor(study1.data$Spring.Pair)
study1.data$PID <- factor(study1.data$PID)
study1.data$Condition.Number <- factor(study1.data$Condition.Number)

# Define the UI
ui <- bootstrapPage(fluidPage(
  titlePanel("Cyberhap Study 1 - Difficulty"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select.facet1", label = h3("Facet 1"), 
                  choices = names(study1.data), selected = 1),
      selectInput("select.facet2", label = h3("Facet 2"), 
                  choices = names(study1.data), selected = 2),
    ),
    mainPanel(
      plotOutput('plot')
    )
  )));


# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    qplot(Difficulty..0.19.,  data=study1.data, geom="density",
          facets=as.formula(paste(input$select.facet1, "~", input$select.facet2)),
          color=Spring.Pair)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)