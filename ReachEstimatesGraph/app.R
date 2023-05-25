library(shiny)
library(ggplot2)

ReachEstimates <- read.csv("ReachPopulationEstimates.csv", header = TRUE)

# Define the UI
ui <- fluidPage(
  titlePanel("Population Estimates"),
  
  sidebarLayout(sidebarPanel(selectInput(inputId = "species", 
                                         label = "Species:", 
                                         choices = unique(ReachEstimates$Species),
                                         selected = "XYTE"),
                             selectInput(inputId = "reach", 
                                         label = "Reach:", 
                                         choices = unique(ReachEstimates$Reach),
                                         selected = 2)),
                mainPanel(plotOutput(outputId = "plot"), 
                          dataTableOutput(outputId = "table"))))

# Define the server logic
server <- function(input, output) {
  
  ReachEstimates <- ReachEstimates[,-1]
  
    # Filter the data based on user selections
  filteredData <- reactive({
    subset(ReachEstimates, Species == input$species & Reach == input$reach)
  })
  
  # Create the plot
  output$plot <- renderPlot({
    ggplot(filteredData(), aes(x = Year, y = Estimate)) +
      geom_line() +
      geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), alpha = 0.3) +
      labs(x = "Year", y = "Population Size") +
      ggtitle(paste("Population Estimates for", input$species, "-", input$reach)) +
      scale_x_continuous(breaks = unique(filteredData()$Year), labels = unique(filteredData()$Year)) +
      expand_limits(y = 0)
  })
  # Create the table
  output$table <- renderDataTable({
    filteredData()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)


