library(shiny)
library(ggplot2)
library(googlesheets4)

gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1ub6stAPzrdNUR0K3L9sFj-5JETRgjBSfTIvhXVdIEpg/edit?usp=sharing"
ReachEstimates <- read_sheet(sheet_id, "Reaches")

ReachEstimates$Year <- as.factor(ReachEstimates$Year)

# Define the UI
ui <- fluidPage(
  titlePanel("Population Estimates"),
  
  sidebarLayout(sidebarPanel(selectInput(inputId = "species", 
                                         label = "Species:", 
                                         choices = unique(ReachEstimates$Species),
                                         selected = "XYTE"),
                             checkboxGroupInput(inputId = "reach", 
                                         label = "Reach:", 
                                         choices = unique(ReachEstimates$Reach),
                                         selected = 2)),
                mainPanel(plotOutput(outputId = "plot"), 
                          dataTableOutput(outputId = "table"))))

# Define the server logic
server <- function(input, output) {
  
    # Filter the data based on user selections
  filteredData <- reactive({
    subset(ReachEstimates, Species == input$species & Reach %in% input$reach)
  })
  
  # Create the plot
  output$plot <- renderPlot({
    ggplot(filteredData(), aes(x = Year, y = Estimate, group = Reach)) +
       geom_line(aes(color = Reach)) +
  geom_point(aes(color = Reach), size = 2)+
      geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), alpha = 0.3) +
      labs(x = "Year", y = "Population Size") +
      ggtitle(paste("Population Estimates for", input$species)) +
      scale_x_discrete(breaks = unique(filteredData()$Year), labels = unique(filteredData()$Year)) +
      expand_limits(y = 0)
  })
  # Create the table
  output$table <- renderDataTable({
    filteredData()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)


