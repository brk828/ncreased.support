library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(sp)

get_polygon <- function(lat, lon) {
  # Create a SpatialPoints object from the input coordinates
  point <- SpatialPoints(cbind(lon, lat), CRS("EPSG:4326"))
  point <- st_as_sf(point)
  
  # Use st_intersection to get the polygon that intersects with the point
  polygon <- st_intersection(point, RiverLocations)
  
  # Return the label of the polygon
  return(polygon$Label)
}

# Load your RiverLocations layer as an sf object
RiverLocations <- st_read("LocationShapeFiles/location_polygons.shp")
RiverLocations <-st_transform(RiverLocations, "+proj=longlat +datum=WGS84")

RiverLocations$Label <- paste0(RiverLocations$LOCATION, " (",  RiverLocations$DECIMAL_ZO,
                              ") - ",
                              RiverLocations$LOCATION_I)

# Define a color palette (you can customize this)
ZoneColors <- colorFactor(palette = "Set3", 
                          domain = RiverLocations$DECIMAL_ZO)

Centroid <- st_centroid(RiverLocations) %>%
  dplyr::mutate(Lon = sf::st_coordinates(.)[,1],
                Lat = sf::st_coordinates(.)[,2]) %>%
  select(Label, Lat, Lon)
  
ui <- fluidPage(
  # App title
  titlePanel("Polygon Location Map"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      tags$form(
        numericInput("lat", "Latitude", value = 35.4762), 
        numericInput("lon", "Longitude", value = -114.6619),
        submitButton("Submit", icon("map-marker"))
      ),
      verbatimTextOutput("polygon_name"),
      radioButtons("basemap", "Change Basemap (must hit Submit):",
                   choices = c("Esri World Imagery" = "Esri.WorldImagery",
                               "Open Street Map" = "OpenStreetMap.Mapnik"),
                   selected = "Esri.WorldImagery")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Leaflet map
      leafletOutput("map", height = "100vh")
    )
  )
)


# Define server logic for app
server <- function(input, output) {
  
  # Reactive expression for latitude and longitude
  coords <- reactive({
    c(input$lon, input$lat)
  })
  
  basemap <- reactive({
    input$basemap
  })
  
  # Create a map widget
  map <- leaflet() 
  
 #  Render the map in the output container
  output$map <- renderLeaflet({
    leaflet() %>%
      # Add a basemap from OpenStreetMap
      addProviderTiles(basemap()) %>%
      # Add RiverLocations layer
      addPolygons(data = RiverLocations, fillColor = ~ZoneColors(DECIMAL_ZO),
                  fillOpacity = 0.5,  
                  color = ~ZoneColors(DECIMAL_ZO),
                  highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 0.7,
                    opacity = 1.0,
                    bringToFront = TRUE,
                    sendToBack = TRUE),  
                  label = RiverLocations$Label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      setView(lng = -114.65694, lat = 35.41765, zoom = 10) 
  })
  
  # Update the map when the user clicks the submit button
  observeEvent(input$lat, {
    output$polygon_name <- renderText({
      # Get the input values
      Lat <- input$lat
      Lon <- input$lon
      paste("The marker is inside:", get_polygon(Lat, Lon))
    })
    # Add a marker based on the input coordinates
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = coords()[1], lat = coords()[2]) %>%
      setView(lng = coords()[1], lat = coords()[2], zoom = 14) 
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))