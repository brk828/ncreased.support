packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(leaflet)
packages(sf)
packages(dplyr)
packages(sp)
packages(mapview)

# Load your RiverLocations layer as an sf object
RiverLocations <- st_read("LocationShapeFiles/location_polygons.shp")
RiverLocations <-st_transform(RiverLocations, "+proj=longlat +datum=WGS84")

RiverLocations$Label <- paste(RiverLocations$LOCATION, "-",
                              RiverLocations$DECIMAL_ZO)

# Define a color palette (you can customize this)
ZoneColors <- colorFactor(palette = "Set3", 
                            domain = RiverLocations$DECIMAL_ZO)

# Create a map widget
map <- leaflet() %>%
  # Add a basemap from OpenStreetMap
  addProviderTiles("OpenStreetMap") %>%
  # Add RiverLocations layer
  addPolygons(data = RiverLocations, 
              fillColor = ~ZoneColors(DECIMAL_ZO),
              fillOpacity = 0.8,  
              color = ~ZoneColors(DECIMAL_ZO),
              label = RiverLocations$Label,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  setView(lng = -114.65694, lat = 35.41765, zoom = 10) 

map

