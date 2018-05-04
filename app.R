library(shiny)
library(leaflet)
library(data.table)
library(geosphere)
library(tidyverse)
library(sp)
library(ggmap)
library(maptools)
library(Hmisc)
library(xlsx)

SUBWAY <-
  read.csv(file = "https://raw.githubusercontent.com/shubhadasuresh/MTA-Subway/master/SUBWAY_STATION.csv", header =
             T, sep = ",")
SUBWAY$long <-
  sub('.*\\(', '', SUBWAY$the_geom)
SUBWAY$lat <- sub('.*\\s+', '', SUBWAY$long)
SUBWAY$lat <- gsub(")", "", SUBWAY$lat)
SUBWAY$long <- sub('\\s+.*$', '', SUBWAY$long)

SUBWAY$lat <- as.numeric(SUBWAY$lat)
SUBWAY$long <- as.numeric(SUBWAY$long)

selection <-
  read.csv(file = "https://raw.githubusercontent.com/shubhadasuresh/MTA-Subway/master/Station_Sample.csv", header = T, sep = ",")

 THEATER <- read.csv("https://raw.githubusercontent.com/shubhadasuresh/MTA-Subway/master/THEATER_cleaned.csv", header =T, sep = ",")

 Just <-
   read.csv("https://raw.githubusercontent.com/shubhadasuresh/MTA-Subway/master/Restaurant_Sample.csv", header =
                T, sep = ",")
 
 Museums <- read.csv("https://raw.githubusercontent.com/shubhadasuresh/MTA-Subway/master/museums_galleries.csv", header =T, sep = ",")
 
 Museums$lat <- sub("\\,.*", "", Museums$Geometry.X.Y)
 Museums$long <- sub('.*,\\s*', '', Museums$Geometry.X.Y)
 
 Museums$lat <- as.numeric(Museums$lat)
 Museums$long <- as.numeric(Museums$long)

 
ui <- fluidPage(
  sidebarPanel(
    selectInput(
      "nameInput1",
      "FROM_STATION",
      choices = selection,
      selected = NULL,
      width = "300px",
      multiple = FALSE
    ),
    selectInput(
      "nameInput2",
     "TO_STATION",
      choices = selection,
     selected = NULL,
      width = "300px",
      multiple = FALSE
    )
  ),
  leafletOutput("mymap"),
  actionButton("reset_button", "Reset view"),
  p()
)




server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    station1_df <-
      SUBWAY %>% select(long, lat, LINE) %>% filter(SUBWAY$NAME == input$nameInput1) %>% head(1)
    station2_df <-
      SUBWAY %>% select(long, lat, LINE) %>% filter(SUBWAY$NAME == input$nameInput2) %>% head(1)
    
    content1 <- paste(sep=',',
                      as.character(input$nameInput1), station1_df$LINE)
    
    content2 <- paste(sep = ',',
                      as.character(input$nameInput2),
                      station2_df$LINE)
    
    
    
    mat1 <-
      distm(station1_df[, c('long', 'lat')], Just[, c('lon', 'lat')], fun =
              distHaversine)
    mat2 <-
      distm(station2_df[, c('long', 'lat')], Just[, c('lon', 'lat')], fun =
              distHaversine)
    
    loc1 <-
      distm(station1_df[, c('long', 'lat')], THEATER[, c('long', 'lat')], fun =
              distHaversine)
    loc2 <-
      distm(station2_df[, c('long', 'lat')], THEATER[, c('long', 'lat')], fun =
              distHaversine)
    
    mus1 <-
      distm(station1_df[, c('long', 'lat')], Museums[, c('long', 'lat')], fun =
              distHaversine)
    mus2 <-
      distm(station2_df[, c('long', 'lat')], Museums[, c('long', 'lat')], fun =
              distHaversine)
    
    Just$mat1 <- t(mat1)
    Just$mat2 <- t(mat2)
    
    THEATER$Theater1 <- t(loc1)
    THEATER$Theater2 <- t(loc2)
    
    Museums$dist1 <- t(mus1)
    Museums$dist2 <- t(mus2)
    
    Top_1 <-  Just[order(Just$mat1)[1:3],]
    Top_2 <-  Just[order(Just$mat2)[1:3],]
    
    Top_1$lat <- as.numeric(Top_1$lat)
    Top_1$long <- as.numeric(Top_1$lon)
    
    Top_2$lat <- as.numeric(Top_2$lat)
    Top_2$long <- as.numeric(Top_2$lon)
    
    Rest_1 <- Top_1 %>% select(lon, lat, DBA)
    Rest_2 <- Top_2 %>% select(lon, lat, DBA)
    
    Thr_1 <-  THEATER[order(THEATER$Theater1)[1:2],]
    Thr_2 <-  THEATER[order(THEATER$Theater2)[1:2],]
    
    mus_gall1 <-  Museums[order(Museums$dist1)[1:2],]
    mus_gall2 <-  Museums[order(Museums$dist2)[1:2],]
    
    URL1 <- paste0("<a href='",Thr_1$URL,"'>",Thr_1$NAME,"</a>")
    URL2 <- paste0("<a href='",Thr_2$URL,"'>",Thr_2$NAME,"</a>")
    
    URL3 <- paste0("<a href='",mus_gall1$URL,"'>",mus_gall1$NAME,"</a>")
    URL4 <- paste0("<a href='",mus_gall2$URL,"'>",mus_gall2$NAME,"</a>")
    greenLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 38,
      iconHeight = 95,
      iconAnchorX = 22,
      iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50,
      shadowHeight = 64,
      shadowAnchorX = 4,
      shadowAnchorY = 62
    )
    
    redLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-red.png",
      iconWidth = 38,
      iconHeight = 95,
      iconAnchorX = 22,
      iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50,
      shadowHeight = 64,
      shadowAnchorX = 4,
      shadowAnchorY = 62
    )
    
    icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
    icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'black')
    icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green')
    
 
    leaflet(data = SUBWAY) %>%
      addTiles() %>%
      addMarkers(
        station1_df$long,
        station1_df$lat,
        popup = ~ as.character(content1),
        label = ~ as.character(content1),
        labelOptions = labelOptions(noHide = T, textsize = "15px")
      ) %>%
      addMarkers(
        station2_df$long,
        station2_df$lat,
        popup = ~ as.character(content2),
        label = ~ as.character(content2),
        labelOptions = labelOptions(noHide = T, textsize = "15px")
      ) %>%
      addMarkers(
        data = Rest_1,
        Rest_1$lon,
        Rest_1$lat,
        icon = icon.fa,
        popup = ~ as.character(Rest_1$DBA),
        label = ~ as.character(Rest_1$DBA),
        labelOptions = labelOptions(noHide = T, textsize = "18px"),
        group = "Restaurants near Station"
      ) %>%
      addMarkers(
        data = Rest_2,
        Rest_2$lon,
        Rest_2$lat,
        icon = icon.fa,
        popup = ~ as.character(Rest_2$DBA),
        label = ~ as.character(Rest_2$DBA),
        labelOptions = labelOptions(noHide = T, textsize = "18px"),
        group = "Restaurants near Station"
      ) %>%
      addMarkers(
        data = Thr_1,
        Thr_1$long,
        Thr_1$lat,
        icon = icon.ion,
        popup= ~ as.character(URL1),
        label = ~ as.character(Thr_1$NAME),
        labelOptions = labelOptions(noHide = T, textsize = "18px"),
        group = "Theaters"
      ) %>%
      addMarkers(
        data = Thr_2,
        Thr_2$long,
        Thr_2$lat,
        icon = icon.ion,
        popup = ~ as.character(URL2),
        label = ~ as.character(Thr_2$NAME),
        labelOptions = labelOptions(noHide = T, textsize = "18px"),
        group = "Theaters"
      ) %>%
      addMarkers(
        data = mus_gall1,
        mus_gall1$long,
        mus_gall1$lat,
        popup = ~ as.character(URL3),
        label = ~ as.character(mus_gall1$NAME),
        labelOptions = labelOptions(noHide = T, textsize = "18px"),
        group = "Museums and Galleries"
      ) %>%
      addMarkers(
        data = mus_gall2,
        mus_gall2$long,
        mus_gall2$lat,
        popup = ~ as.character(URL4),
        label = ~ as.character(mus_gall2$NAME),
        labelOptions = labelOptions(noHide = T, textsize = "18px"),
        group = "Museums and Galleries"
      ) %>% 
      addMarkers(
        data = Just,
        Just$lon,
        Just$lat,
        icon = icon.ion,
        popup = ~ as.character(Just$DBA),
        label = ~ as.character(Just$DBA),
        labelOptions = labelOptions(noHide = T, textsize = "10px"),
        group = "All Restaurants"
      ) %>%
      addLayersControl(
        options =  layersControlOptions(collapsed = FALSE),
        overlayGroups = c("Restaurants near Station", "All Restaurants", "Theaters","Museums and Galleries")
      ) %>% hideGroup("Restaurants near Station") %>% hideGroup("All Restaurants") %>%
      hideGroup("Theaters") %>% hideGroup("Museums and Galleries")
  })
  
  
  observe({
    input$reset_button
    leafletProxy("mymap") %>% setView(lat = 40.71,
                                      lng = -74.00,
                                      zoom = 15)
  })
  
}

shinyApp(ui, server)
