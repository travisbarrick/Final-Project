library(leaflet)
library(shiny)

ui <- fluidPage(
  selectInput("State", "State", 
              c("AK"	,"AL"	,"AR"	,"AZ"	,"CA"	,"CO"	,"CT"	,"DC"	,"DE"	,"FL"	,"GA"	,"HI"	,"IA"	,"ID"	,"IL"	,"IN"	,"KS"	,"KY"	,"LA"	,"MA"	,"MD"	,"ME"	,"MI"	,"MN"	,"MO"	,"MS"	,"MT"	,"NC"	,"ND"	,"NE"	,"NH"	,"NJ"	,"NM"	,"NV"	,"NY"	,"OH"	,"OK"	,"OR"	,"PA"	,"RI"	,"SC"	,"SD"	,"TN"	,"TX"	,"UT"	,"VA"	,"VT"	,"WA"	,"WI"	,"WV"	,"WY"
              ),
              "IA", 
              multiple=TRUE) 
),
  leafletOutput('myMap')
)
server <- function(input, output) {
  map <- leaflet(
    # healthcare <- read_csv("C:/Users/Travis/Desktop/Final Project/Healthcare_Data.csv")
    #healthcare$Provider_City_lower <- tolower(healthcare$ProviderCity)
    #zip<-read_csv("C:/Users/Travis/Desktop/Final Project/zipcode.csv")
    #healthcare$ProviderZipCode <- as.factor(healthcare$ProviderZipCode)
    #Analysis_Dataset <- merge(healthcare, zip, by.x = "ProviderZipCode", by.y = "zip")
    Analysis_Dataset%>%
      filter(ProviderState==input$State)%>%
      group_by(ProviderName,latitude,longitude)%>%
      summarise(payments=mean(AverageTotalPayments))) %>%
    addTiles() %>% 
    setView(-95, 40, zoom = 4)%>%
    addMarkers(lng=~longitude, lat=~latitude)
  output$myMap <- renderLeaflet(map)
}

# Run the application
shinyApp(ui = ui, server = server)