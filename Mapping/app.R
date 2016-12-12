library(leaflet)
library(shiny)
library(htmltools)
library(ggplot2)
library(DT)
require(stringr)



ui <- fluidPage(
  # Application title
  titlePanel("Treatment Cost"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Category", "Category", 
                  choices=c("Ears,Nose,Throat",	"Endocrine",	"Gastrointestinal",	"Heart",	"Infections",	"Lung",	"Nerve",	"Orthopedic",	"Other",	"Poisoning",	"Psychiatric",	"Renal",	"Skin",	"Vascular"
                  ),
                  selected="Heart", 
                  multiple=FALSE), 
      radioButtons("Layout", "Layout", 
                  choices=c("NASAGIBS.ViirsEarthAtNight2012","Thunderforest.SpinalMap","Esri.WorldStreetMap","Esri.NatGeoWorldMap"
                  ),
                  selected=c("Thunderforest.SpinalMap")),
      width=3
      
    ),
   
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("Map",
               leafletOutput('myMap')),
      tabPanel("Most Affordable Hospitals",
               DT::dataTableOutput("table"))
    )
  )
)
)
    



server <- function(input, output,session) {
  catreact<-reactive({input$Category})
  statereact<-reactive({input$Layout})
  
  output$myMap <- renderLeaflet(
    test<-AverageCategory%>%
     filter(Category==catreact())%>%
      group_by(Category)%>%
      mutate(rank=row_number(),CostofTreatment=sprintf("$%s",format(AverageCost,big.mark=",",digits=2,scientific=FALSE)))%>%
      mutate(X=paste0('<strong>ProviderName: </strong>', 
                      ProviderName,
                      '<br><strong>ProviderStreetAddress</strong>: ',
                      ProviderStreetAddress,
                      '<br><strong>ProviderCity</strong>: ',
                      ProviderCity,
                      '<br><strong>AverageCost</strong>: ',
                      CostofTreatment,
                      '<br><strong>TreatmentCostRank</strong>: ',
                      rank
                      ))%>%
    leaflet()%>%
  
      addTiles() %>%
          #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%
      #addProviderTiles("Thunderforest.SpinalMap")%>%
      #addProviderTiles("Esri.WorldStreetMap")%>%
      #addProviderTiles("Esri.NatGeoWorldMap")%>%
      addProviderTiles(statereact())%>%
      setView(-110, 45, zoom = 3)%>%
      addMarkers(lng=~longitude, lat=~latitude,popup=~as.character(X),clusterOptions = markerClusterOptions())%>%
      mapOptions(zoomToLimits = "first")
  )
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <-     AverageCategory%>%
      filter(Category==catreact())%>%
      top_n(n=10,wt=AverageCost)%>%
      mutate(CostofTreatment=sprintf("$%s",format(AverageCost,big.mark=",",digits=2,scientific=FALSE)))
    
    
    data[,c("ProviderName","Name","ProviderState","CostofTreatment")]
   
 
}))
}

# Run the application
shinyApp(ui = ui, server = server)


