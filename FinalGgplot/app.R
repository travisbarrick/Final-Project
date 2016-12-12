library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

#healthcare <- read_csv("C:/Users/Travis/Desktop/Final Project/Healthcare_Data.csv")

ui <- fluidPage(
  titlePanel("Average Cost of Healthcare"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Category", "Category Definition", 
                  choices=c("Ears,Nose,Throat",	"Endocrine",	"Gastrointestinal",	"Heart",	"Infections",	"Lung",	"Nerve",	"Orthopedic",	"Other",	"Poisoning",	"Psychiatric",	"Renal",	"Skin",	"Vascular"
                  ),
                  selected=c("Ears,Nose,Throat",	"Endocrine",	"Gastrointestinal",	"Heart",	"Infections",	"Lung",	"Nerve",	"Orthopedic",	"Other",	"Poisoning",	"Psychiatric",	"Renal",	"Skin",	"Vascular"
                  ), 
                  multiple=TRUE), 
      selectInput("State", "State", 
                  choices=c("AK"	,"AL"	,"AR"	,"AZ"	,"CA"	,"CO"	,"CT"	,"DC"	,"DE"	,"FL"	,"GA"	,"HI"	,"IA"	,"ID"	,"IL"	,"IN"	,"KS"	,"KY"	,"LA"	,"MA"	,"MD"	,"ME"	,"MI"	,"MN"	,"MO"	,"MS"	,"MT"	,"NC"	,"ND"	,"NE"	,"NH"	,"NJ"	,"NM"	,"NV"	,"NY"	,"OH"	,"OK"	,"OR"	,"PA"	,"RI"	,"SC"	,"SD"	,"TN"	,"TX"	,"UT"	,"VA"	,"VT"	,"WA"	,"WI"	,"WV"	,"WY"
                  ),
                  selected=c("AK"	,"AL"	,"AR"	,"AZ"	,"CA"	,"CO"	,"CT"	,"DC"	,"DE"	,"FL"	,"GA"	,"HI"	,"IA"	,"ID"	,"IL"	,"IN"	,"KS"	,"KY"	,"LA"	,"MA"	,"MD"	,"ME"	,"MI"	,"MN"	,"MO"	,"MS"	,"MT"	,"NC"	,"ND"	,"NE"	,"NH"	,"NJ"	,"NM"	,"NV"	,"NY"	,"OH"	,"OK"	,"OR"	,"PA"	,"RI"	,"SC"	,"SD"	,"TN"	,"TX"	,"UT"	,"VA"	,"VT"	,"WA"	,"WI"	,"WV"	,"WY"), 
                  multiple=TRUE)
       ),
    mainPanel(
     # plotOutput("coolplot")
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output,session) {
  output$plot <- renderPlotly({
str(Analysis_Dataset)
    m<-Analysis_Dataset%>%
      filter(Category==input$Category,ProviderState==input$State)%>%
      group_by(Category,ProviderState) %>% 
      summarize(avg_insurance = mean(AverageCoveredCharges),
                avg_medicare = mean(AverageMedicarePayments),
                avg_patient_owed = mean(AverageTotalPayments-AverageMedicarePayments))
    p <- plot_ly(m, x = ~avg_insurance, y = ~avg_medicare, 
                 z = ~avg_patient_owed,hoverinfo = 'text',text=~paste('Category: ',Category,'</br> State: ',ProviderState),color = ~Category 
                 )%>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Average Insurance'),
                          yaxis = list(title = 'Average Medicare'),
                          zaxis = list(title = 'Average Patient Owed')))
    
  })
}

shinyApp(ui = ui, server = server)