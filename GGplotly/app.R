library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
Analysis_Dataset$Is_Iowa <- Analysis_Dataset$ProviderState ==  "IA"



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
                  multiple=TRUE)
    ),
    mainPanel(
      # plotOutput("coolplot")
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output) {
  catfilter<-reactive({
    input$Category
  })
  statefilter<-reactive({
    input$State
  })
  
  output$plot <- renderPlotly({
    CostCategoryDataset <-
      Analysis_Dataset %>%
      filter(Category==catfilter())%>%
      group_by(Category, Is_Iowa) %>%
      summarise(Average_Treatment_Cost = mean(TreatmentCost))
    
      p<-CostCategoryDataset %>%
      ggplot(aes(x = Category, y = Average_Treatment_Cost, group_by(Is_Iowa), fill = Is_Iowa)) + 
      coord_flip() +
      geom_bar(stat="identity", position = position_dodge()) +
      xlab("Average Cost of Treatment") +
      ylab("Treatment Categories") 
    
      (gg <- ggplotly(p))
    
    
  })
}

shinyApp(ui = ui, server = server)