library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


ui <- fluidPage(
  #titlePanel("Cost of Healthcare Compared to Iowa"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Category", "Select Category", 
                  choices=c("Ears,Nose,Throat",	"Endocrine",	"Gastrointestinal",	"Heart",	"Infections",	"Lung",	"Nerve",	"Orthopedic",	"Other",	"Poisoning",	"Psychiatric",	"Renal",	"Skin",	"Vascular"
                  ),
                  selected=c("Heart"
                  ), 
                  multiple=FALSE),width=2
    ),
    mainPanel(
      plotlyOutput("plot",width=1100,height=550
      )
    )
  )
)  


server <- function(input, output,session) {
  output$plot <- renderPlotly({
    states<-map_data("state")

    mapstates<-merge(states,COLA_Mid_Point_Analysis_Table,by.x ="region", by.y ="Name")
    
    catreact<-reactive({input$Category})
    
    mapstates %>% 
      filter(Category==catreact())%>%
      group_by(Category,ProviderState,Mean_Cost_Percentage_Difference_to_Iowa)%>%
      rename(Compared.to.Iowa=Mean_Cost_Percentage_Difference_to_Iowa)%>%
        ggplot(aes(x = long, y = lat,group=group)) +   
      geom_polygon(aes(group = group, fill=Compared.to.Iowa))+
      expand_limits(x=states$long, y=states$lat)+
      labs(title="Cost of Healthcare Compared to Iowa")+
      scale_fill_distiller(palette = "Spectral",name="% Compared to Iowa")+
      theme(plot.title = element_text(hjust = -0.7))
    
  })
  
  }

shinyApp(ui = ui, server = server)