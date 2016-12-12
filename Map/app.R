#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggmap)
runApp(list(
  ui = pageWithSidebar(
    headerPanel('Map'),
    sidebarPanel(""),
    mainPanel(uiOutput('mymap'))
  ),
  server = function(input, output){
    output$mymap <- renderUI({
   
      us = get_map(location = 'US', zoom = 4,color="bw")
      ggmap(us)+ geom_point(aes(x = longitude, y = latitude, size = TotalDischarges, group=1, colour=log(AverageTotalPayments)), data = Analysis_Dataset)+
        scale_color_gradient(low='green',high='red')
    })
  }
))