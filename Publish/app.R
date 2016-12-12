---
title: "Test"
author: "Travis Barrick"
date: "December 7, 2016"
output: ioslides_presentation
runtime: shiny
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Shiny Presentation

This R Markdown presentation is made interactive using Shiny. The viewers of the presentation can change the assumptions underlying whats presented and see the results immediately. 

To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).

## Interactive Plot

```{r eruptions}
library(readr)
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plotly)) install.packages("plotly")
if (!require(leaflet)) install.packages("leaflet")
if (!require(htmltools)) install.packages("htmltools")
if (!require(DT)) install.packages("DT")
if (!require(stringr)) install.packages("stringr")
if (!require(ggmap)) install.packages("ggmap")
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(htmltools)
library(DT)
library(stringr)
library(ggmap)

##### Data Import #####
healthcare <- read_csv("Healthcare_Data.csv")
zip<-read_csv("zipcode.csv")
population <- read_csv("2010_Census_Population.csv")
DRG_categories <- read_csv("Diagnosis_Category2.csv")
COLA <- read_csv("COLA by State.csv")
statesplus <- read.csv("http://www.hofroe.net/data/statesplus.csv")
general_info <- read_csv("Hospital_General_Information.csv")[c(1,9:11,13)]
general_info$`Provider ID` <- substr(general_info$`Provider ID`,regexpr("[^0]",general_info$`Provider ID`),nchar(general_info$`Provider ID`)) ##removes leading zero's from Provider ID so merge works

#### Merge datatsets together ####
Analysis_Dataset <- merge(healthcare, zip, by.x = "ProviderZipCode", by.y = "zip")
Analysis_Dataset <- merge(Analysis_Dataset, population, by.x = "ProviderZipCode", by.y = "Zip_Code")
Analysis_Dataset <- merge(Analysis_Dataset, DRG_categories, by.x = "DRGDefinition", by.y = "DRGDefinition")
Analysis_Dataset <- merge(Analysis_Dataset, general_info, by.x = "ProviderId", by.y = "Provider ID")
Analysis_Dataset <- merge(Analysis_Dataset, COLA, by.x = "ProviderState", by.y = "State")

### Create new variables  ###
Analysis_Dataset$TreatmentCost = Analysis_Dataset$AverageTotalPayments + Analysis_Dataset$AverageCoveredCharges
Analysis_Dataset$Is_Iowa <- Analysis_Dataset$ProviderState ==  "IA"

#### Adjust new variables ####
Analysis_Dataset$Category<-as.factor(Analysis_Dataset$Category)


#### Graph 1 uses Analysis_Dataset ####

#### Graph 2 #####
AverageCategory<-Analysis_Dataset %>% group_by(Category,ProviderState,Name,ProviderName,ProviderStreetAddress,ProviderCity,latitude,longitude,Rating) %>% 
  summarise(AverageCost=mean(TreatmentCost)) %>% 
  arrange(AverageCost)

AverageCategory$Name<-str_to_title(AverageCategory$Name)
AverageCategory$ProviderName<-str_to_title(AverageCategory$ProviderName)
AverageCategory$ProviderCity<-str_to_title(AverageCategory$ProviderCity)
 
##### Graph 3 (both tabs) #####
### Tab 1
COLA_Avg_Cost_by_Category  <- 
  Analysis_Dataset %>% group_by(Category, ProviderState, Name, Index, Rank) %>%
  summarise(
    avg_cost = mean(TreatmentCost)) 

COLA_Avg_Cost_by_Category$COLA_avg_cost <- COLA_Avg_Cost_by_Category$avg_cost * COLA_Avg_Cost_by_Category$Index / 100

IA_Avg_Cost_by_Category <-
  COLA_Avg_Cost_by_Category %>% 
  filter(ProviderState == "IA") %>% ungroup() %>%
  select(Category,ProviderState,avg_cost)

colnames(IA_Avg_Cost_by_Category)[1] <- "IACompCategory"
colnames(IA_Avg_Cost_by_Category)[2] <- "IACompState"
colnames(IA_Avg_Cost_by_Category)[3] <- "IACompCost"


COLA_Mid_Point_Analysis_Table <- merge(IA_Avg_Cost_by_Category, COLA_Avg_Cost_by_Category, by.x = "IACompCategory", by.y = "Category")
COLA_Mid_Point_Analysis_Table$Mean_Cost_Difference_to_Iowa <- COLA_Mid_Point_Analysis_Table$COLA_avg_cost - COLA_Mid_Point_Analysis_Table$IACompCost   
COLA_Mid_Point_Analysis_Table$Mean_Cost_Percentage_Difference_to_Iowa <- ((COLA_Mid_Point_Analysis_Table$COLA_avg_cost - COLA_Mid_Point_Analysis_Table$IACompCost)/COLA_Mid_Point_Analysis_Table$IACompCost)*100

colnames(COLA_Mid_Point_Analysis_Table)[1] <- "Category"
colnames(COLA_Mid_Point_Analysis_Table)[2] <- "State"

states<-map_data("state")
mapstates<-merge(statesplus,COLA_Mid_Point_Analysis_Table,by.x ="region", by.y ="Name")

#### Tab 2
All_State_Midpoint_Table <- COLA_Avg_Cost_by_Category[c(1,2,6)]
Iowa_Midpoint_Table <- IA_Avg_Cost_by_Category

colnames(Iowa_Midpoint_Table)[3] <- "Iowa_Mean_Treatment_Cost"
colnames(Iowa_Midpoint_Table)[2] <- "State"
colnames(All_State_Midpoint_Table)[3] <- "Average_Treatment_Cost"

Mid_Point_Analysis_Table <- merge(All_State_Midpoint_Table, Iowa_Midpoint_Table, by.x = "Category", by.y = "IACompCategory")

Mid_Point_Analysis_Table$Mean_Cost_Difference_to_Iowa <- Mid_Point_Analysis_Table$Average_Treatment_Cost - Mid_Point_Analysis_Table$Iowa_Mean_Treatment_Cost

Mid_Point_Analysis_Table$Mean_Cost_Percentage_Difference_to_Iowa <- ((Mid_Point_Analysis_Table$Average_Treatment_Cost - Mid_Point_Analysis_Table$Iowa_Mean_Treatment_Cost)/Mid_Point_Analysis_Table$Iowa_Mean_Treatment_Cost) * 100


Mid_Point_Analysis_Table <-
  Mid_Point_Analysis_Table %>%
  select(Category,ProviderState,Mean_Cost_Difference_to_Iowa, Mean_Cost_Percentage_Difference_to_Iowa)


secondmap<-merge(Mid_Point_Analysis_Table,COLA[1:2],by.x="ProviderState",by.y="State")
mapstates2<-merge(statesplus,secondmap,by.x ="region", by.y ="Name")
```

## Bullets

- Bullet 1
- Bullet 2
- Bullet 3

## R Output

```{r cars}
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      HTML("Hover over points for information"),
      selectInput("State", "Select States", 
                  choices=c("AK"	,"AL"	,"AR"	,"AZ"	,"CA"	,"CO"	,"CT"	,"DC"	,"DE"	,"FL"	,"GA"	,"HI"	,"IA"	,"ID"	,"IL"	,"IN"	,"KS"	,"KY"	,"LA"	,"MA"	,"MD"	,"ME"	,"MI"	,"MN"	,"MO"	,"MS"	,"MT"	,"NC"	,"ND"	,"NE"	,"NH"	,"NJ"	,"NM"	,"NV"	,"NY"	,"OH"	,"OK"	,"OR"	,"PA"	,"RI"	,"SC"	,"SD"	,"TN"	,"TX"	,"UT"	,"VA"	,"VT"	,"WA"	,"WI"	,"WV"	,"WY"
                  ),
                  selected="IA", 
                  multiple=FALSE),
      width = 3
    ),
    
    mainPanel(
      
      # this is an extra div used ONLY to create positioned ancestor for tooltip
      # we don't change its position
     
        plotlyOutput("scatterplot"),
     

      width = 8
    )
  )
)
server <- function(input, output,session) {

  
  output$scatterplot <- renderPlotly({
    m<-Analysis_Dataset %>%
      filter(ProviderState==input$State)%>%
      group_by(ProviderState,ProviderName,Category) %>%
      summarize(NumberofPatients = sum(TotalDischarges))
    
    gg<-ggplotly(ggplot(m,aes(Category,NumberofPatients,label=ProviderName,color=Category))+
      geom_point() + 
      geom_jitter() + 
      ylab("# of Patients")+
      coord_flip()+
      guides(color=FALSE))
gg
  })
  
  

}
shinyApp(ui = ui, server = server)
```


