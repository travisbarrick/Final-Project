#Scrapping web data
install.packages("rvest")
library(rvest)
library(xml2)
url<-"http://www.espn.com/nfl/boxscore?gameId=400874589"
doc<-xml2::read_html(url)
tables<-rvest::html_table(doc,fill=TRUE)
tables
