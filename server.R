#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(sf)
library(tidyverse)
library(lubridate)
library(DT)


shinyServer(function(input, output) {
  ## Read datasets
  Street_Lights <- read_csv("Street_Lights.csv")
  Business_Licenses_geocoded <- read_csv("Business_Licenses_geocoded.csv")
  schools <- st_read("School_Boundaries/School_Boundaries.shp", stringsAsFactors = FALSE)
  park <- read.csv("Parks_Locations_and_Features.csv")
  park_spatial <- park %>%st_as_sf(coords = c("Lon","Lat")) %>%st_set_crs(value = 4326)
  Phone_call <- read.csv('311_Phone_Call_Log_Mod.csv', header = T)
  Phone_call$Date<-as.Date(Phone_call$Date, format = "%Y-%m-%d")
  Phone_call$weekday <- wday(Phone_call$Date, label=TRUE)
  theTable <- within(Phone_call,
                     Department <- factor(Department,
                                          levels=names(sort(table(Department),
                                                            decreasing=F))))
  houses <- st_read('Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp')
  houses_data <- houses[!is.na(houses$Outcome_St),]
  houses_data <- houses_data[!is.na(houses_data$Zip_Code),]
  council<-st_read('City_Council_Districts/City_Council_Districts.shp')
  business <- read.csv('Business_Licenses_geocoded.csv', header = T)
  facility <- read.csv('Public_Facilities.csv', header = T)
  
  
  ## page(data)
  data_selected <- reactive({
    switch(input$datasetname,
           "Parks locations and features" = park,
           "Facilities" = facility,
           "311 Phone Call" = Phone_call,
           "Council Districts" = council,
           "Abandoned houses" = houses_data)
  })
  output$data <-DT::renderDataTable(datatable(data_selected()))
  
  park_d <- h5('Description1')
  facility_d <- h5('Description2')
  Phone_call_d <- h5('Description3')
  council_d <- h5('Description4')
  houses_data_d <- h5('Description5')
  
  output$datasetDes<-renderPlot({
    Des_selected <- reactive({
      switch(input$datasetname,
             "Parks locations and features" = park_d,
             "Facilities" = facility_d,
             "311 Phone Call" = Phone_call_d,
             "Council Districts" = council_d,
             "Abandoned houses" = houses_data_d)
    })
    print(Des_selected())
  })
  
  ## EDA1
  output$EDA1 <- renderPlot({
    houses_data %>% group_by(Outcome_St) %>% summarise(count = n())
    ggplot(houses_data, aes(x=Outcome_St)) + geom_bar(fill = "blue")
  })
  
  ## EDA2
  output$EDA2 <- renderPlot({
    aaaaa<- st_read('Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp')
    aaaaa <- aaaaa[!is.na(houses$Outcome_St),]
    aaaaa <- aaaaa[!is.na(houses_data$Zip_Code),]
    aaaaa$Zip_Code<-as.character(aaaaa$Zip_Code)
    aaaaa$Zip_Code<-as.factor(aaaaa$Zip_Code)
    
    if(input$input1=='Zip Code'){
      
    
      aaaaa <- aaaaa %>% filter(Zip_Code == input$input3) %>% group_by(Outcome_St) %>% summarise(count=n()) %>% as.data.frame()
      aaaaa$Outcome_St <- as.factor(aaaaa$Outcome_St)
      #aaaaa$output_percentage <- 100*(aaaaa$count/sum(aaaaa$count))
      pie(aaaaa$count, labels = c("Demolished","Deconstructed","Repaired","Repaired & Occupied","Occupied & Not Repaired"))
      #ggplot(aaaaa, aes(x="", y=count,fill=Outcome_St)) +
       # geom_bar(stat="identity", width=1) +
       # coord_polar("y", start=0)
    }
    if(input$input1=='Status'){
      aaaaa <- aaaaa %>% filter(Outcome_St == input$input2) %>% group_by(Zip_Code) %>% summarise(count=n()) %>% as.data.frame()
      aaaaa$Zip_Code <- as.factor(aaaaa$Zip_Code)
      aaaaa$zip_percentage <- 100*(aaaaa$count/sum(aaaaa$count))
      #pie(aaaaa$zip_percentage, labels = c('46601', '46613', "46614", '46615', '46616', '46617', '46618', '46619', '46623','46625', '46628','46637'))
      ggplot(aaaaa, aes(x="", y=zip_percentage,fill=Zip_Code)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)}
    #print(p)
  })
  
  
  # leaflet1 map
  output$leaflet1 <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
    addPolygons(data = schools)
  })
  
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3","#6fb37b",'#b36f6f','#701a1a','#70611a','#311a70'), 
                     domain = c("Block Park", "Neighborhood Park", "Zoo","Community Park",'Special','Memorial','Cemetery','Golf Course'))
  pal1<- colorFactor(pal=c('red','blue'),domain = c('Private','Public'))
  # leaflet2 map
  output$leaflet2 <- renderLeaflet({
    leaflet(park) %>% 
      addCircles(lng = ~Lon, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = park, lat =  ~Lat, lng =~Lon, 
                       radius = 8, #popup = ~as.character(cntnt), 
                       color = ~pal(Park_Type),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=park$Park_Type,opacity=1)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  # leaflet3 map
  output$leaflet3 <- renderLeaflet({
    leaflet(park) %>% 
      addCircles(lng = ~Lon, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = park, lat =  ~Lat, lng =~Lon, 
                       radius = 5, #popup = ~as.character(cntnt), 
                       color = ~pal(Park_Type),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=park$Park_Type,opacity=1)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addPolygons(data = schools,color = ~pal1(SchoolType))%>%
      addLegend(pal=pal1, values=schools$SchoolType,opacity=1)
    
  })
  
  #leaflet4
  output$leaflet4<-renderLeaflet({
    if(input$mapinput=='Parks')
      b<-leaflet(park) %>% 
      addCircles(lng = ~Lon, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = park, lat =  ~Lat, lng =~Lon, 
                       radius = 8, #popup = ~as.character(cntnt), 
                       color = ~pal(Park_Type),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=park$Park_Type,opacity=1)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    if(input$mapinput=='Schools')
      b<- 
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = schools,color = ~pal1(SchoolType))%>%
          addLegend(pal=pal1, values=schools$SchoolType,opacity=1)
    
    if(input$mapinput=='Both')
      b<-leaflet(park) %>% 
          addCircles(lng = ~Lon, lat = ~Lat) %>% 
          addTiles() %>%
          addCircleMarkers(data = park, lat =  ~Lat, lng =~Lon, 
                           radius = 5, #popup = ~as.character(cntnt), 
                           color = ~pal(Park_Type),
                           stroke = FALSE, fillOpacity = 0.8)%>%
          addLegend(pal=pal, values=park$Park_Type,opacity=1)%>%
          addEasyButton(easyButton(
            icon="fa-crosshairs", title="ME",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
          addPolygons(data = schools,color = ~pal1(SchoolType))%>%
          addLegend(pal=pal1, values=schools$SchoolType,opacity=1)
          
        
      
    print(b)
  })
  output$park_business_map<-renderLeaflet({
    p<-leaflet() %>%
      addTiles()%>%
      addPolygons(data = council,popup = ~popup,color='grey')
    if(input$PARK==TRUE){
      p<-p%>%
        addCircleMarkers(data = park, lat =  ~Lat, lng =~Lon, 
                       radius = 5, #popup = ~as.character(cntnt), 
                       color='black',
                       stroke = FALSE, fillOpacity = 0.8)}
        
    if(input$Fire_Station==TRUE){
      p<-p%>%
        addCircleMarkers(data = facility%>%filter(POPL_TYPE=='FIRE STATION'), lat =  ~Lat, lng =~Lon, 
                         radius = 5, #popup = ~as.character(cntnt), 
                         color='red',
                         stroke = FALSE, fillOpacity = 0.8)}
    if(input$LIBRARY==TRUE){
      p<-p%>%addCircleMarkers(data = facility%>%filter(POPL_TYPE=='LIBRARY'), lat =  ~Lat, lng =~Lon, 
                       radius = 5, #popup = ~as.character(cntnt), 
                       color='green',
                       stroke = FALSE, fillOpacity = 0.8)}
    if(input$Police_Station==TRUE){
      p<-p%>%addCircleMarkers(data = facility%>%filter(POPL_TYPE=='POLICE STATION'), lat =  ~Lat, lng =~Lon, 
                              radius = 5, #popup = ~as.character(cntnt), 
                              color='blue',
                              stroke = FALSE, fillOpacity = 0.8)
    }
    print(p)
  })
  
  #leaflet5
  council$popup <- paste('<b>', council$Council_Me,"</b><br>","Email: ", council$Email)
  houses_demolished <- filter(houses_data,Outcome_St=='Demolished')
  output$leaflet5 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = council,popup = ~popup)%>%
      addPolygons(data=filter(council,Dist==input$concilinput),color = 'red',popup = ~popup)%>%
      addPolygons(data=filter(houses_demolished,is.na(Program_De)==TRUE),color = 'black')
    
    
  })
  
 
  
  # hist
  filtered_table = Phone_call%>%
    group_by(Department,Called_About)%>%
    mutate(mean_duration_seconds=mean(duration_Seconds))%>%
    mutate(duration_minutes = duration_Seconds/60)%>%
    mutate(reason_count = n())%>%
    na.omit()
  reactive_table = reactive({
    filter(filtered_table,between(Date,input$date[1],input$date[2]))
  })
  # hist
  output$distPlot <- renderPlot({
    ggplot(data=reactive_table(), aes(x=fct_rev(fct_infreq(Called_About))))+
      geom_bar(aes(fill=Department))+
      geom_text(stat='count', aes(label=..count..),hjust=-0.5,size=4, position=position_dodge(width = 1))+
      coord_flip()+
      theme(legend.title = element_text(colour="Black", size=12,
                                        face="bold"),
            legend.text=element_text(size=8),
            legend.position = c(0.85,0.925))+
      labs(title='Phone Call Reasons per Department',y='Count',x='Call Reason')+
      theme(axis.text.x=element_text(angle=90,size=12),
            axis.text.y=element_text(size=12),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16),
            plot.title =element_text(hjust=0.5,size=20,face='bold') )
  })
  
})
