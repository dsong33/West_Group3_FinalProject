library(leaflet)
library(sf)
library(tidyverse)
library(ggplot2)

####################### Abandoned Properties #######################
houses <- st_read('Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp')
head(houses)
str(houses)
houses_data <- houses[!is.na(houses$Outcome_St),] # %>% filter(Outcome_St == 'Demolished')
houses_data$Outcome_St <- as.factor(houses_data$Outcome_St)

##########What are the status of these abandoned properties?
#Answer: Most of them are demolished. There are 716 demolished houses.
houses_data %>% group_by(Outcome_St) %>% summarise(count = n())

ggplot(houses_data, aes(x=Outcome_St)) + geom_bar(fill = "blue")

length(which(houses_data$Outcome_St == 'Demolished')) #716


##########Where the demolished houses are located by zipcode?
#Answer: Most demolished properties are located at 46625, 46619, and 46613. 
houses_data %>% filter(Outcome_St == 'Demolished') %>% 
  group_by(Zip_Code) %>% summarise(count=n())

houses_data$Zip_Code <- as.factor(houses_data$Zip_Code)

houses_data %>% filter(Outcome_St == 'Demolished') %>% 
  ggplot(data = ., aes(x=Zip_Code)) + geom_bar(fill = 'blue')

##########Out of all demolished houses, how many of them are not for bidding?
#Answer: There are 345 properties are abandoned and not for bidding yet. 
#These properties could be the places where homeless people gather for sheltering. 
houses_data %>% filter(Outcome_St == 'Demolished') %>% 
  group_by(Program_De) %>% summarize(count = n()) %>% as.data.frame()

##########Ratio of demolished houses in each zip code
a <- houses_data %>% filter(Outcome_St == 'Demolished') %>% group_by(Zip_Code) %>% summarise(count=n()) %>% as.data.frame()
a$Zip_Code <- as.factor(a$Zip_Code)
ggplot(a, aes(x = "", y = count, fill = Zip_Code))+geom_bar(width = 1, stat = 'identity') +
  coord_polar('y', start = 0) 
#+scale_fill_brewer(palette = 'Blues')+theme_minimal()
#+geom_text(aes(y = count/10 + c(0, cumsum(count)[-length(count)]), label = percent(count/100)), size = 5)

a$zip_percentage <- 100*(a$count/sum(a$count))
pie(a$zip_percentage, labels = c('46601', '46613', "46614", '46615', '46616', '46617', '46618', '46619', '46623','46625', '46628','46637'))

################################# Parks and Schools #######################################
#Where are the parks? What kind of parks?
parks <- read_csv('Parks_Locations_and_Features.csv')
parks$Park_Type <- as.factor(parks$Park_Type)
parks$Zip_Code <- as.factor(parks$Zip_Code)
parks %>% select(Park_Type, Zip_Code) %>% group_by(Zip_Code, Park_Type) %>% summarise(count = n()) %>% 
  ggplot(., aes(x = Zip_Code, y = count, fill = Park_Type))+geom_bar(position = 'stack', stat = 'identity')

#Parks spatial map
parks.spatial$popup <- paste('<b>', parks.spatial$Park_Name,"</b><br>","Type: ", parks.spatial$Park_Type)
parks.spatial <- parks %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
leaflet() %>% addTiles() %>% addMarkers(data = parks.spatial, popup = ~popup)

#Where are the schools?
#There are twice as many private schools than public schools in South Bend.
schools <- st_read('School_Boundaries/School_Boundaries.shp')
schools$SchoolType <- as.factor(schools$SchoolType)
ggplot(schools, aes(x = SchoolType))+geom_bar()

#Schools spatial map
#schools.spatial$popup <- paste('<b>', schools.spatial$School,"</b><br>","Type: ", schools.spatial$SchoolType)
schools.spatial <- schools %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
leaflet() %>% addTiles() %>% addPolygons(data = schools)

#School map by type(Public/Private)
my_left=-86.40
my_top=41.75
my_right=-86.19
my_bot=41.60
lawn_map <- get_stamenmap(bbox=c(left=my_left,bottom=my_bot,right=my_right,top=my_top),zoom=14,"toner")
ggmap(lawn_map) +
  geom_sf(data = schools.spatial, aes(col = SchoolType), show.legend = "point", inherit.aes = F)+
  guides(fill = guide_legend(override.aes = list(colour = NA)))

#Schools and parks combined
ggplot() + geom_sf(data = schools, aes(fill = SchoolType)) + 
  geom_sf(data = parks.spatial, aes(col = Park_Type), show.legend = 'point') + 
  guides(fill = guide_legend(override.aes = list(color = NA)))

##################### Census #######################
#Doesn't make much sense to me
census <- st_read('020_CensusData/2020_CensusData.shp')
glimpse(census)

##################### Street Lights ####################
lights <- read_csv('Street_Lights.csv')
lights.spatial <- lights %>% st_as_sf(coords = c('Lon', 'Lat')) %>% st_set_crs(value = 4326)
leaflet() %>% addTiles() %>% addMarkers(data = lights.spatial)
