
install.packages("dplyr")
install.packages("readr")
install.packages("ggmap")
install.packages("leaflet")

library(dplyr)
library(leaflet)
library(ggmap)

road <- read.csv("road.csv")
register_google(key = "# google_map_key #")

which(is.na(road$기점)==TRUE)
road <- road[-c( 954, 26151 ,26161 ,26198, 26233 ,26274, 26387, 26407, 26426, 26494, 26510, 39767, 39768, 39769),]

road_관악 <- road %>% filter(자치구명 == "관악구")
road_관악_location <- mutate_geocode(data = road_관악, location = 기점, source = 'google')
#관악_map <- get_googlemap('관악', maptype = 'roadmap', zoom = 13)

leaflet() %>% 
  addTiles() %>% 
  setView(lng=126.9784, lat=37.566, zoom=11)

pal <- colorFactor("viridis", road_관악_location$행정동명)

leaflet(road_관악_location) %>% 
  setView(lng=126.9784, lat=37.566, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(lng = ~lon, lat= ~lat, color= ~pal(행정동명))

road_관악_location$lon

which(is.na(road_관악$기점)==TRUE)
road_관악$기점

road_강남 <- road %>% filter(자치구명 == "강남구")
road_강남_location <- mutate_geocode(data = road_강남, location = 기점, source = 'google')
