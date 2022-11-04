
library(readr)
df <- read.csv("df_count_Sgg.csv", header = TRUE, fileEncoding = "UTF-8")
df$생활권 <- as.factor(df$생활권)
lm3 <- lm(log(강력범죄발생빈도) ~
          log(cctv수) + log(세로_막다른도로수) + log(유동인구수) + 생활권,
          data = df)
summary(lm3)
library(lm.beta)
lm.beta(lm3)
library(car)

lm1 <- lm(강력범죄발생빈도 ~
          cctv수 + 세로_막다른도로수 + 유동인구수 + 생활권,
          data = df)
summary(lm1)
lm.beta(lm1)


setwd("C:/CDA")
library(readr)
road <- read.csv("road.csv", header = TRUE, sep = ",")
table(is.na(road$기점))
road <- road[-which(is.na(road$기점)), ]


library(ggmap)
library(ggplot2)
library(dplyr)
register_google(key = "AIzaSyDMLBeyvYGcL7sRn0usaEIcRHehy5gha1k")

table(road$자치구명)

road_강서 <- road %>% filter(자치구명 == "강서구")
road_양천 <- road %>% filter(자치구명 == "양천구")
road_구로 <- road %>% filter(자치구명 == "구로구")
road_금천 <- road %>% filter(자치구명 == "금천구")
road_관악 <- road %>% filter(자치구명 == "관악구")
road_영등포 <- road %>% filter(자치구명 == "영등포구")
road_동작 <- road %>% filter(자치구명 == "동작구")
road_서초 <- road %>% filter(자치구명 == "서초구")
road_강남 <- road %>% filter(자치구명 == "강남구")
road_송파 <- road %>% filter(자치구명 == "송파구")
road_강동 <- road %>% filter(자치구명 == "강동구")
road_마포 <- road %>% filter(자치구명 == "마포구")
road_용산 <- road %>% filter(자치구명 == "용산구")
road_성동 <- road %>% filter(자치구명 == "성동구")
road_광진 <- road %>% filter(자치구명 == "광진구")
road_중랑 <- road %>% filter(자치구명 == "중랑구")
road_동대문 <- road %>% filter(자치구명 == "동대문구")
road_중구 <- road %>% filter(자치구명 == "중구")
road_서대문 <- road %>% filter(자치구명 == "서대문구")
road_은평 <- road %>% filter(자치구명 == "은평구")
road_성북 <- road %>% filter(자치구명 == "성북구")
road_강북 <- road %>% filter(자치구명 == "강북구")
road_도봉 <- road %>% filter(자치구명 == "도봉구")
road_노원 <- road %>% filter(자치구명 == "노원구")
road_종로 <- road %>% filter(자치구명 == "종로구")



# gc <- geocode(enc2utf8(address))

'''
road_address_강남 <- geocode(enc2utf8(road_강남$기점))
road_address_강동 <- geocode(enc2utf8(road_강동$기점))
road_address_강북 <- geocode(enc2utf8(road_강북$기점))
road_address_강서 <- geocode(enc2utf8(road_강서$기점))
road_address_관악 <- geocode(enc2utf8(road_관악$기점))
road_address_광진 <- geocode(enc2utf8(road_광진$기점))
road_address_구로 <- geocode(enc2utf8(road_구로$기점))
road_address_금천 <- geocode(enc2utf8(road_금천$기점))
road_address_노원 <- geocode(enc2utf8(road_노원$기점))
road_address_도봉 <- geocode(enc2utf8(road_도봉$기점))
road_address_동대문 <- geocode(enc2utf8(road_동대문$기점))
road_address_동작 <- geocode(enc2utf8(road_동작$기점))
road_address_서대문 <- geocode(enc2utf8(road_서대문$기점))
road_address_서초 <- geocode(enc2utf8(road_서초$기점))
road_address_성동 <- geocode(enc2utf8(road_성동$기점))
road_address_성북 <- geocode(enc2utf8(road_성북$기점))
road_address_송파 <- geocode(enc2utf8(road_송파$기점))
road_address_양천 <- geocode(enc2utf8(road_양천$기점))
road_address_영등포 <- geocode(enc2utf8(road_영등포$기점))
road_address_용산 <- geocode(enc2utf8(road_용산$기점))
road_address_은평 <- geocode(enc2utf8(road_은평$기점))
road_address_중구 <- geocode(enc2utf8(road_중구$기점))
road_address_중랑 <- geocode(enc2utf8(road_중랑$기점))

road_address_마포 <- geocode(enc2utf8(road_마포$기점))
road_address_종로 <- geocode(enc2utf8(road_종로$기점))
'''

'''
write.csv(road_address_강남, "road_address_강남.csv")
write.csv(road_address_강동, "road_address_강동.csv")
write.csv(road_address_강북, "road_address_강북.csv")
write.csv(road_address_강서, "road_address_강서.csv")
write.csv(road_address_관악, "road_address_관악.csv")
write.csv(road_address_광진, "road_address_광진.csv")
write.csv(road_address_구로, "road_address_구로.csv")
write.csv(road_address_금천, "road_address_금천.csv")
write.csv(road_address_노원, "road_address_노원.csv")
write.csv(road_address_도봉, "road_address_도봉.csv")
write.csv(road_address_동대문, "road_address_동대문.csv")
write.csv(road_address_동작, "road_address_동작.csv")
write.csv(road_address_서대문, "road_address_서대문.csv")
write.csv(road_address_서초, "road_address_서초.csv")
write.csv(road_address_성동, "road_address_성동.csv")
write.csv(road_address_성북, "road_address_성북.csv")
write.csv(road_address_송파, "road_address_송파.csv")
write.csv(road_address_양천, "road_address_양천.csv")
write.csv(road_address_영등포, "road_address_영등포.csv")
write.csv(road_address_용산, "road_address_용산.csv")
write.csv(road_address_은평, "road_address_은평.csv")
write.csv(road_address_중구, "road_address_중구.csv")
write.csv(road_address_중랑, "road_address_중랑.csv")


write.csv(road_address_마포, "road_address_마포.csv")
write.csv(road_address_종로, "road_address_종로.csv")
'''
setwd("C:/CDA/자치구별_road.csv")
library(readr)
library(dplyr)
road_관악 <- road %>% filter(자치구명 == "관악구")
road_address_관악 <- read.csv("road_address_관악.csv")
road_관악$lon <- road_address_관악$lon
road_관악$lat <- road_address_관악$lat

road_map <- get_googlemap('서울', maptype = 'roadmap', zoom = 11)
road_map <- get_googlemap('서울특별시 관악구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_관악, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')
road_관악 %>% group_by(행정동명) %>% summarise(count = n())
road_봉천동 <- road_관악 %>% filter(행정동명 == "봉천동") %>% select(lon, lat)
road_신림동 <- road_관악 %>% filter(행정동명 == "신림동") %>% select(lon, lat)

road_map <- get_googlemap('서울특별시 관악구 봉천동', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_봉천동, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'red')

road_map <- get_googlemap('서울특별시 관악구 신림동', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_신림동, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'red')


# 지도에 표시
# geom_point_ver

road_map <- get_googlemap('서울', maptype = 'roadmap', zoom = 11)

road_map <- get_googlemap('서울특별시 강남구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_강남, aes(x=lon, y=lat), size=1, alpha = 0.3, col = "blue")

road_map <- get_googlemap('서울특별시 송파구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_송파, aes(x=lon, y=lat), size=1, alpha = 0.3, col = "blue")

road_map <- get_googlemap('서울특별시 광진구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_광진, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 구로구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_구로, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 동대문구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_동대문, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 동작구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_동작, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 서대문구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_서대문, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 성동구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_성동, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 성북구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_성북, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 양천구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_양천, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 용산구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_용산, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 은평구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_은평, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 종로구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_종로, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 중구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_중구, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 중랑구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_중랑, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'purple')

road_map <- get_googlemap('서울특별시 강동구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_강동, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 강서구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_강서, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 관악구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_관악, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 노원구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_노원, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 마포구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_마포, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 서초구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_서초, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 영등포구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_영등포, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'navy')

road_map <- get_googlemap('서울특별시 강북구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_강북, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'dark green')

road_map <- get_googlemap('서울특별시 금천구', maptype = 'roadmap', zoom = 14)
ggmap(road_map) + geom_point(data = road_address_금천, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'dark green')

road_map <- get_googlemap('서울특별시 도봉구', maptype = 'roadmap', zoom = 13)
ggmap(road_map) + geom_point(data = road_address_도봉, aes(x=lon, y=lat), size=1, alpha = 0.3, col = 'dark green')
