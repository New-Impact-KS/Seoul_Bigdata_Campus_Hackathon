
# 10. 우발적 강력범죄 발생가능 입지 분석 (지도 시각화)
#1) CCTV 위치 지도 시각화
#cctv_005

library(ggmap)
register_google(key = "AIzaSyDMLBeyvYGcL7sRn0usaEIcRHehySgha1k")
road_location <- mutate_geocode(data = road2, location = 기점, source = 'google')
head(road_location)



# 11. 단계구분도 ####
# 1) 서울시 행정동 별 CCTV 개수 단계구분도

'''
library(ggiraphExtra)
devtools::install_github("cardiomoon/kormaps2014")
'''

#시군구 코드 포함
cctv_sgg_code <- cctv_005 %>% group_by(시군구코드) %>% summarise(count=n()) 
names(cctv_sgg_code) <- c("id", "count")

'''
#서울시 행정구역 지도화 : "서울시 행정구역 시군구 경계도"
seoul_map <-readOGR("2017/TL_SCCO_SIG.shp") 
slotNames(seoul_map)
seoul_map@polygons[[1]]@Polygons[[1]]@coords[1:4,]

seoul_map@proj4string

ls_crs <- list(wgs84= "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )

seoul_map = spTransform(seoul_map, CRSobj = CRS(ls_crs$wgs84))
seoul_map@proj4string
## Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs 

seoul_map@polygons[[1]]@Polygons[[1]]@coords[1:4,]
##          [,1]     [,2]
## [1,] 127.0087 37.58047
## [2,] 127.0087 37.58045
## [3,] 127.0088 37.58045
## [4,] 127.0089 37.58043

df_seoul_map <- seoul_map@data
head(df_seoul_map,2)

df_map = fortify(seoul_map)
head(df_map)

ggplot(data = df_map,
       aes(x=long, y=lat,
           group = group)) +
  geom_polygon(fill = "#FFFFFF",
               color = "#000000", 
               size = 1.2) +
  theme_bw()
'''

#cctv_map
seoul_map <-readOGR("2017/TL_SCCO_SIG.shp", encoding = "UTF-8") 
slotNames(seoul_map)
seoul_map@polygons[[1]]@Polygons[[1]]@coords[1:4,]
##          [,1]     [,2]
## [1,] 200765.0 553435.5
## [2,] 200771.1 553433.6
## [3,] 200775.8 553432.8
## [4,] 200788.3 553430.7

seoul_map@proj4string
#Deprecated Proj.4 representation: +proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs 

ls_crs <- list(wgs84= "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
seoul_map = spTransform(seoul_map, CRSobj = CRS(ls_crs$wgs84))
## 국제 지표 좌표계로 변환

seoul_map@proj4string
## Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs 

seoul_map@polygons[[1]]@Polygons[[1]]@coords[1:4,]
##          [,1]     [,2]
## [1,] 127.0087 37.58047
## [2,] 127.0087 37.58045
## [3,] 127.0088 37.58045
## [4,] 127.0089 37.58043

df_seoul_map <- seoul_map@data
head(df_seoul_map,2)

df_seoul_map = fortify(seoul_map, region = "SIG_CD")

cctv_map <- merge(df_seoul_map, cctv_sgg_code, by = "id")


#cctv 시군구별 단계 구분도
cctv_graph <- ggplot()+geom_polygon(data = cctv_map, aes(x=long, y=lat, group=group, fill=count))
cctv_graph

#지도 내 지명 추가







#cctv 위도 경도 이미 있음;;;;;
#유동인구 적은 행정동 내 cctv 위치 그리기
# 종로구 창신 3동

changshin3_cctv <- cctv_005 %>% filter(행정동명 == "창신3동")
#11110

#df_seoul_map
df_jong <- df_seoul_map %>% filter(id == "11110")


ggplot()+geom_polygon(data = df_jong, aes(x=long, y=lat, group=group, color = id), fill="#FFFFFF", size = 1.2)+ theme(legend.position = "none")+
  geom_point(data = changshin3_cctv, aes(x=경도, y=위도), color = "red")





#읍면동
seoul_EMD <-readOGR("2015/TC_SPBE17.shp") 
slotNames(seoul_EMD)
seoul_EMD@polygons[[1]]@Polygons[[1]]@coords[1:4,]

seoul_EMD@proj4string

ls_crs <- list(wgs84= "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )

seoul_EMD = spTransform(seoul_EMD, CRSobj = CRS(ls_crs$wgs84))
seoul_EMD@proj4string
## Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs 

seoul_EMD@polygons[[1]]@Polygons[[1]]@coords[1:4,]


df_seoul_EMD <- seoul_EMD@data
head(seoul_EMD,2)

df_seoul_EMD = fortify(seoul_EMD, region = "EMD_CD")
head(df_seoul_EMD)
'''
ggplot(data = df_seoul_EMD,
       aes(x=long, y=lat,
           group = group)) +
  geom_polygon(fill = "#FFFFFF",
               color = "#000000", 
               size = 1.2) +
  theme_bw()
'''

df_jong_changshin <- df_seoul_EMD %>% filter(id == "11110174")
head(df_jong_changshin)
ggplot()+geom_polygon(data = df_jong_changshin, aes(x=long, y=lat, group=group, color = id), fill="#FFFFFF", size = 1.2)+ theme(legend.position = "none")+
  geom_point(data = changshin3_cctv, aes(x=경도, y=위도), color = "red")

str(changshin3_cctv)
changshin3_cctv$경도 <- as.numeric(changshin3_cctv$경도)
changshin3_cctv$위도 <- as.numeric(changshin3_cctv$위도)
str(df_jong_changshin)




#법정동 행정코드
EMD_Code <- read.csv("법정동_행정코드.csv") 
names(EMD_Code) <- c("id", "id_name")

seoul_EMD <- merge(df_seoul_EMD, EMD_Code, by = "id")

cctv_005$위도 <- as.numeric(cctv_005$위도)
cctv_005$경도 <- as.numeric(cctv_005$경도)
ys_dong <- seoul_EMD %>% filter(id_name == "역삼동")
ys_cctv <- cctv_005 %>% filter(행정동명 == "역삼1동")



ggplot()+geom_polygon(data = ys_dong, aes(x=long, y=lat, group=group, color=id), fill="#FFFFFF", size = 1.2) + theme(legend.position = "none")+
  geom_point(data = ys_cctv, aes(x=경도, y=위도), color="red")






'''
골목길
road_raw = readOGR("2016년road/TL_SPRD_MANAGE.shp", encoding="UTF-8")
slotNames(road_raw)
road3 <- road_raw@data
str(road3)

road3 <- road3 %>% select(
  BSI_INT, SIG_CD, RBP_CN, REP_CN,
  WDR_RD_CD, RN, RN_CD, ROAD_BT, ROAD_CD, ROAD_LT
)

colnames(road3) <- c("기초간격", "시군구코드", "기점", "종점",
                     "광역도로구분코드", "도로명", "도로명코드", 
                     "도로폭", "도로코드", "도로길이")
road1 <- road1 %>% relocate(도로길이, .after = 도로폭)
road1 <- road1 %>% relocate(광역도로구분코드, .before = 도로폭)
road1 <- road1 %>% relocate(종점, .after = 도로코드)
road1 <- road1 %>% relocate(기점, .after = 도로코드)
road1 <- road1 %>% relocate(기초간격, .before = 도로폭)
str(road1$시군구코드)
table(road1$광역도로구분코드)


road1 <- road1 %>% filter(광역도로구분코드 != 2)
road1 <- road1 %>%  mutate(막다른도로 = case_when(도로폭 <= 6 & 도로길이 >= 35 ~ "1번", 
                                                도로폭 <= 3  & 도로길이 < 35 ~ "2번",
                                                도로폭 <= 2  & 도로길이 < 10 ~ "3번"))
road1 <- road1 %>% filter(도로폭 <= 4)
road1 <- road1 %>% relocate(막다른도로, .after = 도로길이)


road_raw@polygons[[1]]@Polygons[[1]]@coords[1:4,]
road_raw@
road_raw@proj4string
#Deprecated Proj.4 representation:+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m+no_defs

ls_crs <- list(wgs84= "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
road_raw = spTransform(road_raw, CRSobj = CRS(ls_crs$wgs84))
## 국제 지표 좌표계로 변환

road_map@proj4string
## Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs 

road_map@polygons[[1]]@Polygons[[1]]@coords[1:4,]
##          [,1]     [,2]
## [1,] 127.0087 37.58047
## [2,] 127.0087 37.58045
## [3,] 127.0088 37.58045
## [4,] 127.0089 37.58043

df_road_map <- road_map@data
head(df_road_map,2)

df_road_map = fortify(road_map)




#서울시 지도에 뿌리기
ggplot()+geom_polygon(data = df_seoul_map, aes(x=long, y=lat, group=group), fill = id, )
'''























#도로 전자지도
road_map <- readOGR("서울시_도로구간_위치도/2017/TL_SPRD_MANAGE.shp", encoding="UTF-8")
slotNames(road_map)

road_map@data
road_map@proj4string

ls_crs <- list(wgs84= "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )

road_map = spTransform(road_map, CRSobj = CRS(ls_crs$wgs84))
road_map@proj4string
## Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs 

road_map@polygons[[1]]@Polygons[[1]]@coords[1:4,]


df_road_map <- seoul_map@data
head(df_road_map,2)

seoul_road_map = fortify(road_map)
head(seoul_road_map)


road2_XY <- road2$XY좌표
road2_XY
slotNames(road2_XY)

road_shp
