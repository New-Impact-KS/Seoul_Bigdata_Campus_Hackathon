
# 6. 통합 데이터 프레임 생성 및 상관분석 ####

df_count_sgg <- merge(crimeScene, cctv_count_sgg, by = "자치구명")
df_count_sgg <- merge(df_count_sgg, road_count_sgg, by = "자치구명")
df_count_sgg <- merge(df_count_sgg, ktPop_count_sgg, by = "자치구명")

'''
    [cctv_count_sgg | road_count_sgg | ktPop_count_sgg | crimeScene]
    library(readr)
    write.csv(df_count_sgg, "df_count_sgg.csv")
'''

str(df_count_sgg)
df_count_sgg$강력범죄발생빈도 <- as.integer(df_count_sgg$강력범죄발생빈도)

cor <- cor(df_count_sgg[, -1], method = "pearson", use = "pairwise.complete.obs")
corrplot(cor, 
         method = "number",
         type = "lower",
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         diag = F)

pairs(df_count_sgg[, -1], col = "blue")


'''
    [상관분석 결과 기반의 다중회귀분석 시 대립가설 수립]
    H1 : cctv 수 -> 강력범죄발생빈도 (+) r = 0.35
    H2 : 세로_막다른도로 수 -> 강력범죄발생빈도 (-) r = -0.20
    H3 : 유동인구 수 -> 강력범죄발생빈도 (+) r = 0.84
'''



# 7. lm1. 다중회귀분석 ####

lm1 <- lm(강력범죄발생빈도 ~ 
                    cctv수 + 세로_막다른도로수 + 유동인구수,
                  data = df_count_sgg)

summary(lm1) 
# [lm1 회귀식은 통계적으로 유의함] [R^2 = 0.7193,	Adjusted R^ = 0.6792]
plot(lm1)
# df_count_sgg[c(8, 24, 17), ]
shapiro.test(df_count_sgg$강력범죄발생빈도)
# [정규성 조건 충족] p-value = 0.1938
vif(lm1)
# [다중공선성 X 조건 충족]
# cctv수 세로_막다른도로수        유동인구수
# 1.146999        1.075666          1.193518 
durbinWatsonTest(lm1)
# [잔차의 독립성 조건 충족]
# D-W Statistic p-value
#      1.447063   0.146

lm1_f <- step(lm1, direction = "forward")
lm1_b <- step(lm1, direction = "backward")
lm1_s <- step(lm1, direction = "both")


lm.beta(lm1)
vip(lm1)
# [인터넷을 이용할 수 없어 R 버전 업데이트 불가능]
# 버전에 맞지 않는 변수로 변수 중요도 확인 불가능




# 7. lm2. 다중회귀분석 ####


crimeScene <- crimeScene %>% arrange(desc(강력범죄발생빈도))
crimeScene$강력범죄발생빈도 <- as.numeric(crimeScene$강력범죄발생빈도)
summary(crimeScene)
ggplot(data = crimeScene, aes(x = 자치구명, y = 강력범죄발생빈도, fill = factor(자치구명))) + geom_col() + coord_flip()
df_count_sgg$자치구명 <- as.factor(df_count_sgg$자치구명)
df_count_sgg$자치구명 <- factor(df_count_sgg$자치구명,
                            levels = c("강동구",
                                       "강남구", "강북구", "강서구", "관악구", "광진구", "구로구", "금천구",
                                       "노원구", "도봉구", "동대문구", "동작구", "마포구", "서대문구", "서초구", 
                                       "성동구", "성북구", "송파구", "양천구", "영등포구", "용산구", "은평구",
                                       "종로구", "중구", "중랑구"))

df_count_sgg <- df_count_sgg %>% mutate(생활권 = case_when(
  자치구명 == "동대문구" | 자치구명 == "중랑구" | 자치구명 ==  "성동구" |자치구명 ==   "광진구" ~ "동북일생활권",
  자치구명 == "강북구" | 자치구명 == "노원구" | 자치구명 ==  "도봉구" |자치구명 ==   "성북구" ~ "동북이생활권",
  자치구명 == "강남구" | 자치구명 ==  "서초구" ~ "동남일생활권",
  자치구명 == "강동구" |  자치구명 == "송파구" ~ "동남이생활권",
  자치구명 == "종로구"|  자치구명 == "중구" | 자치구명 ==  "용산구" ~ "도심권",
  자치구명 == "은평구" | 자치구명 ==  "서대문구" | 자치구명 ==  "마포구" ~ "서북생활권",
  자치구명 == "강서구" | 자치구명 ==  "양천구" ~ "서남일생활권",
  자치구명 == "구로구" | 자치구명 ==  "금천구" | 자치구명 ==  "영등포구" ~ "서남이생활권",
  자치구명 == "관악구" | 자치구명 ==  "동작구" ~ "서남삼생활권"
))
# [범죄발생빈도가 중간값 3176에 가까운 "강동구"가 속한 "동남2생활권"을 더미변수의 레퍼런스로 설정]
table(df_count_sgg$생활권)
df_count_sgg$생활권 <- factor(df_count_sgg$생활권,
                           levels = c("동남이생활권",
                                      "도심권", "동남일생활권", "동북이생활권", "동북일생활권", 
                                      "서남삼생활권", "서남이생활권", "서남일생활권", "서북생활권"))

lm2 <- lm(강력범죄발생빈도 ~ 
                    cctv수 + 세로_막다른도로수 + 유동인구수 + 생활권,
                  data = df_count_sgg)

summary(lm2)
# [lm2 회귀식은 통계적으로 유의함] [R^2 = 0.8922,	Adjusted R^ = 0.801]
plot(lm2)
shapiro.test(df_count_sgg$강력범죄발생빈도)
# [정규성 조건 충족] p-value = 0.1938
vif(lm2)
# [다중공선성 X 조건 충족]
#                     GVIF Df GVIF^(1/(2*Df))
# cctv수            2.011881  1        1.418408
# 세로_막다른도로수 2.039191  1        1.428003
# 유동인구수        4.020385  1        2.005090
# 생활권            8.793409  8        1.145539

durbinWatsonTest(lm2)
# [잔차의 독립성 조건 충족]
# D-W Statistic p-value
#      2.22187   0.676

lm2_f <- step(lm2, direction = "forward")
lm2_b <- step(lm2, direction = "backward")
lm2_s <- step(lm2, direction = "both")
# [모형적합도지표 AIC값 가장 낮은 경우] 강력범죄발생빈도 ~ 유동인구수 + 생활권


# [lm1 회귀식은 통계적으로 유의함] [R^2 = 0.7193,	Adjusted R^ = 0.6792]
# [lm2 회귀식은 통계적으로 유의함] [R^2 = 0.8922,	Adjusted R^ = 0.801]
anova(lm1, lm2)
# [lm1보다 lm2가 통계적으로 유의한 증가하는 결정계수 차이가 아님] lm2가 lm1보다 설명력 좋다고 말할 수 없음




# 7. lm3. 다중회귀분석 ####

lm3 <- lm(log(강력범죄발생빈도) ~ 
            scale(cctv수) + scale(세로_막다른도로수) + scale(유동인구수) + 생활권, 
          data = df_count_sgg)

summary(lm3)
plot(lm3)
shapiro.test(df_count_sgg$강력범죄발생빈도)
vif(lm3)
durbinWatsonTest(lm3)

lm3_f <- step(lm3, direction = "forward")
lm3_b <- step(lm3, direction = "backward")
lm3_s <- step(lm3, direction = "both")

# [lm1 회귀식은 통계적으로 유의함] [R^2 = 0.7193,	Adjusted R^ = 0.6792]
# [lm3 회귀식은 통계적으로 유의함] [R^2 = 0.8375,	Adjusted R^ = 0.7]
anova(lm1, lm3)
# [lm1보다 lm3가 통계적으로 유의한 증가하는 결정계수 차이] 2.545e-06, lm3가 lm1보다 설명력 좋음
# Yi(hat) = 8.01996 + 0.06025 * X1 + 0.06025 * X2 + 0.27544 * X3 + 
# 0.19848 * dv1 + 0.05466 * dv2 + -0.09778 * dv3 + -0.0163* dv4 + 0.15376 * dv5 + 0.35021 * dv6 + 0.16213 * dv7 + -0.08311 * dv8

df_count_sgg$생활권 <- as.numeric(df_count_sgg$생활권)
df_count_sgg$interaction <- df_count_sgg$유동인구수 * df_count_sgg$생활권
# [상호작용변수 설정]
lm4 <- lm(log(강력범죄발생빈도) ~ 
            scale(cctv수) + scale(세로_막다른도로수) + scale(유동인구수) + 생활권 + interaction, 
          data = df_count_sgg)

summary(lm4)
anova(lm3, lm4)
# [상호작용변수의 조절효과 없음]
# 범죄빈발시간 내 유동인구수가 증가할 때 -> 강력범죄발생빈도가 증가한다는 인과관계에
# 생활권이 동남2생활권일 때(0) 대비 타 생활권일 때(1) 상기된 상호작용을 강화하지 못함




# 8. 군집분석 
# [K-means clustering] [Hiearacy clustering] 수행 가능하나 [Density Based clustering]은 
# R 버전 업데이트 불가능한 VM 환경 문제로 진행 불가능


# [K-means clustering] ####
set.seed(12345)

df_count_sgg <- df_count_sgg[, -7]
table(df_count_sgg$자치구명)
df_count_sgg$시군구명 <- as.numeric(df_count_sgg$자치구명)
table(df_count_sgg$시군구명)

fviz_nbclust(df_count_sgg[, -c(1, 6, 7)], kmeans,
             method = "wss", k.max = 10, verbose = FALSE)
# iVAT(df_count_sgg[, -c(1, 6)]) : iVAT() 사용 불가능

kmclust <- kmeans(df_count_sgg[, -c(1, 6, 7)], 4)
kmclust$centers
kmclust$size

fviz_cluster(kmclust, data = df_count_sgg[, -c(1, 6, 7)], palette = "Set2")

df_sgg_kmclust <- df_count_sgg
df_sgg_kmclust$cluster <- kmclust$cluster
df_sgg_kmclust <- df_sgg_kmclust %>% arrange(cluster)


'''
   [K-means 군집분석 변수]
   cctv수 | KT 야간 유동인구수 | 세로_막다른 도로 수 | 강력범죄발생빈도
   cluster의 거리 비유사도가 가시적인 K = 4 채택
'''


# [Hiearacy clustering] ####

hclust <- hclust(dist(df_count_sgg[, -c(1, 6, 7)]), method = "ward.D2")
summary(hclust)
plot(hclust, cex = 0.6, hang = -1)
rect.hclust(hclust, k = 3, border = "red")

fit = cutree(hclust, 3)
fit
df_sgg_hclust <- df_count_sgg
table(fit, df_sgg_hclust$자치구명)
df_sgg_hclust$cluster <- fit
df_sgg_hclust <- df_sgg_hclust %>% arrange(cluster)



# 9. 시군구 아닌 행정동별 통합 데이터 프레임 생성 (입지 분석 사전 데이터 전처리) ####

ktPop_count_hjd <- ktPopulation[, c(3, 4)]
road_count_hjd <- road2 %>% group_by(행정동명) %>% summarise(세로_막다른도로수 = n())
cctv_count_hjd <- cctv_005 %>% group_by(행정동명, 자치구명) %>% summarise(cctv수 = n())

df_count_hjd <- full_join(ktPop_count_hjd, road_count_hjd, by = "행정동명")
df_count_hjd <- full_join(df_count_hjd, cctv_count_hjd, by = "행정동명")
df_count_hjd <- df_count_hjd %>% relocate(자치구명, .before = 행정동명) %>% arrange(desc(세로_막다른도로수))
df_count_hjd <- df_count_hjd %>% arrange(cctv수)
# write.csv(df_count_hjd, "df_count_hjd.csv")



# 10. 우발적 강력범죄 발생가능 입지 분석 (지도 시각화) ####
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

#서울 법정동 지도 시각화

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

#법정동 행정코드
EMD_Code <- read.csv("법정동_행정코드.csv") 
names(EMD_Code) <- c("id", "id_name")

df_seoul_EMD <- merge(df_seoul_EMD, EMD_Code, by = "id")

#골목길

#도로 좌표 구하기
centroid_shp <- st_centroid(road_shp)
head(centroid_shp)
XY <- st_coordinates(centroid_shp)
centroid_road <- cbind(centroid_shp, XY)

#X,Y -> 위도,경도
#CRS function 생성

convertCRS <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string = from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}

from.crs <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs"
#"+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs" 
#"+prof=termc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#X,Y좌표만 뽑아서 coord 생성
coord <- data.frame(grs.long=centroid_road$X, grs.lat=centroid_road$Y)
str(coord)
head(coord)

#위도 경도 변환
coord <- cbind(coord, convertCRS(coord$grs.long, coord$grs.lat, from.crs, to.crs))
head(coord)

#join 기준 변수 만들기
centroid_road_2 <- centroid_road %>% mutate(index=paste0(X,',',Y))
head(centroid_road_2)
coord2 <- coord %>% mutate(index = paste0(grs.long, ',', grs.lat)) %>% select(index, long, lat)

final_road <- left_join(centroid_road_2, coord2, by="index") %>% select(-"index")
head(final_road)


#final_road 에서 골목길만 추출
names(final_road)
road_map <- final_road %>% select(
  BSI_INT, SIG_CD, RBP_CN, REP_CN,
  WDR_RD_CD, RN, RN_CD, ROAD_BT, ROAD_CD, ROAD_LT, long, lat
)
road_map <- data.frame(road_map)
colnames(road_map) <- c("기초간격", "시군구코드", "기점", "종점",
                     "광역도로구분코드", "도로명", "도로명코드", 
                     "도로폭", "도로코드", "도로길이", "경도", "위도", "geometry")
road_map <- road_map %>% relocate(도로길이, .after = 도로폭)
road_map <- road_map %>% relocate(광역도로구분코드, .before = 도로폭)
road_map <- road_map %>% relocate(종점, .after = 도로코드)
road_map <- road_map %>% relocate(기점, .after = 도로코드)
road_map <- road_map %>% relocate(기초간격, .before = 도로폭)
str(road_map$시군구코드)
table(road_map$광역도로구분코드)


road_map <- road_map %>% filter(광역도로구분코드 != 2)
road_map <- road_map %>%  mutate(막다른도로 = case_when(도로폭 <= 6 & 도로길이 >= 35 ~ "1번", 
                                                도로폭 <= 3  & 도로길이 < 35 ~ "2번",
                                                도로폭 <= 2  & 도로길이 < 10 ~ "3번"))
road_map <- road_map %>% filter(도로폭 <= 4)
road_map <- road_map %>% relocate(막다른도로, .after = 도로길이)



final_road_map <- full_join(road_map, SGG_code, by = "시군구코드")
final_road_map <- final_road_map %>% relocate(자치구명, .after = 시군구코드)
final_road_map[is.na(final_road_map$자치구명), ]


final_road_map$행정동명 <- substr(final_road_map$기점, 1, 4)
final_road_map <- final_road_map %>% relocate(행정동명, .after = 자치구명)
final_road_map[which(str_detect(final_road_map$행정동명, "동") == FALSE), 3]
final_road_map[which(str_detect(final_road_map$행정동명, "동") == FALSE), 3] <- NA


which(final_road_map$자치구명%in% c("강북구", "금천구", "광진구"))
final_road_map <- final_road_map[-c(39796, 39797, 39798), ]

final_road_map$자치구명 <- ifelse(final_road_map$시군구코드 == "11215", "광진구", final_road_map$자치구명)
final_road_map$자치구명 <- ifelse(final_road_map$시군구코드 == "11305", "강북구", final_road_map$자치구명)
final_road_map$자치구명 <- ifelse(final_road_map$시군구코드 == "11545", "금천구", final_road_map$자치구명)
final_road_map <- final_road_map %>% filter(!is.na(final_road_map$자치구명))

#위도, 경도도 같이 불러와져야하기 때문에 행정동명 없는 데이터 수정해서 불러오는 건 하지 않음


# 2) 유동인구 많은 구, 행정동 내의 골목길 위치 표시하기

ys_dong <- seoul_EMD %>% filter(id_name == "역삼동")
ys_road <- final_road_map %>% filter(행정동명 == "역삼동 ")
ys_cctv <- cctv_005 %>% filter(행정동명 == "역삼1동")

ys_road$경도 <- as.numeric(ys_road$경도)
ys_road$위도 <- as.numeric(ys_road$위도)
ys_cctv$경도 <- as.numeric(ys_cctv$경도)
ys_cctv$위도 <- as.numeric(ys_cctv$위도)

ggplot()+geom_polygon(data = ys_dong, aes(x=long, y=lat, group=group, color=id), fill="#FFFFFF", size = 1.2) + theme(legend.position = "none")+ geom_point(data = ys_cctv, aes(x=경도, y=위도), color="blue")

write.csv(road2, "road2.csv")

