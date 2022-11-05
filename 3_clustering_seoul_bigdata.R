
# 2022년도 서울특별시 빅데이터 캠퍼스 공모전
# III. 군집분석 

# 8.1. k-평균 군집분석 [-means clustering] ####

set.seed(12345)

df_count_sgg <- df_count_sgg[, -7]
table(df_count_sgg$자치구명)
df_count_sgg$시군구명 <- as.numeric(df_count_sgg$자치구명)
table(df_count_sgg$시군구명)

fviz_nbclust(df_count_sgg[, -c(1, 6, 7)], kmeans,
             method = "wss", k.max = 10, verbose = FALSE)
iVAT(df_count_sgg[, -c(1, 6)])

kmclust <- kmeans(df_count_sgg[, -c(1, 6)], 4)
kmclust$centers
kmclust$size
kmclust$withinss/kmclust$betweenss
kmclust$withinss
kmclust$betweenss

distance <- dist(df_count_sgg[, -c(1, 6)], method= "euclidean")
sil <- silhouette(kmclust$cluster, distance)
plot(sil, col = "blue")

fviz_cluster(kmclust, data = df_count_sgg[, -c(1, 6, 7)], palette = "Set2")

df_sgg_kmclust <- df_count_sgg
df_sgg_kmclust$cluster <- kmclust$cluster
df_sgg_kmclust <- df_sgg_kmclust %>% arrange(cluster)


#전체 실루엣 계수 평균 구하는 함수 정의
avg_sil <- function(k, data) {
  km.res <- kmeans(df_count_sgg[, -c(1, 6)], centers = k)
  ss <- silhouette(km.res$cluster, dist(df_count_sgg[, -c(1, 6)]))
  avgSil <- mean(ss[, 3])
  return(avgSil)
}

#테스트할 군집수
kClusters <-  2:5 

#군집수에 따른 전체 실루엣 계수 결과 저장 변수 생성
resultForEachK <- data.frame(k = kClusters, silAvg = rep(NA, length(kClusters))) 

#전체 실루엣 계수 평균 결과 계산
for(i in 1:length(kClusters)) {
    resultForEachK$silAvg[i] <- avg_sil(kClusters[i], data)
}

#결과 그래프로 그리기 
plot(resultForEachK$k, resultForEachK$silAvg,
      type = "b", pch = 19, frame = FALSE, 
      xlab = "Number of clusters K",
      ylab = "Average Silhouettes")





'''
   [K-means 군집분석 변수]
   cctv수 | KT 야간 유동인구수 | 세로_막다른 도로 수 | 강력범죄발생빈도
   cluster의 거리 비유사도가 가시적인 K = 4 채택
'''



# 8.2. 계층적 군집분석 [Hiearacy clustering] ####

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



# 9. 시군구 아닌 행정동별 통합 데이터 프레임 생성 > 입지 분석 사전 데이터 전처리 ####

ktPop_count_hjd <- ktPopulation[, c(3, 4)]
road_count_hjd <- road2 %>% group_by(행정동명) %>% summarise(세로_막다른도로수 = n())
cctv_count_hjd <- cctv_005 %>% group_by(행정동명, 자치구명) %>% summarise(cctv수 = n())

df_count_hjd <- full_join(ktPop_count_hjd, road_count_hjd, by = "행정동명")
df_count_hjd <- full_join(df_count_hjd, cctv_count_hjd, by = "행정동명")
df_count_hjd <- df_count_hjd %>% relocate(자치구명, .before = 행정동명) %>% arrange(desc(세로_막다른도로수))
df_count_hjd <- df_count_hjd %>% arrange(cctv수)

