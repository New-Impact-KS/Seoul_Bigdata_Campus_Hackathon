
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
    # [k = 3, k = 4로 각 군집분석 수행]
kmclust$centers
kmclust$size

distance <- dist(df_count_sgg[, -c(1, 6)], method= "euclidean")
sil <- silhouette(kmclust$cluster, distance)
plot(sil, col = "blue")

fviz_cluster(kmclust, data = df_count_sgg[, -c(1, 6, 7)], palette = "Set2")

df_sgg_kmclust <- df_count_sgg
df_sgg_kmclust$cluster <- kmclust$cluster
df_sgg_kmclust <- df_sgg_kmclust %>% arrange(cluster)

'''
   [k-means 군집분석 변수]
   cctv수 | KT 야간 유동인구수 | 세로_막다른 도로 수 | 강력범죄발생빈도

   [k-means 군집분석의 k값 튜닝]
   Elbow plot(WSS)에 따르면 k = 3이 최적이나
   cluster별 응집도가 시각적으로 높다고 판단되는 K = 4 채택
'''

sil_avg <- function(k, data) {
  kmclust_n <- kmeans(df_count_sgg[, -c(1, 6)], centers = k)
  silhouette <- silhouette(kmclust_n$cluster, dist(df_count_sgg[, -c(1, 6)]))
  silAvg <- mean(silhouette[, 3])
  return(silAvg)
}
kclusters <-  2:5 

result_n <- data.frame( k = kclusters, 
                        silAvg = rep(NA, length(kClusters)) ) 

for(i in 1:length(kclusters)) {
    result_n$silAvg[i] <- sil_avg(kclusters[i], df_count_sgg[, -c(1, 6)])
}
plot(result_n$k, result_n$silAvg,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters' K", ylab = "Average of Silhouettes")

'''
    [실루엣 분석]
    전체 실루엣 계수 평균을 구하는 사용자 정의 함수 생성
    테스트할 군집수 2 ~ 5를 kClusters로 설정
    군집수에 따른 전체 실루엣 계수 결과 저장 변수 생성
    전체 실루엣 계수 평균 결과 산출
    실루엣 분석 결과 시각화
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

'''
    [계층적 군집분석 메소드]
    거리 유사도 = 유클리디안 거리(L2 distance)
    알고리즘 = 군집 내 오차제곱합(SSE, 정보의 손실 최소화)에 근거를 두고 군집들을 병합시키는 와드연결법
'''



# 9. 시군구 아닌 행정동별 통합 데이터 프레임 생성 > 입지 분석 사전 데이터 전처리 ####

ktPop_count_hjd <- ktPopulation[, c(3, 4)]
road_count_hjd <- road2 %>% group_by(행정동명) %>% summarise(세로_막다른도로수 = n())
cctv_count_hjd <- cctv_005 %>% group_by(행정동명, 자치구명) %>% summarise(cctv수 = n())

df_count_hjd <- full_join(ktPop_count_hjd, road_count_hjd, by = "행정동명")
df_count_hjd <- full_join(df_count_hjd, cctv_count_hjd, by = "행정동명")
df_count_hjd <- df_count_hjd %>% relocate(자치구명, .before = 행정동명) %>% arrange(desc(세로_막다른도로수))
df_count_hjd <- df_count_hjd %>% arrange(cctv수)

