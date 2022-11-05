
# 2022년도 서울특별시 빅데이터 캠퍼스 공모전
# II. 통계적 가설검정

# 6. 통합 데이터 프레임 생성 및 상관분석 ####

df_count_sgg <- merge(crimeScene, cctv_count_sgg, by = "자치구명")
df_count_sgg <- merge(df_count_sgg, road_count_sgg, by = "자치구명")
df_count_sgg <- merge(df_count_sgg, ktPop_count_sgg, by = "자치구명")

'''
    [4가지 서브 데이터 프레임 병합]
    cctv_count_sgg | road_count_sgg | ktPop_count_sgg | crimeScene
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
    H1 : cctv 수가 증가할수록 강력범죄발생빈도가 높아진다. r = 0.35
    H2 : 세로_막다른도로(골목길) 수가 증가할수록 강력범죄발생빈도가 높아진다. r = -0.20
    H3 : 유동인구 수가 증가할수록 강력범죄발생빈도가 높아진다. (+) r = 0.84
'''



# 7. lm1. 다중회귀분석 ####

lm1 <- lm(강력범죄발생빈도 ~ 
                    cctv수 + 세로_막다른도로수 + 유동인구수,
                  data = df_count_sgg)

summary(lm1) 
plot(lm1)
shapiro.test(df_count_sgg$강력범죄발생빈도)
vif(lm1)
durbinWatsonTest(lm1)

lm1_f <- step(lm1, direction = "forward")
lm1_b <- step(lm1, direction = "backward")
lm1_s <- step(lm1, direction = "both")

lm.beta(lm1)

'''
    [lm1] 베이스라인 모델   
    lm1 회귀추정식은 통계적으로 유의함
    R^2 = 0.7193, Adjusted R^ = 0.6792

    [5가지 전제조건 성립 여부 검토]
        [정규성 조건 충족] p-value = 0.1938        
        [잔차의 등분산성, 잔차의 정규성 조건 충족] plot
        [다중공선성 X 조건 충족]
        cctv수    세로_막다른도로수    유동인구수
        1.146999        1.075666      1.193518 
        [잔차의 독립성 조건 충족]
        D-W Statistic p-value
        1.447063   0.146

    [변수선택법]
    전진선택법_후진제거법_단계별혼합법 결과
    모형 적합도 지표 AIC는 유동인구 수를 제외한 독립변수 제거하는 경우에 최솟값이 됨
    강력범죄발생빈도 ~ 유동인구수
    [변수 중요도]
    유동인구수 > cctv 수 > 세로_막다른도로 수
'''



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
table(df_count_sgg$생활권)
df_count_sgg$생활권 <- factor(df_count_sgg$생활권,
                           levels = c("동남이생활권",
                                      "도심권", "동남일생활권", "동북이생활권", "동북일생활권", 
                                      "서남삼생활권", "서남이생활권", "서남일생활권", "서북생활권"))
    # [범죄발생빈도가 중간값 3176에 가까운 "강동구"가 속한 "동남2생활권"을 더미변수의 레퍼런스로 설정]

lm2 <- lm(강력범죄발생빈도 ~ 
                    cctv수 + 세로_막다른도로수 + 유동인구수 + 생활권,
                  data = df_count_sgg)

summary(lm2)
plot(lm2)
shapiro.test(df_count_sgg$강력범죄발생빈도)
vif(lm2)
durbinWatsonTest(lm2)

lm2_f <- step(lm2, direction = "forward")
lm2_b <- step(lm2, direction = "backward")
lm2_s <- step(lm2, direction = "both")

anova(lm1, lm2)

'''
    [lm2] 더미변수(생활권) 추가한 뉴 모델   
    lm2 회귀추정식은 통계적으로 유의함
    R^2 = 0.8922, Adjusted R^ = 0.801
    [5가지 전제조건 성립 여부 검토]
        [정규성 조건 충족] p-value = 0.1938
        [잔차의 등분산성, 잔차의 정규성 조건 충족] plot
        [다중공선성 X 조건 충족]
        GVIF Df GVIF^(1/(2*Df))
        cctv수            2.011881  1        1.418408
        세로_막다른도로수 2.039191  1        1.428003
        유동인구수        4.020385  1        2.005090
        생활권            8.793409  8        1.145539
        [잔차의 독립성 조건 충족]
        D-W Statistic p-value
        2.22187   0.676

    [변수선택법]
    모형적합도지표 AIC값 가장 낮은 경우 
    강력범죄발생빈도 ~ 유동인구수 + 생활권
    [변수 중요도]
    유동인구수 > 세로_막다른도로 수 > cctv 수 > dv6(서남 2생활권)
    [lm1보다 lm2가 통계적으로 유의한 증가하는 결정계수 차이가 아님]
    anova 결과 lm2가 lm1보다 설명력 좋다고 말할 수 없음
'''



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

anova(lm1, lm3)

df_count_sgg$생활권 <- as.numeric(df_count_sgg$생활권)
df_count_sgg$interaction <- df_count_sgg$유동인구수 * df_count_sgg$생활권
lm4 <- lm(log(강력범죄발생빈도) ~ 
            scale(cctv수) + scale(세로_막다른도로수) + scale(유동인구수) + 생활권 + interaction, 
          data = df_count_sgg)

summary(lm4)
anova(lm3, lm4)

'''
    [lm3] IV, DV 간 선형성 조건 성립시키기 위한 log-log 변환 모델
    lm3 회귀추정식은 통계적으로 유의함
    R^2 = 0.82.78, Adjusted R^ = 68.22
    [5가지 전제조건 성립 여부 검토]
        [정규성 조건 충족]
        [잔차의 등분산성, 잔차의 정규성 조건 충족]
        [다중공선성 X 조건 충족]
        [잔차의 독립성 조건 충족]

    [변수 중요도]
    유동인구수 > > dv6(서남 2생활권) > 세로_막다른도로 수 > cctv 수
    [lm1보다 lm3가 통계적으로 유의한 증가하는 결정계수 차이] 
    anova 결과 p-value = 2.545e-06, lm3가 lm1보다 설명력 좋음

    [상호작용변수의 조절효과 없음]
    유동인구 수 x 생활권 곱한 2차 상호작용변수 interaction 생성
    범죄빈발시간 내 유동인구수가 증가할 때 -> 강력범죄발생빈도가 증가한다는 인과관계에
    생활권이 동남2생활권일 때(0) 대비 타 생활권일 때(1) 상기된 상호작용을 강화하지 못함
'''

