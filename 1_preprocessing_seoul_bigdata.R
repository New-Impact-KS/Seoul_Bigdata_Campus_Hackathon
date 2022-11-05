
library(readr)
library(readxl)
library(dplyr)
library(rgdal)
library(foreign)
library(sp)
library(sf)
library(tidyverse)
library(mice)
library(stringr)
library(corrplot)
library(car)
library(lm.beta)
library(lmtest)
'''
    [Error]
    library(installr)
    check.for.updates.R()
    install.R()
'''
library(ggplot2)
RColorBrewer::display.brewer.all()
library(RColorBrewer)
library(cluster)
library(factoextra)
'''
   [error]
  library(hclust)
  library(dbscan)
'''




# 1. 대검찰청_경찰청 데이터 전처리 [crimeScene dataframe] ####

setwd("//98.44.9.103/seoul_bigdata/seoul_bigdata")
crimeArea_18 <- read.csv("대검찰청_범죄발생지_20181231.csv", header = TRUE, na = NA)
crimeArea_19 <- read.csv("대검찰청_범죄발생지_20191231.csv", header = TRUE, na = NA)

which(crimeArea_18$범죄분류 == "살인")
which(crimeArea_18$범죄분류 == "강도")
which(crimeArea_18$범죄분류 == "성폭력")
which(crimeArea_18$범죄분류 == "절도")
which(crimeArea_18$범죄분류 == "폭행")

crimeArea_18 <- crimeArea_18[c(7, 8, 11, 1, 10), 1:26]
crimeArea_19 <- crimeArea_19[c(7, 8, 11, 1, 10), 1:26]

crimeArea <- bind_rows(crimeArea_18, crimeArea_19)
for (i in 2:26) {
  crimeArea[11, i] <- sum(crimeArea[1:10, i])/2
}
crimeArea[11, 1] <- c("강력범죄")
crimeScene <- crimeArea[11,]
crimeScene <- t(crimeScene)
crimeScene <- data.frame(crimeScene)
crimeScene[, 2] <- crimeScene[,1]
crimeScene[, 1] <- rownames(crimeScene)
colnames(crimeScene) <- crimeScene[1,]
crimeScene <- crimeScene[-1,]
colnames(crimeScene) <- c("자치구명", "강력범죄발생빈도")
rownames(crimeScene) <- 1:25

crimeScene$자치구명 <- str_replace(crimeScene$자치구명, "서울_", "")
crimeScene$자치구명 <- paste(crimeScene$자치구명, "구")
crimeScene$자치구명 <- str_replace(crimeScene$자치구명, " ", "")
crimeScene$자치구명 <- str_replace(crimeScene$자치구명, "중구구", "중구")

rm(crimeArea_18)
rm(crimeArea_19)

'''
    [데이터 프레임 수정]
    서울시 행정구 외 시군구 | 5대 강력범죄 외 범죄분류 열 삭제
    2018년 지역별 범죄 빈도와 2019년 지역별 범죄 빈도의 평균값
    
'''



# 2. 서울시 내국인 KT 생활이동 데이터 로드 > problem of secondary memory ####

getwd()
setwd("//98.44.9.103/seoul_bigdata/seoul_bigdata/행정동별 kt_202206")
ktPoligon <- read.table("교통폴리곤코드_행정동코드(반출불가).txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")

rm(ktPoligon)

'''

[2022-06-01 ~ 2022-06-31 일별 생활이동 데이터 ]

setwd("//98.44.9.103/seoul_bigdata/seoul_bigdata/행정동별 kt_202206/202206")
ktMove_220601 <- read.table("202206_01_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220602 <- read.table("202206_02_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220603 <- read.table("202206_03_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220604 <- read.table("202206_04_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220605 <- read.table("202206_05_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220606 <- read.table("202206_06_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220607 <- read.table("202206_07_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220608 <- read.table("202206_08_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220609 <- read.table("202206_09_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220610 <- read.table("202206_10_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220611 <- read.table("202206_11_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220612 <- read.table("202206_12_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220613 <- read.table("202206_13_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220614 <- read.table("202206_14_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220615 <- read.table("202206_15_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220616 <- read.table("202206_16_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220617 <- read.table("202206_17_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220618 <- read.table("202206_18_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220619 <- read.table("202206_19_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220620 <- read.table("202206_20_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220621 <- read.table("202206_21_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220622 <- read.table("202206_22_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220623 <- read.table("202206_23_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220624 <- read.table("202206_24_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220625 <- read.table("202206_25_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220626 <- read.table("202206_26_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220627 <- read.table("202206_27_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220628 <- read.table("202206_28_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220629 <- read.table("202206_29_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")
ktMove_220630 <- read.table("202206_30_OUTPUT.txt", sep = "|", header = TRUE, fileEncoding = "UTF-8")

ktPop_강남구 <- read.table("wlk_강남구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_강동구 <- read.table("wlk_강동구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_강북구 <- read.table("wlk_강북구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_강서구 <- read.table("wlk_강서구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_관악구 <- read.table("wlk_관악구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_광진구 <- read.table("wlk_광진구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_구로구 <- read.table("wlk_구로구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_금천구 <- read.table("wlk_금천구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")

ktPop_노원구 <- read.table("wlk_노원구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_도봉구 <- read.table("wlk_도봉구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_동대문구 <- read.table("wlk_동대문구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_동작구 <- read.table("wlk_동작구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")

ktPop_마포구 <- read.table("wlk_마포구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_서대문구 <- read.table("wlk_서대문구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_서초구 <- read.table("wlk_서초구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_성동구 <- read.table("wlk_성동구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_성북구 <- read.table("wlk_성북구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_송파구 <- read.table("wlk_송파구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")

ktPop_양천구 <- read.table("wlk_양천구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_영등포구 <- read.table("wlk_영등포구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_용산구 <- read.table("wlk_용산구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_은평구 <- read.table("wlk_은평구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")

ktPop_종로구 <- read.table("wlk_종로구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_중구 <- read.table("wlk_중구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
ktPop_중랑구 <- read.table("wlk_중랑구_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")

'''





# 3. 서울시 행정동 단위 월별 KT 유동인구 데이터 전처리 [ktPopulation dataframe] ####

setwd("//98.44.9.103/seoul_bigdata/seoul_bigdata/2018")
ktPopulation <-  read.table("TBDM_FLPOP_MONTH_201801.txt", sep = ",", header = TRUE, fileEncoding = "UTF-8")
table(ktPopulation$HOUR)
ktPopulation <- ktPopulation %>% filter(HOUR %in% c(20, 21, 22, 23, 0, 1, 2, 3, 4))
ktPopulation <- ktPopulation %>% select(c(2, 3, 4, 7, 8))
ktPopulation <- ktPopulation %>% group_by(SIGUNGU_NM, ADMI_CD, ADMI_NM) %>% summarise(sumPOP_CNT = sum(POP_CNT))
table(ktPopulation$SIGUNGU_NM)
table(ktPopulation$ADMI_NM)
colnames(ktPopulation) <- c("자치구명", "행정동코드", "행정동명", "유동인구합계")


'''
    [데이터 컬럼 삭제 목록]
    기준연월 | 성별 | 연령대 | 20시 ~ 4시 외 시간대 제거
    [데이터 프레임 수정]
    행정동별 유동인구 수 합계
'''

ktPop_count_sgg <- ktPopulation %>% group_by(자치구명) %>% summarise(유동인구수 = sum(유동인구합계))




# 4. 서울시 10m 도로구간 공간 데이터 전처리 2016 [road dataframe] ####

#setwd("//98.44.9.103/seoul_bigdata/seoul_bigdata/10m 단위 도로/2016년상")
road_shp = st_read("2016년road/TL_SPRD_MANAGE.shp")
road1 <- road_shp %>% select(
  BSI_INT, SIG_CD, RBP_CN, REP_CN,
  WDR_RD_CD, RN, RN_CD, ROAD_BT, ROAD_CD, ROAD_LT, 
  geometry
)
road1 <- data.frame(road1)
str(road1)
colnames(road1) <- c("기초간격", "시군구코드", "기점", "종점",
                     "광역도로구분코드", "도로명", "도로명코드", 
                     "도로폭", "도로코드", "도로길이", "XY좌표")
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


setwd("//98.44.9.103/seoul_bigdata/seoul_bigdata")
sigunguCode <- read.csv("시군구코드.csv", header = TRUE, sep = ",")
str(sigunguCode)
sigunguCode$시군구코드 <- as.character(sigunguCode$시군구코드)
colnames(sigunguCode) <- c("행정동코드", "코드", "시군구코드")

SGG_code <- read.csv("SGG_data.csv")
SGG_code$sgg <- paste(SGG_code$C_SIDO_CD,SGG_code$C_SGG_CD,"0")
SGG_code$sgg <- str_replace(SGG_code$sgg,' ', '')
SGG_code$sgg <- str_replace(SGG_code$sgg,' ', '')
SGG_code <- SGG_code[, -c(1:3)]
colnames(SGG_code) <- c("자치구명", "시군구코드")
str(SGG_code$시군구코드)
str(road1$시군구코드)

road2 <- full_join(road1, SGG_code, by = "시군구코드")
road2 <- road2 %>% relocate(자치구명, .after = 시군구코드)
road2[is.na(road2$자치구명), ]

road2_sgg <- road2 %>% group_by(자치구명, 시군구코드) %>% summarise(count = n())
# road2 <- road2 %>% filter(시군구코드 != c("28245", "41195", "41199", "41110", "41181", "41450"))
# [서울시 아닌 지역 제거] : 에러 코드

which(road2$자치구명 %in% c("강북구", "금천구", "광진구"))
road2 <- road2[-c(39796, 39797, 39798), ]

road2$자치구명 <- ifelse(road2$시군구코드 == "11215", "광진구", road2$자치구명)
road2$자치구명 <- ifelse(road2$시군구코드 == "11305", "강북구", road2$자치구명)
road2$자치구명 <- ifelse(road2$시군구코드 == "11545", "금천구", road2$자치구명)
road2 <- road2 %>% filter(!is.na(road2$자치구명))
road2_sgg <- road2 %>% group_by(자치구명, 시군구코드) %>% summarise(count = n())
table(road2$자치구명)


'''
    [데이터 파일 변환 후 필요한 데이터 추출]
    shape.file -> csv.file
    [시군구코드] 
    [도로명코드]
    
    [광역도로 구분코드]
    1. 행정자치부 도로
    2. 광역도로 : 2개 이상의 시도를 통과하는 노선, 대도시권 중장거리 통행 처리 도로
    3. 시군구도로

    [통행이 협소한 골목길 위치 파악 목적]
    보행 및 자동차 통행 도로로서 건축법상 막다른 도로
    1. 폭 2m 이하, 길이 10m 미만
    2. 폭 3m 이하, 길이 10m 이상 ~ 35m 미만
    3. 폭 6m(도시지역) / 폭 4m(읍면지역), 길이 35m 이상
    NA. 막다른 도로 아닌 
    
    [건축법상 도로 및 이면/생활도로]
    4m, 9m -> 우발적 강력범죄 발생예상지역의 도로조건으로 
    폭 4m 이하 채택
    
    기점 = 건축법상 도로의 분기점이며,
    종점 = 건축물의 경계선 아닌 건축허가대상 부지의 경계선
'''

''' 
    [에러 코드]
    road2 <- full_join(road1, sigunguCode, by = "시군구코드")
    road2 <- road2 %>% relocate(코드, .after = 시군구코드)
'''

road_count_sgg <- road2 %>% group_by(자치구명) %>% summarise(세로_막다른도로수 = n())
rm(road1)
rm(road2_sgg)

# [도로의 기점 변수에서 행정동명 추출]

road2$행정동명 <- substr(road2$기점, 1, 4)
road2 <- road2 %>% relocate(행정동명, .after = 자치구명)
road2[which(str_detect(road2$행정동명, "동") == FALSE), 3]
road2[which(str_detect(road2$행정동명, "동") == FALSE), 3] <- NA
# [981개 행은 행정동 단위의 기점이 아닌바 결측치 처리]
table(is.na(road2$행정동명))
road2$행정동명 <- substr(road2$기점, 1, 3)
road2$행정동명 <- paste(road2$행정동명, "동")
road2$행정동명 <- str_replace(road2$행정동명, " ", "")
road2$행정동명 <- str_replace(road2$행정동명, "동동", "동")
road2$행정동명 <- str_replace(road2$행정동명, "동 동", "동")
road2$행정동명 <- ifelse(road2$행정동명 == "NA동", NA, road2$행정동명)
road2$행정동명 <- ifelse(road2$행정동명 == "4.1동", NA, road2$행정동명)
road2[which(str_detect(road2$행정동명, "묵동") == TRUE), 3] <- "묵동"
road2[which(str_detect(road2$행정동명, "필동") == TRUE), 3] <- "필동"
print(road2 %>% group_by(행정동명) %>% summarise(count = n()), n = 356)



# 5. 서울시 CCTV 현황 공간데이터 2019 [cctv_n dataframe] ####

cctv_shp = st_read("2019/TL_CCTV_ST_2019.shp")
# [sbh -> csv]
cctv_001 <- cctv_shp %>% select(OBJECTID, CCTV_SE,  ADSTRD_ID, INLPS_SE, PURPS_SE, LO, LA, RDNMADR_DC, LMN_ADR_DC, LC_DC)
# [#필요한 데이터만 추출]
# 일련번호 | CCTV_구분 | 행정동ID | 설치장소 구분 | 목적 구분 | 경도 | 위도 | 도로명주소 | 지번주소 | 위치_설명 | 
# (1-objectid, 9-lo, 10-la, 11-rdnmard_dc, 12-lmn_adr_dc, 16-lc_dc, 19-emd_id, 21-adstrd_id)

cctv_002 <- cctv_001[!is.na(cctv_001$LMN_ADR_DC),]
# [결측치 & 시군구,행정동명 없는 데이터 제거 후 시군구, 행정동 명 결합]
# [NA값 제거]
# [경기도 행 제거]
cctv_002 <- cctv_002[-grep("경기도", cctv_002$LMN_ADR_DC),]
# [코드로 되어있는 행 데이터 제거]
cctv_002 <- cctv_002[-grep("C", cctv_002$LMN_ADR_DC),]
# [50048행 데이터 제거]
# length(grep("C", cctv_002$LMN_ADR_DC)) 
# [2308개]

table(cctv_002$CCTV_SE)
table(cctv_002$INLPS_SE)
table(cctv_002$PURPS_SE)
cctv_sbn = st_drivers("2019/CP_BD_2016_16.sbn")
rm(cctv_sbn)
# [GRP CODE와 비교 필요하나 코드북에 상세설명 미기재]
cctv_002 <- cctv_002[, -c(2, 4, 5)]
# [CCTV_구분 | 설치장소 구분 | 목적 구분 열 제거]
cctv_002 <- data.frame(cctv_002)

colnames(cctv_002) <- c("일련번호", "행정동코드", "경도", "위도", "도로명주소", "지번주소", "위치_설명", "XY좌표")
str(cctv_002$행정동코드)
sigunguCode$행정동코드 <- as.character(sigunguCode$행정동코드)
sigunguCode <- sigunguCode %>% rename(행정동명 = 코드)
cctv_003 <- full_join(cctv_002, sigunguCode, by = "행정동코드")

cctv_003 <- cctv_003 %>% relocate(행정동명, .after = 행정동코드)
cctv_003 <- cctv_003 %>% relocate(시군구코드, .before = 행정동코드)


data.na <- cctv_003 %>% select(시군구코드, 행정동코드, 행정동명)
md.pattern(data.na)


SGG_code <- read.csv("SGG_data.csv")
SGG_code$sgg <- paste(SGG_code$C_SIDO_CD, SGG_code$C_SGG_CD, "0")
SGG_code$sgg <- str_replace(SGG_code$sgg, ' ', '')
SGG_code$sgg <- str_replace(SGG_code$sgg, ' ', '')
SGG_code <- SGG_code[, -c(1:2)]

# [paste : 문자끼리 붙여주는 함수]
# [-열 끼리 문자를 붙임]
# [-띄어쓰기가 생기는 문제가 발생함]
# [str_replace : 처음으로 매칭이 되는 문자의 값만 치환과 삭제를 해주는 함수]
# [-space(' ')를 nonspace('')로 바꿔줌]

names(SGG_code)
colnames(SGG_code) <- c("시도", "자치구명",  "시군구코드")
SGG_code$시군구코드 <- as.character(SGG_code$시군구코드)
cctv_003$시군구코드 <- as.character(cctv_003$시군구코드)
cctv_003 <- full_join(cctv_003, SGG_code, by = "시군구코드")
table(cctv_003$시도)
table(cctv_003$자치구명)
cctv_003 <- cctv_003 %>% relocate(자치구명, .after = "시군구코드")
cctv_003 <- cctv_003 %>% filter(!is.na(일련번호))

cctv_003$자치구명 <- ifelse(cctv_003$시군구코드 == "11215", "광진구", cctv_003$자치구명)
cctv_003$자치구명 <- ifelse(cctv_003$시군구코드 == "11305", "강북구", cctv_003$자치구명)
cctv_003$자치구명 <- ifelse(cctv_003$시군구코드 == "11545", "금천구", cctv_003$자치구명)
cctv_sgg <- cctv_003 %>% group_by(자치구명) %>% summarise(count = n())

cctv_003$시군구행정동명 <- substr(cctv_003$지번주소, 1, 14)
cctv_003$시도 <- substr(cctv_003$시군구행정동명, 1, 5)
table(cctv_003[,12] == "인천광역시")
# [인천광역시 138개 행 데이터 제거]
cctv_003 <- cctv_003[-which(cctv_003[, 12] == "인천광역시"), ]

cctv_003 %>% group_by(시도) %>% summarise(count = n())
cctv_003$시군구행정동명 <- ifelse(cctv_003$시도 != "서울특별시", NA, cctv_003$시군구행정동명)
table(is.na(cctv_003$시군구행정동명))
# [6251개 행 데이터는 지번주소가 시군구 행정동으로 되어있지 않은 데이터]
cctv_003 <- cctv_003[, -12]

cctv_sgg_hjd <- cctv_003 %>% group_by(시군구행정동명) %>% summarise(count = n())

table(is.na(cctv_003$자치구명))
# [10546개]
table(is.na(cctv_003$행정동명))
# [10546개]

cctv_004 <- cctv_003 %>% filter(is.na(cctv_003$자치구명) & is.na(cctv_003$행정동명))
cctv_003 <- cctv_003 %>% filter(!is.na(cctv_003$자치구명) & !is.na(cctv_003$행정동명))
# [자치구명 및 행정동명이 NA 아닌 DF (cctv_003), NA DF 분리 (cctv_004)]


# [지번주소 추출하여 자치구명(26)으로 결측값 대체] ####
print(cctv_004 %>% group_by(시군구행정동명) %>% summarise(count = n()), n = 907)

cctv_004$자치구명 <- substr(cctv_004$시군구행정동명, 7, 9)
print(cctv_004 %>% group_by(자치구명) %>% summarise(count = n()), n = 27)

cctv_004$자치구명 <- str_replace(cctv_004$자치구명, "영등포", "영등포구")
cctv_004$자치구명 <- str_replace(cctv_004$자치구명, "동대문", "동대문구")
cctv_004$자치구명 <- str_replace(cctv_004$자치구명, "서대문", "서대문구")
cctv_004$자치구명 <- str_replace(cctv_004$자치구명, "중구 ", "중구")
cctv_004[which((cctv_004$자치구명) == "중구?"), ]
cctv_004[c(322, 676), 3] <- "중구"
print(cctv_004 %>% group_by(자치구명) %>% summarise(count = n()), n = 27)


# [지번주소 추출하여 행정구명(542)으로 결측값 대체] ####
table(is.na(cctv_004$행정동코드))
# [sigunguCode와 cctv_004 병합(join)하여 행정동명으로 결측값 대체하기에는 NA's 9315개]

cctv_004$행정동명 <- substr(cctv_004$시군구행정동명, 11, 15)
cctv_004$행정동명 <- str_replace(cctv_004$행정동명, " ", "")

print(cctv_004 %>% group_by(행정동명) %>% summarise(count = n()), n = 856)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?당산동", "당산동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?도림동", "도림동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?신길동", "신길동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?양평동", "양평동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?여의도", "여의동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?영등포", "영등포동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?홍은동", "홍은동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?홍제동", "홍제동", cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$자치구명 == "중구", 
                        substr(cctv_004$시군구행정동명, 10, 14), cctv_004$행정동명)
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "?대림동", "대림동", cctv_004$행정동명)

cctv_004[which(str_detect(cctv_004$행정동명, "동") == FALSE), 5] <- NA
# [행정동 단위가 아닌 cctv 설치구역 1087행의 데이터 결측치 처리]
table(is.na(cctv_004$행정동명))
print(cctv_004 %>% group_by(행정동명) %>% summarise(count = n()), n = 670)
cctv_004$행정동명 <- substr(cctv_004$행정동명, 1, 3)
cctv_004$행정동명 <- paste(cctv_004$행정동명, "동")
cctv_004$행정동명 <- str_replace(cctv_004$행정동명, " ", "")
cctv_004$행정동명 <- str_replace(cctv_004$행정동명, "동동", "동")
cctv_004$행정동명 <- str_replace(cctv_004$행정동명, "동 동", "동")
cctv_004$행정동명 <- str_replace(cctv_004$행정동명, "NA동", "NA")
cctv_004$행정동명 <- ifelse(cctv_004$행정동명 == "NA", NA, cctv_004$행정동명)

print(cctv_004 %>% group_by(행정동명) %>% summarise(count = n()), n = 542)


# [cctv_003과 cctv_004 병합] ####
cctv_005 <- bind_rows(cctv_003, cctv_004)
table(is.na(cctv_005$행정동명)) 
# [NA's 1087개]
table(is.na(cctv_005$자치구명))
# [NA's 241개]
cctv_005 <- cctv_005 %>% relocate(시군구코드, .before = 행정동코드)
cctv_005 <- cctv_005 %>% relocate(자치구명, .after = 시군구코드)

cctv_count_sgg <- cctv_005 %>% group_by(자치구명) %>% summarise(cctv수 = n())

