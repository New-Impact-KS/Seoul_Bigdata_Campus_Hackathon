
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
