# 서울특별시 빅데이터 캠퍼스 공모전
# 유동인구 및 입지 데이터 분석 기반의 범죄 예방 솔루션

library(readr)
crime_r <- read.csv("대검찰청_범죄발생지_20181231.csv", header = TRUE, sep = ",", encoding = "utf-8", fileEncoding = "euc-kr")
crime_t <- read.csv("대검찰청_범죄발생시간_20171231.csv", header = TRUE, sep = ",", encoding = "utf-8", fileEncoding = "euc-kr")
crime_p <- read.csv("경찰청_범죄 발생 장소별 통계_20201231.csv", header = TRUE, sep = ",", encoding = "utf-8", fileEncoding = "euc-kr")

crime_t <- t(crime_t)
crime_t <- data.frame(crime_t)
colnames(crime_t) <- crime_t[1,]
crime_t <- crime_t[-1,]
crime_t$time <- rownames(crime_t)

library(ggplot2)
ggplot(data = crime_t, aes(x = time, y = `마약류관리에관한법률(마약)`)) + geom_col()

crime_g <- read.csv("대검찰청_범죄자 성별_20171231.csv", header = TRUE, sep = ",", encoding = "utf-8", fileEncoding = "euc-kr")
str(crime_g)
ggplot(data = crime_g, aes(x = ))
