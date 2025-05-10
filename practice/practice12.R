# 의자 개수 데이터 입력
chairs <- c(23, 19, 9, 18, 22, 23, 18, 25, 8, 14, 24, 15, 16, 22, 19, 27, 14, 19, 21, 20)

# (1) 히스토그램 그리기
hist(chairs, 
     main = "프랜차이즈 커피숍 매장별 의자 개수 분포", 
     xlab = "의자 개수", 
     ylab = "빈도수",
     col = "lightblue",
     breaks = seq(5, 30, by = 5))  # 5부터 30까지 5 단위로 구간 설정

# (2) 상자그림 그리기
boxplot(chairs, 
        main = "프랜차이즈 커피숍 매장별 의자 개수 분포",
        ylab = "의자 개수",
        col = "lightblue")

# (3) 다섯수치요약 산출
five_num <- fivenum(chairs)
names(five_num) <- c("최솟값", "1사분위수", "중앙값", "3사분위수", "최댓값")
print("다섯수치요약:")
print(five_num)

# 다른 방법으로 다섯수치요약 확인
summary(chairs)

# (4) 사분위수 범위(IQR) 산출
iqr_value <- IQR(chairs)
print(paste("사분위수 범위(IQR):", iqr_value))
