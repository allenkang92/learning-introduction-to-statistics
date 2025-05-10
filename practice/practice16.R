# 데이터 입력
scores <- c(54, 57, 55, 23, 51, 64, 90, 51, 52, 43, 15, 10, 82, 74, 54, 78, 37, 73, 52, 48, 
           41, 33, 52, 30, 41, 51, 18, 39, 46, 28, 53, 44, 46, 56, 28, 58, 29, 58, 67, 35, 
           25, 38, 61, 53, 23, 73, 69, 47, 41, 45, 77, 56, 89, 28, 54, 99, 10, 43, 35, 24, 
           21, 23, 67, 14, 53)

# (1) 기초 통계량 계산
mean_score <- mean(scores)                # 평균
median_score <- median(scores)            # 중앙값
var_score <- var(scores)                  # 표본분산
sd_score <- sd(scores)                    # 표본표준편차
cv_score <- sd_score / mean_score * 100   # 변동계수 (%)

# 결과 출력
cat("\n----- (1) 데이터의 기초 통계량 -----\n")
cat("개수:", length(scores), "\n")
cat("평균:", round(mean_score, 2), "\n")
cat("중앙값:", median_score, "\n")
cat("표본분산:", round(var_score, 2), "\n")
cat("표본표준편차:", round(sd_score, 2), "\n")
cat("변동계수:", round(cv_score, 2), "%\n")

# (2) 그래픽을 통한 분포 시각화
# 줄기-잎 그림
cat("\n----- (2) 줄기-잎 그림 -----\n")
stem_result <- stem(scores)
print(stem_result)

# 히스토그램
par(mfrow=c(2, 2))  # 2x2 그래프 레이아웃 설정

hist(scores, 
     main="히스토그램", 
     xlab="점수", 
     ylab="빈도수",
     col="skyblue",
     breaks=10)

# 상자그림
boxplot(scores, 
        main="상자그림", 
        ylab="점수",
        col="lightgreen")

# 분포의 형태 확인을 위한 확률밀도함수 그래프
plot(density(scores), 
     main="점수의 확률밀도함수", 
     xlab="점수", 
     ylab="밀도")

# QQ-Plot (정규성 확인)
qqnorm(scores)
qqline(scores, col="red")

par(mfrow=c(1,1))  # 그래프 레이아웃 초기화

# 결과 해석
cat("\n----- 분포의 종합적 특징 -----\n")
cat("1. 중심 경향: 평균(", round(mean_score, 2), ")과 중앙값(", median_score, ")의 비교\n", sep="")
if (mean_score > median_score) {
  cat("   - 평균이 중앙값보다 큰 것으로 보아 오른쪽으로 꼬리가 긴 분포(양의 왜도)입니다.\n")
} else if (mean_score < median_score) {
  cat("   - 평균이 중앙값보다 작은 것으로 보아 왼쪽으로 꼬리가 긴 분포(음의 왜도)입니다.\n")
} else {
  cat("   - 평균과 중앙값이 같아 대칭적인 분포의 특징을 보입니다.\n")
}

cat("2. 분산: 표준편차는", round(sd_score, 2), "로, 데이터의 퍼짐 정도를 나타냅니다.\n")

# 사분위수 범위(IQR) 계산
q1 <- quantile(scores, 0.25)
q3 <- quantile(scores, 0.75)
iqr_val <- IQR(scores)
cat("3. 사분위수 범위(IQR):", iqr_val, "\n")

# 이상치 확인
lower_bound <- q1 - 1.5 * iqr_val
upper_bound <- q3 + 1.5 * iqr_val
outliers <- scores[scores < lower_bound | scores > upper_bound]
if (length(outliers) > 0) {
  cat("4. 이상치가", length(outliers), "개 발견되었습니다:", outliers, "\n")
} else {
  cat("4. 이상치가 발견되지 않았습니다.\n")
}

# 정규성 판단 (Shapiro-Wilk 검정)
shapiro_test <- shapiro.test(scores)
cat("5. 정규성 검정(Shapiro-Wilk) p-value:", round(shapiro_test$p.value, 4), "\n")
if (shapiro_test$p.value < 0.05) {
  cat("   - p-value가 0.05보다 작아 정규분포를 따른다고 볼 수 없습니다.\n")
} else {
  cat("   - p-value가 0.05보다 커서 정규분포를 따른다고 볼 수 있습니다.\n")
}