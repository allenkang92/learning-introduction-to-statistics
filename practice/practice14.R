# 데이터 입력
A <- c(75, 79, 96, 72, 95, 72, 89, 63, 85, 85)
B <- c(76, 86, 85, 87, 98, 78, 83, 85, 76, 90)
C <- c(65, 63, 73, 61, 91, 74, 63, 73, 64, 78)

# 데이터 변환 및 재구성
scores <- c(A, B, C)
groups <- factor(rep(c("A", "B", "C"), each = 10))
data <- data.frame(score = scores, group = groups)

# 기술통계량 출력
cat("각 그룹별 기술통계량:\n")
stats <- aggregate(score ~ group, data = data, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), n = length(x))
})
print(stats)

# 박스플롯으로 시각화
boxplot(score ~ group, data = data, 
        main = "교재별 영어 점수 비교",
        xlab = "교재 그룹", 
        ylab = "점수", 
        col = c("lightblue", "lightgreen", "lightpink"))

# (1) 귀무가설 설정
cat("\n(1) 귀무가설: 세 그룹의 영어 점수 평균은 동일하다.)\n")

# (2) 대립가설 설정
cat("(2) 대립가설: 적어도 한 그룹의 영어 점수 평균은 다른 그룹과 다르다.\n")

# (3) 가설검정 : 분산분석(ANOVA) 수행
anova_result <- aov(score ~ group, data = data)
summary_result <- summary(anova_result)
print(summary_result)

# 유의확률(p-value) 확인
p_value <- summary_result[[1]]["group", "Pr(>F)"]
cat("\n(3) 가설검정 결과:\n")
cat("F-통계량:", round(summary_result[[1]]["group", "F value"], 4), "\n")
cat("유의확률(p-value):", round(p_value, 4), "\n")

# (4) 유의수준 5%에서 가설검정의 결론
alpha <- 0.05
cat("\n(4) 유의수준 5%에서 가설검정의 결론:\n")

if(p_value < alpha) {
  cat("p-value(", round(p_value, 4), ")가 유의수준(0.05)보다 작으므로 귀무가설을 기각합니다.\n")
  cat("따라서, 적어도 한 그룹의 영어 점수 평균은 다른 그룹과 유의하게 다르다고 할 수 있습니다.\n")
  
  # 사후 검정(Tukey의 HSD 검정) 수행
  cat("\n사후 검정(Tukey HSD) 결과:\n")
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else {
  cat("p-value(", round(p_value, 4), ")가 유의수준(0.05)보다 크므로 귀무가설을 기각할 수 없습니다.\n")
  cat("따라서, 세 그룹의 영어 점수 평균이 다르다고 할 충분한 증거가 없습니다.\n")
}

# 그룹별 평균 점수 출력
cat("\n각 그룹별 평균 점수:\n")
means <- tapply(data$score, data$group, mean)
for(i in 1:length(means)) {
  cat(names(means)[i], "교재 그룹:", round(means[i], 2), "\n")
}