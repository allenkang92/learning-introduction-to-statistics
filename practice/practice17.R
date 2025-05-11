# 데이터 입력
age <- c(3, 1, 5, 8, 1, 4, 2, 6, 9, 3, 5, 7, 2, 6)
cost <- c(39, 24, 115, 105, 50, 86, 67, 90, 140, 112, 70, 186, 43, 126)

# 데이터프레임 생성
machine_data <- data.frame(age, cost)

# 1) 산점도와 회귀직선
plot(age, cost, 
     main = "기계 사용연도와 정비비용의 관계",
     xlab = "사용연도(년)",
     ylab = "정비비용(천원)",
     pch = 19,
     col = "blue")

# 회귀분석 수행
regression_model <- lm(cost ~ age, data = machine_data)
abline(regression_model, col = "red", lwd = 2)

# 회귀식 표시
model_summary <- summary(regression_model)
intercept <- coef(regression_model)[1]
slope <- coef(regression_model)[2]
equation <- paste("Y =", round(intercept, 2), "+", round(slope, 2), "X")
legend("topleft", legend = equation, bty = "n")

# 2) 결정계수와 상관계수 계산
r_squared <- model_summary$r.squared
correlation <- cor(age, cost)
cat("\n2) 결정계수와 상관계수:\n")
cat("   결정계수(R²):", round(r_squared, 4), "\n")
cat("   상관계수(r):", round(correlation, 4), "\n")

# 3) 분산분석표 작성 및 회귀직선의 유의성 검정
anova_table <- anova(regression_model)
cat("\n3) 분산분석표:\n")
print(anova_table)

# 회귀직선의 유의성 검정 (F-검정)
f_value <- anova_table$`F value`[1]
p_value <- anova_table$`Pr(>F)`[1]
cat("   F-통계량:", round(f_value, 4), "\n")
cat("   p-value:", round(p_value, 4), "\n")

if (p_value < 0.05) {
  cat("   결론: p-value가 0.05보다 작으므로, 회귀직선은 통계적으로 유의합니다.\n")
} else {
  cat("   결론: p-value가 0.05보다 크므로, 회귀직선은 통계적으로 유의하지 않습니다.\n")
}

# 4) 사용연도가 4년인 기계의 평균정비비용 추정
new_data <- data.frame(age = 4)
predicted_cost <- predict(regression_model, newdata = new_data)
cat("\n4) 사용연도가 4년인 기계의 추정 평균정비비용:", round(predicted_cost, 2), "천원\n")

# 5) 잔차 계산 및 합이 0임을 확인
residuals <- residuals(regression_model)
cat("\n5) 잔차(오차) 계산:\n")
for(i in 1:length(age)) {
  cat("   데이터", i, ": e_i =", round(residuals[i], 2), "\n")
}
residual_sum <- sum(residuals)
cat("   잔차의 합:", round(residual_sum, 10), "\n")
cat("   (수치적 오차로 인해 정확히 0이 아닐 수 있으나, 매우 작은 값임)\n")

# 6) 잔차들의 x에 대한 가중합 계산
weighted_sum_x <- sum(residuals * age)
cat("\n6) 잔차들의 x에 대한 가중합:", round(weighted_sum_x, 10), "\n")

# 7) 잔차들의 y에 대한 가중합 계산
weighted_sum_y <- sum(residuals * cost)
cat("\n7) 잔차들의 y에 대한 가중합:", round(weighted_sum_y, 10), "\n")

# 추가: 회귀분석의 가정 검토
par(mfrow=c(2,2))
plot(regression_model)