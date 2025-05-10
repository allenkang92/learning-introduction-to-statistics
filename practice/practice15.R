# 데이터 입력
student_id <- 1:15
mother_height <- c(156, 154, 161, 167, 157, 163, 172, 152, 162, 168, 166, 159, 155, 163, 155)
student_height <- c(170, 167, 170, 174, 159, 166, 177, 162, 165, 163, 175, 160, 159, 166, 159)

# 데이터프레임 생성
height_data <- data.frame(student_id, student_height, mother_height)

# (1) 학생의 엄마 키를 가로축, 학생 본인의 키를 세로축으로 하는 산점도 
# 제목 학번 : 202535368005
plot(mother_height, student_height, 
     main = "202535368005", 
     xlab = "엄마의 키(cm)", 
     ylab = "학생 본인의 키(cm)",
     pch = 19, 
     col = "lightblue",
     xlim = c(150, 175),
     ylim = c(155, 180))

# 데이터 포인트에 학생 번호 표시
text(mother_height, student_height, labels = student_id, pos = 4, cex = 0.8)

# (2) 엄마의 키와 본인 키의 상관계수 계산
correlation <- cor(mother_height, student_height)
cat("\n(2) 엄마의 키와 본인 키의 상관계수:", round(correlation, 4), "\n")

# 상관관계 검정
cor_test <- cor.test(mother_height, student_height)
cat("   상관관계 검정 p-value:", round(cor_test$p.value, 4), "\n")

# (3) 회귀분석 - 엄마의 키를 독립변수, 본인의 키를 종속변수로
regression_model <- lm(student_height ~ mother_height)
summary_result <- summary(regression_model)

# 회귀직선의 기울기와 절편
intercept <- regression_model$coefficients[1]
slope <- regression_model$coefficients[2]

cat("\n(3) 회귀직선 분석 결과:\n")
cat("   회귀식: student_height =", round(intercept, 4), "+", round(slope, 4), "× mother_height\n")
cat("   절편:", round(intercept, 4), "\n")
cat("   기울기:", round(slope, 4), "\n")
cat("   결정계수(R²):", round(summary_result$r.squared, 4), "\n")
cat("   회귀모형의 p-value:", round(summary_result$coefficients[2,4], 4), "\n")

# 산점도에 회귀직선 포함
abline(regression_model, col = "red", lwd = 2)
legend("topleft", legend = c("데이터 포인트", "회귀직선"), 
       col = c("blue", "red"), 
       pch = c(19, NA), 
       lty = c(NA, 1),
       lwd = c(NA, 2))