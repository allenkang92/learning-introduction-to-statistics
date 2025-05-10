# 데이터 입력
x <- 59  # 밀떡볶이를 선호 인원 수
n <- 100  

# (1) 귀무가설 설정
# 귀무가설, 영가설(H0): 밀떡볶이 선호 비율은 50%이다. (p = 0.5)
# 대립가설(H1): 밀떡볶이 선호 비율은 50%와 다르다. (p != 0.5)
cat("(1) 귀무가설: 밀떡볶이를 선호하는 비율은 50%이다. (p = 0.5)\n")

# (2) 대립가설 설정
cat("(2) 대립가설: 밀떡볶이를 선호하는 비율은 50%와 다르다. (p != 0.5)\n")

# (3)가설검정 수행 및 유의확률
# 일표본 비율검정(One-sample proportion test) 수행
result <- prop.test(x, n, p = 0.5, alternative = "two.sided")
print(result)

# 유의확률(p-value) 확인
p_value <- result$p.value
cat("\n(3) 가설검정 결과:\n")
cat("검정통계량(X-squared):", round(result$statistic, 4), "\n")
cat("유의확률(p-value):", round(p_value, 4), "\n")

# (4) 유의수준 5%에서 가설검정의 결론
alpha <- 0.05
cat("\n(4) 유의수준 5%에서 가설검정의 결론:\n")

if(p_value < alpha) {
  cat("p-value(", round(p_value, 4), ")가 유의수준(0.05)보다 작으므로 귀무가설을 기각합니다.\n")
  cat("따라서, 밀떡볶이를 선호하는 비율은 50%와 유의하게 다르다고 할 수 있습니다.\n")
} else {
  cat("p-value(", round(p_value, 4), ")가 유의수준(0.05)보다 크므로 귀무가설을 기각할 수 없습니다.\n")
  cat("따라서, 밀떡볶이를 선호하는 비율이 50%와 다르다고 할 충분한 증거가 없습니다.\n")
}

# 신뢰구간 출력
conf_int <- result$conf.int
cat("\n95% 신뢰구간:", round(conf_int[1], 4), "~", round(conf_int[2], 4), "\n")
