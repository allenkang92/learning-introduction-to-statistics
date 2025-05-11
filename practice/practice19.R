# 1. 데이터 생성 (예시)
# 관측치 수 (n)
n <- 10
# 설명 변수 수 (k) - 절편 제외
k <- 2

# 응답 변수 Y 생성
set.seed(123) # 결과 재현을 위해 시드 설정
Y <- rnorm(n)

# 설명 변수 X_pred 생성
X_pred <- matrix(rnorm(n * k), nrow = n, ncol = k)

# 2. 설계 행렬 X 생성 (절편항 포함)
# 첫 번째 열에 모두 1을 추가합니다.
X <- cbind(1, X_pred)

# X 행렬 확인
print("설계 행렬 X:")
print(X)

# 3. Hat 행렬 H 계산
# H = X * (X'X)^-1 * X'
# solve() 함수는 역행렬을 계산합니다.
# t() 함수는 전치 행렬을 계산합니다.
# %*% 연산자는 행렬 곱셈입니다.
H <- X %*% solve(t(X) %*% X) %*% t(X)

# H 행렬 확인 (크기가 n x n 임)
print("Hat 행렬 H:")
print(round(H, 4)) # 소수점 4자리까지 표시

# 4. 행렬 J 계산
# J = (1/n) * 1 * 1' (여기서 1은 n x 1 벡터)
ones <- matrix(1, nrow = n, ncol = 1) # 모든 원소가 1인 n x 1 벡터 생성
J <- (1/n) * (ones %*% t(ones))

# J 행렬 확인 (모든 원소가 1/n 임)
print("행렬 J:")
print(round(J, 4)) # 소수점 4자리까지 표시

# 5. JH 계산
JH <- J %*% H

# JH 행렬 확인
print("행렬 JH:")
print(round(JH, 4)) # 소수점 4자리까지 표시

# 6. JH와 J 비교
# 행렬이 같은지 확인합니다. 부동 소수점 계산 오차 때문에 all.equal() 함수를 사용.
comparison_result <- all.equal(JH, J)

# 비교 결과 출력
if (isTRUE(comparison_result)) {
  print("결론: JH와 J는 같습니다 (부동 소수점 오차 범위 내).")
  print("이는 절편항이 있는 회귀모형에서 JH = J 임을 수치적으로 보여줍니다.")
} else {
  print("결론: JH와 J는 다릅니다.")
  print("차이점:")
  print(comparison_result)
}

# H * 1 = 1 인지 확인 (JH=J 증명의 핵심 단계 중 하나)
H_times_ones <- H %*% ones
print("H %*% 1 벡터:")
print(round(H_times_ones, 4))
print("1 벡터:")
print(ones)
print("H %*% 1 == 1 비교:")
print(all.equal(as.vector(H_times_ones), as.vector(ones))) # 벡터로 변환하여 비교