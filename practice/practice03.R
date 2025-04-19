# 정수 시퀀스 1, 2, 3, 4를 생성하여 i에 할당
# 결과: i는 numeric 타입의 벡터 c(1, 2, 3, 4)
i <- 1:4

# 1부터 4까지의 정수 시퀀스를 생성하고 factor 타입으로 변환하여 j에 할당
# factor는 범주형 데이터를 표현하는 R의 특수한 데이터 타입
# 결과: j는 레벨(수준)이 "1", "2", "3", "4"인 factor
j <- as.factor(1:4)

# i의 각 원소에 1을 더함
# 숫자형 벡터에 숫자를 더하는 것은 가능하므로 결과는 c(2, 3, 4, 5)가 됨
i + 1

# j(factor 타입)에 1을 더하려고 시도
# factor 타입에는 산술 연산을 적용할 수 없으므로 경고 메시지가 발생
# 실제 계산은 이루어지지 않음
j + 1

## Warning in Ops.factor(j, 1) : '+' not meaningful for factors

# 1부터 4까지의 정수를 문자열로 변환하여 k에 할당
# 결과: k는 character 타입의 벡터 c("1", "2", "3", "4")
k <- as.character(1:4)

# 문자열 "K", "B", "O"를 벡터로 생성하여 l에 할당
# 결과: l은 character 타입의 벡터 c("K", "B", "O")
l <- c("K", "B", "O")

# i의 각 원소가 2보다 큰지 비교하여 논리값(TRUE/FALSE) 벡터 생성
# 결과: m은 logical 타입의 벡터 c(FALSE, FALSE, TRUE, TRUE)
m <- i > 2