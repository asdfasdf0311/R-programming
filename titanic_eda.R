titanic<-read.csv("C:/Users/juhayeon/Desktop/주하연 학교/.탐색적자료분석 - 박대우/data/train.csv", header = T)
head(titanic)
summary(titanic)

#결측치 개수 확인
sum(is.na(titanic))

#공백 -> NA로 변경
titanic[titanic == ""] <- NA

#변수마다 결측치가 몇개인지 확인
library(dplyr)
titanic %>% summarise_all(~sum(is.na(.)))

#Cabin 변수는 결측치가 너무 많으므로 행을 삭제하는 방향으로 가겠다.
titanic <- subset(titanic, select = -Cabin)
summary(titanic)

#Embarked 변수는 결측치가 2개이다. 최빈값으로 대체하도록 하겠다. 
print(table(titanic$Embarked))
#확인해보았더니 최빈값은 'S'
# 'Embarked' 변수의 결측치를 'S'로 대체
titanic$Embarked[is.na(titanic$Embarked)] <- 'S'

#Age변수의 결측치는 177개이다. 전체 데이터의 개수는 891개 이므로 제거해서는 안된다.
#Age변수의 결측치는 Pclass(객실등급)에 따른 평균으로 대체해주도록 하겠다.
class_1_age <- mean(titanic$Age[titanic$Pclass == 1], na.rm = TRUE)
class_2_age <- mean(titanic$Age[titanic$Pclass == 2], na.rm = TRUE)
class_3_age <- mean(titanic$Age[titanic$Pclass == 3], na.rm = TRUE)

# Pclass에 따라 Age의 결측치 대체
titanic$Age <- ifelse(is.na(titanic$Age), 
                      ifelse(titanic$Pclass == 1, class_1_age, 
                             ifelse(titanic$Pclass == 2, class_2_age, 
                                    ifelse(titanic$Pclass == 3, class_3_age, NA))),
                      titanic$Age)


#모든 결측치가 제거되었음을 확인할 수 있다. 
titanic %>% summarise_all(~sum(is.na(.)))

#645번 승객의 나이를 보면 0.75로 이상한 값임을 확인하였다.
titanic[645,,]
#따라서 Age 변수는 정수까지만 나오게 반올림 진행
titanic$Age <- round(titanic$Age)

#Fare 변수는 값이 굉장히 난잡하므로, 분포를 조절하기 위해 로그화 진행
titanic$Fare <- log(titanic$Fare + 1)  # 로그 변환 시 0이 되지 않도록 1을 더해줌



#이제 데이터의 분포, 타입을 확인해봅시다
#데이터모양확인
head(titanic)
tail(titanic)
#데이터 통계량 확인
summary(titanic) 
#자료구조,관측치, 자료형 확인
str(titanic)

#자료 구조를 확인해보면, Age가 int형이 아닌것을 볼 수 있다. int로 변환해주겠다.
titanic$Age <- as.integer(titanic$Age)
#잘 변환이 되었다.
str(titanic)

#데이터 모양을 확인해 보았더니,
#우리는 index를 볼 수 있으므로 PassengerId 변수는 필요가 없으니 제거
titanic <- subset(titanic, select = -PassengerId)
#Name변수와 Ticket변수는 제거하는 방향이 좋을 것 같아서 제거를 하도록 하겠다.
titanic <- subset(titanic, select = -Name)
titanic <- subset(titanic, select = -Ticket)

str(titanic)
#잘 제거가 되었다.


#문자열 변수  수치화
#Sex변수와 Embarked변수는 character(문자열)타입으로 분석하기에 어려움이 있을 수 있다.

#1) Sex변수는 male = 1, female = 0으로 변환
titanic$Sex <- ifelse(titanic$Sex == "male", 1, ifelse(titanic$Sex == "female", 0, NA))

#2) Embarked변수는 one-hot인코딩을 이용하여 변환
onehot_encoded <- model.matrix(~Embarked - 1, data = titanic)
# 원핫 인코딩된 데이터프레임을 원래의 데이터셋과 합치기
titanic <- cbind(titanic, onehot_encoded)
#변환하였으므로 원래의 Embarked변수는 삭제
titanic <- subset(titanic, select = -Embarked)


head(titanic)
#잘 변경이 되었음을 확인


#연속형 변수들의 값 분포를 그래프로 확인(Age, SibSp, Parch, Fare)
par(mfrow=c(2,2))                       #3x3 가상화면 분할
for(i in 4:7) {
  hist(titanic[,i],main=colnames(titanic)[i],col="skyblue")
}

#모든변수 분포
par(mfrow=c(2,2))                       #3x3 가상화면 분할
for(i in 1:11) {
  hist(titanic[,i],main=colnames(titanic)[i],col="skyblue")
}



#이 데이터의 목표는 '승객이 생존했는지 못했는지' 이므로
#Survived 변수의 값에 따른(1 = 생존, 0 = 사망) 다른 변수들의 분포도 확인

library(ggplot2)
# [Pclass(객실등급)에 따른 생존율]
survival_rate <- aggregate(Survived ~ Pclass, data = titanic, FUN = mean)
# Pclass에 따른 생존율을 나타내는 막대 그래프 그리기
ggplot(survival_rate, aes(x = factor(Pclass), y = Survived, fill = factor(Pclass))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "생존율 by Pclass", x = "Pclass", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
#****높은 등급 객실일수록 생존율이 높음을 확인


# [Sex에 따른 생존율]
survival_rate <- aggregate(Survived ~ Sex, data = titanic, FUN = mean)
# Sex에 따른 생존율을 나타내는 막대 그래프 그리기
ggplot(survival_rate, aes(x = factor(Sex), y = Survived, fill = factor(Sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "생존율 by Sex", x = "Sex", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  scale_x_discrete(labels = c("Female", "Male"))
#****여성이 남성보다 생존율이 높음을 확인


# [Age(나이)에 따른 생존율]
# 나이를 구간별로 나누기
titanic$Age_Group <- cut(titanic$Age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80"))
# 구간별 생존율 계산
survival_rate <- aggregate(Survived ~ Age_Group, data = titanic, FUN = mean)
# 나이에 따른 생존율을 나타내는 막대 그래프 그리기
ggplot(survival_rate, aes(x = Age_Group, y = Survived, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "생존율 by Age Group", x = "Age Group", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# [Embarked에 따른 생존율] - Embarked변수를 없애지 않았을 시만 활용가능
survival_rate <- aggregate(Survived ~ Embarked, data = titanic, FUN = mean)
# Sex에 따른 생존율을 나타내는 막대 그래프 그리기
ggplot(survival_rate, aes(x = factor(Embarked), y = Survived, fill = factor(Embarked))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "생존율 by Embarked", x = "Embarked", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
#****여성이 남성보다 생존율이 높음을 확인


library(corrplot)


# 상관행렬 계산
cor_matrix <- cor(titanic[, c("Pclass", "Age", "SibSp", "Parch", "Fare", "EmbarkedC", "EmbarkedQ", "EmbarkedS")])

# 상관행렬 시각화 
par(mfrow = c(1,1))  # 그래픽 디바이스를 1x1로 설정
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, 
         tl.cex = 1.2, addCoef.col = "black", mar = c(0, 0, 2, 0))  # tl.cex로 글자 크기 조절



library(MASS)
library(robustbase)

# 그래픽 디바이스를 1x1로 설정
par(mfrow = c(1,1))
# 로버스트 다중 선형 회귀 
robust_model <- lmrob(Fare ~ Age + Pclass + SibSp + Parch, data = titanic)
summary(robust_model)

# Q-Q plot 그리기
qqnorm(robust_model$residuals)
qqline(robust_model$residuals)
