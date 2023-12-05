titanic<-read.csv("C:/Users/juhayeon/Desktop/���Ͽ� �б�/.Ž�����ڷ�м� - �ڴ��/data/train.csv", header = T)
head(titanic)
summary(titanic)

#����ġ ���� Ȯ��
sum(is.na(titanic))

#���� -> NA�� ����
titanic[titanic == ""] <- NA

#�������� ����ġ�� ����� Ȯ��
library(dplyr)
titanic %>% summarise_all(~sum(is.na(.)))

#Cabin ������ ����ġ�� �ʹ� �����Ƿ� ���� �����ϴ� �������� ���ڴ�.
titanic <- subset(titanic, select = -Cabin)
summary(titanic)

#Embarked ������ ����ġ�� 2���̴�. �ֺ����� ��ü�ϵ��� �ϰڴ�. 
print(table(titanic$Embarked))
#Ȯ���غ��Ҵ��� �ֺ��� 'S'
# 'Embarked' ������ ����ġ�� 'S'�� ��ü
titanic$Embarked[is.na(titanic$Embarked)] <- 'S'

#Age������ ����ġ�� 177���̴�. ��ü �������� ������ 891�� �̹Ƿ� �����ؼ��� �ȵȴ�.
#Age������ ����ġ�� Pclass(���ǵ��)�� ���� ������� ��ü���ֵ��� �ϰڴ�.
class_1_age <- mean(titanic$Age[titanic$Pclass == 1], na.rm = TRUE)
class_2_age <- mean(titanic$Age[titanic$Pclass == 2], na.rm = TRUE)
class_3_age <- mean(titanic$Age[titanic$Pclass == 3], na.rm = TRUE)

# Pclass�� ���� Age�� ����ġ ��ü
titanic$Age <- ifelse(is.na(titanic$Age), 
                      ifelse(titanic$Pclass == 1, class_1_age, 
                             ifelse(titanic$Pclass == 2, class_2_age, 
                                    ifelse(titanic$Pclass == 3, class_3_age, NA))),
                      titanic$Age)


#��� ����ġ�� ���ŵǾ����� Ȯ���� �� �ִ�. 
titanic %>% summarise_all(~sum(is.na(.)))

#645�� �°��� ���̸� ���� 0.75�� �̻��� ������ Ȯ���Ͽ���.
titanic[645,,]
#���� Age ������ ���������� ������ �ݿø� ����
titanic$Age <- round(titanic$Age)

#Fare ������ ���� ������ �����ϹǷ�, ������ �����ϱ� ���� �α�ȭ ����
titanic$Fare <- log(titanic$Fare + 1)  # �α� ��ȯ �� 0�� ���� �ʵ��� 1�� ������



#���� �������� ����, Ÿ���� Ȯ���غ��ô�
#�����͸��Ȯ��
head(titanic)
tail(titanic)
#������ ��跮 Ȯ��
summary(titanic) 
#�ڷᱸ��,����ġ, �ڷ��� Ȯ��
str(titanic)

#�ڷ� ������ Ȯ���غ���, Age�� int���� �ƴѰ��� �� �� �ִ�. int�� ��ȯ���ְڴ�.
titanic$Age <- as.integer(titanic$Age)
#�� ��ȯ�� �Ǿ���.
str(titanic)

#������ ����� Ȯ���� ���Ҵ���,
#�츮�� index�� �� �� �����Ƿ� PassengerId ������ �ʿ䰡 ������ ����
titanic <- subset(titanic, select = -PassengerId)
#Name������ Ticket������ �����ϴ� ������ ���� �� ���Ƽ� ���Ÿ� �ϵ��� �ϰڴ�.
titanic <- subset(titanic, select = -Name)
titanic <- subset(titanic, select = -Ticket)

str(titanic)
#�� ���Ű� �Ǿ���.


#���ڿ� ����  ��ġȭ
#Sex������ Embarked������ character(���ڿ�)Ÿ������ �м��ϱ⿡ ������� ���� �� �ִ�.

#1) Sex������ male = 1, female = 0���� ��ȯ
titanic$Sex <- ifelse(titanic$Sex == "male", 1, ifelse(titanic$Sex == "female", 0, NA))

#2) Embarked������ one-hot���ڵ��� �̿��Ͽ� ��ȯ
onehot_encoded <- model.matrix(~Embarked - 1, data = titanic)
# ���� ���ڵ��� �������������� ������ �����ͼ°� ��ġ��
titanic <- cbind(titanic, onehot_encoded)
#��ȯ�Ͽ����Ƿ� ������ Embarked������ ����
titanic <- subset(titanic, select = -Embarked)


head(titanic)
#�� ������ �Ǿ����� Ȯ��


#������ �������� �� ������ �׷����� Ȯ��(Age, SibSp, Parch, Fare)
par(mfrow=c(2,2))                       #3x3 ����ȭ�� ����
for(i in 4:7) {
  hist(titanic[,i],main=colnames(titanic)[i],col="skyblue")
}

#��纯�� ����
par(mfrow=c(2,2))                       #3x3 ����ȭ�� ����
for(i in 1:11) {
  hist(titanic[,i],main=colnames(titanic)[i],col="skyblue")
}



#�� �������� ��ǥ�� '�°��� �����ߴ��� ���ߴ���' �̹Ƿ�
#Survived ������ ���� ����(1 = ����, 0 = ���) �ٸ� �������� ������ Ȯ��

library(ggplot2)
# [Pclass(���ǵ��)�� ���� ������]
survival_rate <- aggregate(Survived ~ Pclass, data = titanic, FUN = mean)
# Pclass�� ���� �������� ��Ÿ���� ���� �׷��� �׸���
ggplot(survival_rate, aes(x = factor(Pclass), y = Survived, fill = factor(Pclass))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "������ by Pclass", x = "Pclass", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
#****���� ��� �����ϼ��� �������� ������ Ȯ��


# [Sex�� ���� ������]
survival_rate <- aggregate(Survived ~ Sex, data = titanic, FUN = mean)
# Sex�� ���� �������� ��Ÿ���� ���� �׷��� �׸���
ggplot(survival_rate, aes(x = factor(Sex), y = Survived, fill = factor(Sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "������ by Sex", x = "Sex", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  scale_x_discrete(labels = c("Female", "Male"))
#****������ �������� �������� ������ Ȯ��


# [Age(����)�� ���� ������]
# ���̸� �������� ������
titanic$Age_Group <- cut(titanic$Age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80), labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80"))
# ������ ������ ���
survival_rate <- aggregate(Survived ~ Age_Group, data = titanic, FUN = mean)
# ���̿� ���� �������� ��Ÿ���� ���� �׷��� �׸���
ggplot(survival_rate, aes(x = Age_Group, y = Survived, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "������ by Age Group", x = "Age Group", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# [Embarked�� ���� ������] - Embarked������ ������ �ʾ��� �ø� Ȱ�밡��
survival_rate <- aggregate(Survived ~ Embarked, data = titanic, FUN = mean)
# Sex�� ���� �������� ��Ÿ���� ���� �׷��� �׸���
ggplot(survival_rate, aes(x = factor(Embarked), y = Survived, fill = factor(Embarked))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "������ by Embarked", x = "Embarked", y = "Survival Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
#****������ �������� �������� ������ Ȯ��


library(corrplot)


# ������ ���
cor_matrix <- cor(titanic[, c("Pclass", "Age", "SibSp", "Parch", "Fare", "EmbarkedC", "EmbarkedQ", "EmbarkedS")])

# ������ �ð�ȭ 
par(mfrow = c(1,1))  # �׷��� ����̽��� 1x1�� ����
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, 
         tl.cex = 1.2, addCoef.col = "black", mar = c(0, 0, 2, 0))  # tl.cex�� ���� ũ�� ����



library(MASS)
library(robustbase)

# �׷��� ����̽��� 1x1�� ����
par(mfrow = c(1,1))
# �ι���Ʈ ���� ���� ȸ�� 
robust_model <- lmrob(Fare ~ Age + Pclass + SibSp + Parch, data = titanic)
summary(robust_model)

# Q-Q plot �׸���
qqnorm(robust_model$residuals)
qqline(robust_model$residuals)