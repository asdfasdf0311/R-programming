#EDA 데이터탐색
#데이터불러오기
dat <- read.csv("BostonHousing.csv", header = TRUE)
#데이터모양확인
head(dat)
tail(dat)
#데이터 통계량 확인
summary(dat) 
#자료구조,관측치, 자료형 확인
str(dat)
#결측치 확인
is.na(dat)
sum(is.na(dat))

#변수들의 분포 확인
par(mfrow=c(4,4))                       #4x4 가상화면 분할
for(i in 1:14) {
  hist(dat[,i],main=colnames(dat)[i],col="skyblue")
}


#주택 가격을 H,M,L로 나누기 위한 grp변수추가
grp <- c()
for (i in 1:nrow(dat)){  #dat$medv 값에 따라 그룹 분류
  if (dat$medv[i] >= 21.0){
    grp[i] <- "H"
  } else {
    grp[i] <- "L"
  }
}
grp <-factor(grp)  #문자벡터를 팩터 타입으로 변경
grp <- factor(grp, levels=c("H","L"))  #레벨의 순서를 H,L -> H,L

dat <- data.frame(dat, grp)  #myds에 grp 컬럼 추가

#추가된데이터 확인
str(dat)
head(dat)
table(dat$grp)

#두 개의 그룹의 평균벡터가 차이가 나는지 안나는지
groupH <- dat[dat$grp == "H", -15] # High
groupL <- dat[dat$grp == "L", -15] # Low

colMeans(groupH)
colMeans(groupL)

library(Hotelling)#mean vector
#귀무가설 : 두개의 mean vector가 동일하다
#p-value가 0.05보다 작으면 귀무가설 기각

result <- hotelling.test(x = groupH, y = groupL)
result


#두 그룹의 Covariance Matrix가 동일한지 아닌지
round(cov(groupH), 2) # High
round(cov(groupL), 2) # Low

library(heplots)

result <- boxM(cbind(crim, zn, indus, chas, nox, rm, age, dis, rad, tax, 
                     ptratio, black, lstat) ~ grp, data = dat)
result


#상관행렬을 이용하여 변수간 상관관계 분석
dat_real <- read.csv("BostonHousing.csv", header = TRUE)
dat_cor <- round(cor(dat_real),2)
dat_cor
#히트맵
library(corrplot)
corrplot(dat_cor)
#숫자형태로
corrplot(dat_cor, method="number")

#H, L 각 그룹별 관측값 분포 확인
par(mfrow=c(2,2))
for(i in 1:14) {
  boxplot(dat[,i]~dat$grp,main=colnames(dat)[i])
}




#데이터분석-주성분 분석
dat <- read.csv("BostonHousing.csv", header = TRUE)
dat <- dat[, -4]
dat#범주형, 제외

fit_pca <- princomp(dat, cor = TRUE)
fit_pca$sdev^2
fit_pca$loadings

summary(fit_pca)
screeplot(fit_pca, npcs = 13, type = "lines", main = "scree plot")
biplot(fit_pca)

library(factoextra)
fviz_eig(fit_pca)
summary(fit_pca)
fviz_contrib(fit_pca, choice = "var", axes = 1) #PC1영향받는것
fviz_contrib(fit_pca, choice = "var", axes = 2) #PC2영향받는것
fviz_contrib(fit_pca, choice = "var", axes = 3) #PC3영향받는것
fviz_contrib(fit_pca, choice = "var", axes = 4) #PC4영향받는것

fviz_pca_var(fit_pca,
             col.var = "cos2", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_biplot( fit_pca,
                 repel = TRUE,
                 geom = c("point"),
                 col.var = "#2E9FDF", # Variables color
                 col.ind = "#696969"  # Individuals color
)
