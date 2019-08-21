#이 코드를 이용하려면 설치해야 하는 패키지들입니다.
install.packages("class")
install.packages("tree")
install.packages("nnet")
install.packages("ggplot2")
#업데이트는 패키지가 실행이 안될 때, 적용하시면 됩니다.
update.packages('class')

# KNN기법을 이용하기 위해 설치해야 하는 패키지들입니다.
library(class)
# Decision tree기법을 이용하기 위해 설치해야 하는 패키지들입니다.
library(tree)
# Multinomial logistic regression(다항 로지스틱 회귀분석)기법을 이용하기 위해 설치해야 하는 패키지들입니다.
library(nnet)
# 그래프을 그리기 위해 설치해야 하는 패키지들입니다.
library(ggplot2)
# sample로 인해 생기는 랜덤 변수를 고정해주었습니다.(seed 고정)
set.seed(999)

#iris 데이터 설명한 링크입니다.
#https://thebook.io/006723/ch04/01/

#iris데이터의 구조
str(iris)

#iris데이터의 산점도
plot(iris[,1:4],col=iris$Species)

#iris데이터 정보를 나타내줍니다.(Summary)
summary(iris)
View(iris)

#데이터의 시각화를 위해서 값들을 범주형으로 변경했습니다.
iris_fac = floor(iris[,1:4]*2)
iris_fac$Species = iris$Species
iris_fac

iris_fac$Sepal.Length = as.factor(iris_fac$Sepal.Length)
iris_fac$Sepal.Width = as.factor(iris_fac$Sepal.Width)
iris_fac$Petal.Length = as.factor(iris_fac$Petal.Length)
iris_fac$Petal.Width = as.factor(iris_fac$Petal.Width)

p <- ggplot(iris_fac,aes(Sepal.Width,fill=Species))+geom_bar(position='fill')
p
p <- ggplot(iris_fac,aes(Sepal.Length,fill=Species))+geom_bar(position='fill')
p
p <- ggplot(iris_fac,aes(Petal.Width,fill=Species))+geom_bar(position='fill')
p
p <- ggplot(iris_fac,aes(Petal.Length,fill=Species))+geom_bar(position='fill')
p

#결과: 모든 feature가 y값에 중요한 것으로 판단됩니다.


# 다른 분석기법을 적용하기 위해 trainset를 설정합니다.
# testset 분리 비율은 7 : 3 으로 했습니다. 데이터가 150개 밖에 되지 않아 조금 비율을 많이 설정했습니다.
idx <- sample(c('train','test'),nrow(iris),replace = TRUE, prob = c(0.7,0.3))
iris_train <- iris[idx == 'train',]
iris_test <- iris[idx == 'test',]

nrow(iris_train)
nrow(iris_test)

iris_train_x <- iris_train[,1:4]
iris_train_y <- iris_train[,5]

iris_test_x <- iris_test[,1:4]
iris_test_y <- iris_test[,5]


# Decision tree model에 적용합니다.
iris_treemodel <- tree(iris_train$Species ~., data = iris_train)
predict_tree <- predict(iris_treemodel, iris_test, type = 'class')
table(iris_test_y, predict_tree)
mean(iris_test_y == predict_tree)
plot(iris_treemodel)
text(iris_treemodel)

# test모델과 Decision tree model에 적용시킨 모델을 비교해봅니다.
plot(iris_test_x, col = iris_test_y)
plot(iris_test_x, col = predict_tree)


# iris 데이터를 정규화시킵니다.
iris_scale <- as.data.frame(scale(iris[,1:4]))
iris_scale$Species <- iris$Species

# idx <- sample(c('train','test'),nrow(iris),replace = TRUE, prob = c(0.7,0.3))입니다.
# train데이터와 test데이터를 만들어줍니다.
iris_scale_train <- iris_scale[idx == 'train',]
iris_scale_test <- iris_scale[idx == 'test',]

iris_scale_train_x <- iris_scale_train[,1:4]
iris_scale_train_y <- iris_scale_train[,5]

iris_scale_test_x <- iris_scale_test[,1:4]
iris_scale_test_y <- iris_scale_test[,5]

# KNN model를 적용시킵니다.
predict_knn <- knn(train = iris_scale_train_x, test = iris_scale_test_x, cl = iris_scale_train_y, k = 3)
table(iris_scale_test_y, predict_knn)
mean(iris_scale_test_y == predict_knn)

# test모델과 KNN model에 적용시킨 모델을 비교해봅니다.
plot(iris_scale_test_x, col = iris_scale_test_y)
plot(iris_scale_test_x, col = predict_knn)


# Multinomial logistic model(다항 로지스틱 회귀분석)기법을 적용시킵니다.
iris_multinom <- multinom(iris_scale_train$Species ~., data = iris_scale_train)
predict_multinom <- predict(iris_multinom, iris_scale_test, type = 'class')
table(iris_scale_test_y, predict_multinom)
mean(iris_scale_test_y == predict_multinom)

# test모델과 Multinomial logistic model(다항 로지스틱 회귀분석)에 적용시킨 모델을 비교해봅니다.
plot(iris_scale_test_x, col = iris_scale_test_y)
plot(iris_scale_test_x, col = predict_multinom)
