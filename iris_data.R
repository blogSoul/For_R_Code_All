
# knn
library(class)
# decision tree
library(tree)
# multinomial logistic regression 다항 로지스틱 회귀분석
library(nnet)
# 그래프
library(ggplot2)
# seed 고정
set.seed(999)

#iris 데이터 설명한 링크입니다.
#https://thebook.io/006723/ch04/01/

#iris데이터의 구조
str(iris)

#iris데이터의 산점도
plot(iris[,1:4],col=iris$Species)

#Summary
summary(iris)

#데이터의 시각화를 위해서 값들을 범주형으로 변경
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

#결과: 모든 feature가 y값에 중요한 것으로 판단된다




# iris 데이터 분석
# 데이터 확인
# View(iris)
table(is.na(iris))

plot(iris[,1:4],col = iris$Species)

# trainset, testset 분리 : 7 : 3 으로 분리
idx <- sample(c('train','test'),nrow(iris),replace = TRUE, prob = c(0.7,0.3))
iris_train <- iris[idx == 'train',]
iris_test <- iris[idx == 'test',]

iris_train_x <- iris_train[,1:4]
iris_train_y <- iris_train[,5]

iris_test_x <- iris_test[,1:4]
iris_test_y <- iris_test[,5]


# decision tree model
iris_treemodel <- tree(iris_train$Species ~., data = iris_train)
predict_tree <- predict(iris_treemodel, iris_test, type = 'class')
table(iris_test_y, predict_tree)
mean(iris_test_y == predict_tree)
plot(iris_treemodel)
text(iris_treemodel)
plot(iris_test_x, col = iris_test_y)
plot(iris_test_x, col = predict_tree)




# iris 데이터 정규화
iris_scale <- as.data.frame(scale(iris[,1:4]))
iris_scale$Species <- iris$Species




# idx <- sample(c('train','test'),nrow(iris),replace = TRUE, prob = c(0.7,0.3))
iris_scale_train <- iris_scale[idx == 'train',]
iris_scale_test <- iris_scale[idx == 'test',]

iris_scale_train_x <- iris_scale_train[,1:4]
iris_scale_train_y <- iris_scale_train[,5]

iris_scale_test_x <- iris_scale_test[,1:4]
iris_scale_test_y <- iris_scale_test[,5]



# knn model
predict_knn <- knn(train = iris_scale_train_x, test = iris_scale_test_x, cl = iris_scale_train_y, k = 3)
table(iris_scale_test_y, predict_knn)
mean(iris_scale_test_y == predict_knn)
plot(iris_scale_test_x, col = iris_scale_test_y)
plot(iris_scale_test_x, col = predict_knn)


# multinomial logistic model
iris_multinom <- multinom(iris_scale_train$Species ~., data = iris_scale_train)
predict_multinom <- predict(iris_multinom, iris_scale_test, type = 'class')
table(iris_scale_test_y, predict_multinom)
mean(iris_scale_test_y == predict_multinom)
plot(iris_scale_test_x, col = iris_scale_test_y)
plot(iris_scale_test_x, col = predict_multinom)












##

my_test = floor(iris[,1:4]*2)
my_test$Species = iris$Species
my_test
