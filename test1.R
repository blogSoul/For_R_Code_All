# https://m.blog.naver.com/PostView.nhn?blogId=leedk1110&logNo=220774824473&proxyReferer=https%3A%2F%2Fwww.google.com%2F
head(state.x77)
colnames(state.x77)

states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
head(states)

fit <- lm(Murder~Population+Illiteracy+Income+Frost,data = states) 
fit <- lm(Murder~.,data = states) 
fit
plot(Murder~.,data = states)

summary(fit)

fit2 <- lm(Murder~Population + Illiteracy, data = states)
summary(fit2)

#이번에는 위 데이터에 income에서 수입이 높은 사람과 낮을사람을 구별하는 가변수를 만들어
#회귀모형에 적합시켜보겠습니다.
rich <- NA
tmp <- cbind(states,rich)
head(tmp)
summary(tmp$Income) 
#평균보다 높으면 1 아니면 0
tmp$rich[tmp$Income>4436] <- 1
tmp$rich[is.na(tmp$rich)] <- 0
head(tmp)

tmp$rich <- as.factor(tmp$rich)
fit <- lm(Murder~.,data = tmp) 
summary(fit)

#이번에는 rich라는 가변수를 포함한 각각의 산점도와 다중공성성에 대해 알아보겠습니다.
install.packages("psych")
library(psych)
install.packages("car")
library(car)
pairs.panels(tmp[names(tmp)])
#위 그래프에서 숫자는 상관관계를 나타내며, -1에 가까우면 음의 상관관계, 1에 가까우면 양의 상관관계입니다.
car::vif(fit) 
sqrt(car::vif(fit)) > 2
#'sqrt(car::vif(fit))' 가 2보다 크면 다중공선성이 있는 것입니다. 

#하나씩 제거하다 보면 유의하지 않았던 변수가 유의해 질 수도 있습니다.
#방법은 전진 선택, 후진 선택이 있습니다.
fit.con <- lm(Murder~1,data=tmp) 
fit.both <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "both")
fit.both

#마지막으로 지금까지 새운 모델로 살인률(y, 종속변수)을 예측해 보겠습니다.

pre_murder <- predict(fit.both, newdata = tmp) 
pre_murder <- as.data.frame(pre_murder)
pre_murder 

pre_murder <- predict(fit.both, newdata = tmp, interval = "confidence") 
pre_murder <- as.data.frame(pre_murder)
pre_murder

ttmp <- cbind(pre_murder,tmp$Murder) 
ttmp
