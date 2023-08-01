#log linear model

data(Titanic)
library(MASS) # for loglm()
library(vcd) # for mosaic, aka plot.loglm()
install.packages("knitr")

data(Titanic)
Titanic <- Titanic + 0.5 # adjust for 0 cells
titanic.mod1 <- loglm(~ (Class * Age * Sex) + Survived, data=Titanic)
titanic.mod1
plot(titanic.mod1, main="Model [AGC][S]")

titanic.mod2 <- update(titanic.mod1, . ~ . + Survived*(Class+Age+Sex)) #기존 모델에 새로운 term 추가
titanic.mod2

titanic.mod2 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age + Sex),data=Titanic)
titanic.mod2
plot(titanic.mod2, main="Model [AGC][AS][GS][CS]")

titanic.mod3 <- loglm(~(Class * Age * Sex) + Survived * (Class + Age * Sex), data = Titanic)
titanic.mod3
plot(titanic.mod3, main = "Model [AGC][AS][GS][CS][AGS]")


anova(titanic.mod1, titanic.mod2, titanic.mod3, test="chisq")    #비교

#실행결과
# http://www.datavis.ca/courses/VCD/R/output/titanic-loglin.html


train_data2 <- train_data2 + 0.5 # adjust for 0 cells
titanic.mod1 <- loglm(~ (Class * Age * Sex) + Survived, data=train_data2)
titanic.mod1





#logistic model
#http://www.utstat.toronto.edu/~guerzhoy/303/lec/lec4/logistic_regression.pdf

#코드와 함께 logistic 모델 쭉 비교. stepsize로 선정하는 과정도 자세한듯.
#https://statkclee.github.io/ml/ml-modeling-titanic.html

#logistic regression
#https://rpubs.com/zheshuen/596809


data(titanic_test)
library(tidyverse)
library(MASS)



train_data <- titanic::titanic_train
test_data <- titanic::titanic_test

suppressMessages(library(readr))  #왜있는지 모르겟음..
suppressMessages(library(dplyr))

sapply(train_data, function(x) sum(is.na(x)))
sapply(train_data, function(x) length(unique(x)))
suppressMessages(library(Amelia))

# 2.2. 결측값에 대한 응징
# full data에 적용. 분석에 사용될 변수만 선정

train_data2 <- train_data %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%  # 결측값이 많은 Cabin과 이름, Ticket은 제거
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm=TRUE), Age)) %>%  # 나이를 평균값으로 치환
  filter(!is.na(Embarked)) %>%  # 결측값 2개 행 제거
  filter(!is.na(Fare)) %>%   # 결측값 1개 행 제거
  filter(!is.na(Survived))   # 결측값 418개 행 제거


##========================================================
## 03. 모형 적합
##========================================================
## 
suppressMessages(library(caret))
#train_data에만 적용.


anova(logit.full.m, test="Chisq")





# titan_null created for R2 computation or stepwise regression if not using MASS-stepAIC
titan_null <- glm(Survived~1,data=train_data,family = binomial)

# model 1 - complete model
titan_logistic_complete <- glm(Survived~., data=train_data, family = binomial)
summary(titan_logistic_complete)
#######################

logit.full.m <- glm(Survived ~.,family=binomial(link='logit'), data=train_data)
summary(logit.full.m)

# Model 2 - Stepwise regression starting from full model
titan_logistic_stepwise <- titan_logistic_complete %>% stepAIC(direction='both',trace = FALSE)
summary(titan_logistic_stepwise)

# compare AIC for all 3 model
AIC(titan_logistic_complete,titan_logistic_stepwise)


# Check for accuracy using test dataset
predict_2 <- predict(titan_logistic_stepwise,newdata = test,type = 'response') 
# Since Survived can only be either 1 or 0, write if statement to round up of down the response
predict_2 <- ifelse(predict_2>0.5,1,0)
error_2 <- mean(predict_2!=test$Survived)
accuracy_2 <- 1-error_2
accuracy_2


# 3.4. 모형 평가

suppressMessages(library(ROCR))
# 전체 모형
logit.full.pred <- predict(logit.full.m, newdata=titanic.test.df, type="response")
logit.full.pr <- prediction(logit.full.pred, titanic.test.df$Survived)
logit.full.prf <- performance(logit.full.pr, measure = "tpr", x.measure = "fpr")
plot(logit.full.prf)


# ROC 면적
logit.full.auc <- performance(logit.full.pr, measure = "auc")
logit.full.auc <- logit.full.auc@y.values[[1]]
logit.full.auc




##========================================================
## 04. 변수선택 모형 선정
##========================================================
## 

logit.null.m2 <- glm(Survived ~1, family=binomial(link='logit'), data=train_data2)
logit.full.m2 <- glm(Survived ~., family=binomial(link='logit'), data=train_data2)

logit.bic.m <- step(logit.null.m, scope=formula(logit.full.m), direction="both", criterion="BIC", k=log(nrow(train_data2)))

logit.aic.m <- step(logit.null.m, scope=formula(logit.full.m), direction="both", criterion="AIC", k=2)
logit.bic.m
logit.aic.m










#전체적인 eda 및 모델비교. r코드는 있는지 모르겠음-없는듯.
#http://people.stern.nyu.edu/jsimonof/classes/2301/pdf/titanic.pdf



#EDA 시각화가 잘되어있음
#https://rpubs.com/mincopxkiftu2/653928


#한국어로 eda 설명 및 시각화 친절히 보여줌. 모델은 accuracy도 한번에 나오는거같긴 한데 잘 모르겟음
#https://rpubs.com/Ryukyungjun/Kaggle_titanic_2



