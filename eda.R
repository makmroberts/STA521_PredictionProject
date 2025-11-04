library(xgboost)
library(pROC)
library(MASS)

dat = read.csv("nepal_dat.csv")
str(dat)

model = glm(damage_grade ~ ., family = poisson, data = dat)
summary(model)

set.seed(932)
train_ids = sample(1:nrow(dat), 4*nrow(dat)/5)
train = dat[train_ids, ]
test = dat[-train_ids, ]

m2 = glm(damage_grade ~ ., family = poisson, data = train)

preds <- predict(m2, newdata = test, type = "response")
m2_roc <- roc(response = test$damage_grade, predictor = preds)
m2_auc <- m2_roc$auc

table(dat$damage_grade)

m3 = polr(factor(damage_grade) ~ ., data = train)

summary(m3)

preds3 = predict(m3, newdata = test, type = "class")

mean(abs(as.numeric(preds3) - as.numeric(dat$damage_grade)))
