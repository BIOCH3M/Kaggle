#library
library(tidyverse)
library(dplyr)
library(MASS)
library(boot)
library(glmnet)
library(leaps)
library(splines)
library(gam)
library(akima)
library(boot)
library(tree)
library(randomForest)
library(gbm)
library(neuralnet)

###########################DATA WRANGLING
#bring in data
raw_data_macro<-as.data.frame(read.csv("/Users/asurin/Documents/Kaggle/Sberbank/macro.csv", header = T,na.strings = "NA"))
raw_data_test<-data.frame(read.csv("/Users/asurin/Documents/Kaggle/Sberbank/test.csv",na.strings = "NA"))
raw_data_train<-data.frame(as.factor(read.csv("/Users/asurin/Documents/Kaggle/Sberbank/train.csv", na.strings = "NA")))
#Factors will not work in regressions, so you need to find all varialbes in factor form
(l <- sapply(raw_data_train, function(x) is.factor(x)))
l
m <- raw_data_train[, l]
typeof(m$sub_area)
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
as.factor(m$culture_objects_top_25)
typeof(m$culture_objects_top_25)
glm(train$price_doc~culture_objects_top_25)
glm(train$price_doc~timestamp)

names(raw_data_train)
##############TRAININ/TEST
set.seed(1)
#Training and test set
index <- sample(1:nrow(raw_data_train),round(0.80*nrow(raw_data_train)))
train <- raw_data_train[index,]
test <- raw_data_train[-index,]
typeof(raw_data_train)
nrow(test)
attach(train)
###Run regular regression and look at MSE
glm.fit<-glm(train$price_doc~., data = train)
glm.fit
pred.glm<-predict(glm.fit, test)
#MSE
mse.ols<-mean((pred.glm-test$price_doc)^2)
mse.ols
# ######Use validation set approach model:
# 
# regfit.best<-regsubsets(price_doc~., data=train, nvmax=13)
# #create model matrix from the test data
# test.mat<-model.matrix(price_doc~.,data=test )
# #create loop for each model size
# 
# val.errors<-rep(NA,13)
# for (i in 1:13){
#   coefi<-coef(regfit.best,id=i)
#   pred<-test.mat[,names(coefi)]%*%coefi
#   #test MSE
#   val.errors[i]<-mean((test$price_doc-pred)^2)
#   
# }
# 
# which.min(val.errors)
# mse.val.errors<-mean(val.errors)
# mse.val.errors
# #perfrom best subset selection on full data set and select best 11 variable model
# regfit.best<-regsubsets(price_doc~., data = Boston,nvmax = 11)
# coef(regfit.best,11)
# 
# 
# ##########Now I perform cross validation 
# #create matrix for results
# k=10
# set.seed(1)
# folds<-sample(1:k,nrow(Boston), replace=TRUE)
# cv.errors<-matrix(NA,k,13,dimnames = list(NULL,paste(1:13)))
# 
# #create loop which will have jth fold, and elements there are test set and remanining are traing
# #predict and calclate test errors
# mean(cv.errors)
# predict.regsubsets=function(object, newdata, id,...){
#   form=as.formula(object$call[[2]])
#   mat=model.matrix(form, newdata)
#   coefi=coef(object, id=id)
#   xvars=names(coefi)
#   mat[,xvars]%*%coefi
# }
# 
# 
# for (j in 1:k){
#   best.fit<-regsubsets(price_doc~., data=Boston[folds !=j,],
#                        nvmax = 13)
#   for(i in 1:13){
#     pred=predict(best.fit, Boston[folds==j,], id=i)
#     cv.errors[j,i]=mean((Boston$price_doc[folds==j]-pred)^2)
#   }
# }
# 
# mean.cv.errors<-mean(apply(cv.errors,2,mean))
# 
# par(mfrow<-c(1,1))
# plot(mean.cv.errors,type='b')
# reg.best<-regsubsets(price_doc~.,data=Boston, nvmax = 11)
# coef(reg.best,11)
# 
# 
# #########I now fit a ridge regression:
# 
# train.mat <- model.matrix(price_doc ~ ., data = train)
# test.mat <- model.matrix(price_doc ~ ., data = test)
# grid <- 10 ^ seq(4, -2, length = 100)
# fit.ridge <- glmnet(train.mat, train$price_doc, alpha = 0, lambda = grid,thresh = 1e-12)
# cv.ridge <- cv.glmnet(train.mat, train$price_doc, alpha = 0, lambda = grid,thresh = 1e-12)
# bestlam.ridge <- cv.ridge$lambda.min
# #get best lambda according to cross validation
# bestlam.ridge
# #now find the MSE by running this model on test
# pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
# #MSE 
# mse.ridge<-mean((pred.ridge - test$price_doc)^2)
# mse.ridge
# #let's see which coeff are zero
# out.ridge<-predict(fit.ridge,s=bestlam.ridge,type = "coefficients")
# out.ridge
# 
# 
# ###########I now fit a lasso regression
# 
# fit.lasso <- glmnet(train.mat, train$price_doc, alpha = 1, lambda = grid, thresh = 1e-12)
# cv.lasso <- cv.glmnet(train.mat, train$price_doc, alpha = 1, lambda = grid, thresh = 1e-12)
# bestlam.lasso <- cv.lasso$lambda.min
# #get best lambda according to cross validation
# bestlam.lasso
# #now find the MSE by running this model on test
# pred.lasso <- predict(fit.lasso, s = bestlam.ridge, newx = test.mat)
# #MSE 
# mse.lasso<-mean((pred.lasso - test$price_doc)^2)
# mse.lasso
# #let's see which coeff are zero
# out.lasso<-predict(fit.lasso,s=bestlam.lasso,type = "coefficients")
# out.lasso
# 
# 
# ################################################# ADDITIVE MODELS
# 
# #Find best fit model 
# fit.step <- regsubsets(price_doc ~ ., data = train, nvmax = 13, method = "forward")
# fit.step.summary <- summary(fit.step)
# #NOW PLOT RSS, Cp and BIC and adjusted R.squared to find best model
# par(mfrow = c(1, 4))
# 
# #Cp 
# plot(fit.step.summary$adjr2, xlab = "Number of variables", ylab = "Cp", type = 'l')
# which.min(fit.step.summary$cp)#11
# points(11,fit.step.summary$adjr2[11],col="red", cex=2,pch=25)
# 
# #RSS
# plot(fit.step.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
# which.min(fit.step.summary$rss)#13
# points(13,fit.step.summary$rss[13],col="red", cex=2,pch=25)
# 
# #adjR2
# plot(fit.step.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = 'l')
# which.max(fit.step.summary$adjr2)#11
# 
# points(11,fit.step.summary$adjr2[11],col="red", cex=2,pch=25)
# 
# #BIC
# plot(fit.step.summary$bic, xlab = "Number of variables", ylab = "BIC", type = 'l')
# which.min(fit.step.summary$bic)#11
# points(11,fit.step.summary$bic[11], col="red", cex=2,pch=25)
# 
# 
# 
# 
# #####################CHECKING FOR NON-LINEAR FIT and getting best degrees of freedom for the model
# set.seed(1)
# pairs(Boston)
# #for price_doc i see that there looks to be a non-linear relationship with variable'lstat'
# # 
# # # #TEST ERROR OBTAINED THROUGH CROSS VALIDATION
# # crosv_crim<-rep(NA, 15)
# # crosv_zn<-rep(NA, 15)
# # crosv_nox<-rep(NA, 15)
# # crosv_rm<-rep(NA, 15)
# # crosv_dis<-rep(NA, 15)
# # crosv_rad<-rep(NA, 15)
# # crosv_tax<-rep(NA, 15)
# # crosv_ptratio<-rep(NA, 15)
# # crosv_black<-rep(NA, 15)
# # crosv_lstat<-rep(NA, 15)
# # 
# # for (i in 3:15){
# #   fcrim<-glm(price_doc~bs(crim,df=i))
# #   fzn<-glm(price_doc~bs(zn,df=i))
# #   fnox<-glm(price_doc~bs(nox,df=i))
# #   frm<-glm(price_doc~bs(rm,df=i))
# #   fdis<-glm(price_doc~bs(dis,df=i))
# #   frad<-glm(price_doc~bs(rad,df=i))
# #   ftax<-glm(price_doc~bs(tax,df=i))
# #   fptratio<-glm(price_doc~bs(ptratio,df=i))
# #   fblack<-glm(price_doc~bs(black,df=i))
# #   flastat<-glm(price_doc~bs(lstat,df=i))
# #   #crossval
# #   crosv_lstat[i]=cv.glm(Boston,flastat,K=10)$delta[1]
# #   crosv_crim[i]=cv.glm(Boston,fcrim,K=10)$delta[1]
# #   crosv_zn[i]=cv.glm(Boston,fzn,K=10)$delta[1]
# #   crosv_nox[i]=cv.glm(Boston,fnox,K=10)$delta[1]
# #   crosv_rm[i]=cv.glm(Boston,frm,K=10)$delta[1]
# #   crosv_dis[i]=cv.glm(Boston,dis,K=10)$delta[1]
# #   crosv_rad[i]=cv.glm(Boston,frad,K=10)$delta[1]
# #   crosv_tax[i]=cv.glm(Boston,tax,K=10)$delta[1]
# #   crosv_ptratio[i]=cv.glm(Boston,fptratio,K=10)$delta[1]
# # crosv_black[i]=cv.glm(Boston,fblack,K=10)$delta[1]
# # }
# 
# # plot(3:15, crosv_black[-c(1,2)], xlab="Degrees of Freedom", ylab = "Test Error obtained Through CV")
# 
# ##################### HERE I TRY TO FIT SPLINES
# #So now I fit splines
# 
# ##################### Here I fit the GAM
# 
# # fit.gam <- gam(price_doc ~ s(crim,2) + s(zn, df = 3) + s(chas, df = 3) + s(nox, df = 3) + s(rm, df = 3) + 
# #                  s(rad, df = 3)+s(tax, df = 3)+s(ptratio, df = 3)+s(black, df = 3)+s(lstat, df = 3), data=train)
# # 
# # fit.gam <- gam(price_doc ~ crim  + zn + chas+nox+ rm + 
# #                  rad +tax +ptratio+black+lstat, data=train)
# 
# fit.gam <- gam(price_doc ~ s(crim,2) + s(zn,2) + chas +s(nox,2)+ s(rm,2) + s(dis,2) 
#                +s(rad,2) +s(tax,2) +s(ptratio,2)+s(black,2)+s(lstat,2), data=train)
# summary(fit.gam)
# # fit.gam<-gam(price_doc~.-age -indus,data=train)
# #fit3b <- gam(Outstate ~ Private + s(Room.Board, df = 2) + s(Terminal, df = 2) + s(perc.alumni, df = 2) + s(Expend, df = 2) + s(Grad.Rate, df = 2), data=train2)
# 
# par(mfrow = c(2, 3))
# plot.gam(fit.gam, se=T, col="blue")
# #plot.gam(fit3b, se=T, col="blue")
# 
# #MEASURE MSE
# pred.gam<-predict(fit.gam, test)
# pred.gam
# #MSE
# mse.gam<-mean((pred.gam - test$price_doc)^2)
# mse.gam
# 
# summary(fit.gam)
# 
# ########################## REGRESSION TREES
# tree.Boston<-tree(price_doc~., data=train)
# summary(tree.Boston)
# plot(tree.Boston)
# text(tree.Boston,pretty = 0)
# #look at the MSE
# pred.tree<-predict(tree.Boston,newdata = test)
# mse.tree<-mean((pred.tree-test$price_doc)^2)
# #MSE
# mse.tree
# 
# #Use cross-validation to determine optimal tree complexity
# cv.boston<-cv.tree(tree.Boston)
# names(cv.boston)
# par(mfrow=c(1,2))
# #you can choose the tree with more pruned see if it helps by this graph
# plot(cv.boston$size,cv.boston$dev, xlim = c(0,12))
# 
# 
# prune.boston<-prune.tree(tree.Boston,best=4)
# plot(prune.boston)
# text(prune.boston,pretty = 0)
# #look at the MSE
# pred.tree.pruned<-predict(prune.boston,newdata = test)
# mse.tree.pruned<-mean((pred.tree.pruned-test$price_doc)^2)
# #MSE
# mse.tree.pruned
# 
# 
# ######################Bagging Tree
# 
# set.seed(1)
# bag.boston<-randomForest(price_doc~.,data =train, mtry=13,importance=TRUE)
# bag.boston
# #look at the MSE
# pred.bag<-predict(bag.boston,newdata = test)
# mse.bag<-mean((pred.bag-test$price_doc)^2)
# #MSE
# mse.bag
# 
# #change numbeer of tree argument
# bag.boston.2<-randomForest(price_doc~.,data =train, mtry=13,ntree=25)
# bag.boston.2
# #look at the MSE
# pred.bag.2<-predict(bag.boston.2,newdata = test)
# mse.bag.2<-mean((pred.bag.2-test$price_doc)^2)
# #MSE
# mse.bag.2
# 
# ######################RANDOM FOREST
# rf.boston<-randomForest(price_doc~.,data =train, mtry=6,importance=TRUE)
# rf.boston
# #look at the MSE
# pred.rf<-predict(rf.boston,newdata = test)
# mse.rf<-mean((pred.rf-test$price_doc)^2)
# #MSE
# mse.rf
# 
# ######################BOOSTING
# 
# boost.boston<-gbm(price_doc~.,data=train, distribution="gaussian", n.trees = 5000, interaction.depth=4)
# summary(boost.boston)
# #here we can see how important are all of the variables 
# 
# #we can produce plots of partial dependence
# 
# # par(mfrow=c(1,2))
# #since these are improtant
# # plot(boost.boston, i="rm")
# # plot(boost.boston, i="lstat")
# 
# #now we try to predict price_doc on test set
# 
# pred.boost=predict(boost.boston, newdata=test, n.trees=5000)
# #this MSE is almost exactly what random forest is
# mse.boost<-mean((pred.boost-test$price_doc)^2)
# #MSE
# mse.boost
# 
# #Alternate shrinkage
# 
# boost.boston.2<-gbm(price_doc~.,data=train, distribution="gaussian", n.trees = 5000, interaction.depth=4, shrinkage = .02)
# summary(boost.boston.2)
# pred.boost.2=predict(boost.boston.2, newdata=test, n.trees=5000)
# #this MSE is almost exactly what random forest is
# mse.boost.2<-mean((pred.boost.2-test$price_doc)^2)
# #MSE####BEST MSE SO FAR
# mse.boost.2
# 
# ######################################NEURAL NETWORKS
# 
# #clear plots:
# dev.off()
# #normalize your data!  Here scale in [0,1] 
# set.seed(1)
# #Training and test set
# index <- sample(1:nrow(Boston),round(0.80*nrow(Boston)))
# train <- Boston[index,]
# test <- Boston[-index,]
# 
# #normalize your data!  Here scale in [0,1] 
# scaled <- as.data.frame(scale(Boston, center = TRUE, scale = TRUE))
# train_ <- scaled[index,]
# test_ <- scaled[-index,]
# 
# #PARAMETERS:
# #we are going to use 2 hidden layers with this configuration: 13:5:3:1. 
# #The input layer has 13 inputs, the two hidden layers have 5 and 3 neurons,
# #and the output layer has, of course, a single output since we are doing regression.
# 
# n <- names(train_)
# f <- as.formula(price_doc ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
#                   tax + ptratio + black + lstat)
# #For some reason the formula y~. is not accepted in the neuralnet() function. 
# #You need to first write the formula and then pass it as an argument in the fitting function.
# nn <- neuralnet(f,data=train_,hidden=c(7,5,3),linear.output=TRUE)
# # nn.2 <- neuralnet(f,data=train_,hidden=c(5,2,2),linear.output=TRUE)
# #The hidden argument accepts a vector with the number of neurons 
# #for each hidden layer, while the argument linear.output is used to specify 
# #whether we want to do regression linear.output=TRUE or classification linear.output=FALSE
# nn
# #Illustrate NN
# # plot(nn)
# 
# #Predict
# pr.nn <- compute(nn,test_[,1:13])
# pr.nn_ <- pr.nn$net.result*sd(Boston$price_doc) + mean(Boston$price_doc)
# mse.nn <- sum((test$price_doc - pr.nn_)^2)/nrow(pr.nn_)
# mse.nn
# # mse.nn.2 <- sum((test$price_doc - pr.nn_)^2)/nrow(pr.nn_)
# # mse.nn.2
# 
# 
# # plot(test$price_doc,pr.nn_)
# # abline(0,1,lwd=2)
# 
# #SINGLE LAYER...
# 
# library(nnet)
# 
# #single layer
# ns <- nnet(price_doc ~ .,data=train_,size=5,linout=TRUE,decay=5e-4)
# 
# #Illustrate NN
# # plot(ns)
# 
# #Predict
# pr.ns <- predict(ns,test_[,1:13])
# pr.nn_2 <- pr.ns*sd(Boston$price_doc) + mean(Boston$price_doc)
# mse.ns <- sum((test$price_doc - pr.nn_2)^2)/nrow(pr.nn_2)
# mse.ns
# # plot(test$price_doc,pr.nn_2)
# # abline(0,1,lwd=2)
# 
# ########################SUMMUARY DATAFRAME
# 
# models<-c("OLS","RIDGE","LASSO","GAM","REG. TREE", "PRUNED TREE","BAGGING.1","BAGGING.2",
#           "RANDOM FOREST","BOOSTING.1","BOOSTING.2","MULTI-LAYER_NN","SINGLE-LAYER_NN" )
# 
# mse.result<-c(mse.lasso,mse.ridge,mse.lasso,mse.gam,mse.tree,mse.tree.pruned,mse.bag,mse.bag.2,
#               mse.rf,mse.boost,mse.boost.2,mse.nn,mse.ns )
# try<-data.frame(models, mse.result)
# 
# 
