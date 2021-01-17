# data-analysis
python\R\MySQL\VBA
#载入软件包和数据

install.packages(“margin”)   # 下载软件包

install.packages("ROCR")

install.packages("pROC")

install.packages("vcd")

                                             #载入

library(“margin”)             #用于计算平均边际效应

library("pROC")

library(ROCR)                   #用于画ROC线

library(vcd)                     #用于计算kappa



      titanic<- data.frame(Titanic)  # 载入数据



#预处理和查看描述统计
str(titanic)   #查看变量

summary(titanic)   #描述统计

titanic<- titanic[rep(1:nrow(titanic),titanic$Freq),]   #整理成变量-值格式

titanic$Freq<- NuLL  #去掉多余列

summary(titanic)    #再次描述统计

prop.table(table(titanic$Class,titanic$Survived),margin= 1) #联络表统计

prop.table(table(titanic$Sex,titanic$Survived),margin= 1)



#设置训练集和测试集

set.seed(100)  #设随机数  #分出测试集与训练集

index<- sample(2201,1541)

train<- titanic[index,]

test<- titanic[-index,]



#回归

fit<- glm(Survived~.,data=train,family=binomial(link=logit)) 

fit_step<- step(fit)

      summary(fit_step)

 #计算拟合优度
fit_R <- (fit$null.deviance- fit$deviance )/ fit$null.deviance
fit_R
#计算回归系数
coef(fit)
exp(coef(fit)

#计算平均边际效应
effects <- margins(fit)   
summary(effects)
#画AME图
plot(effects,main=”averagemarginal effects”)


#预测诊断
P_train <- predict(fit,type= “response”)     
#训练集回测求概率
Y_train<- P_train >0.5 
#根据概率和临界值确定结果
table <- table(Predicted =Y_train, Actual = train$Survived)
table
P_test <- predict(fit,type =“response”,newdata = test)
#求预测集取值概率
Y_test <- P_test >0.5   
#根据概率和临界值确定结果
table <- table(Predicted =Y_test, Actual = test$Survived)
table                             
#混淆矩阵

accuracy<- (table[1,1]+table[2,2])/sum(table)
accuracy #准确率
error_rate<- (table[2,1]+table[1,2])/sum(table)
error_rate #错误率

sensitivity<- table[2,2]/(table[1,2]+table[2,2])
sensitivity  #真阳率（灵敏度）
specificity<- table[1,1]/(table[1,1]+table[2,1]）
specificity  #真阴率（特异度）
recall <- table[2,2]/(table[2,1]+table[2,2])
recall         #查全率（召回率）

#ROC AUG KAPPA
Y_test<- predict.glm(fit, type = 'response', newdata = test) 
date_roc<- roc(test$Survived,Y_test)
plot(date_roc,main="ROC")
abline(0,1)       
#用真阳率和假阳率做Y轴和X轴#画ROC图

plot(date_roc,print.auc=TRUE, auc.polygon=TRUE,max.auc.polygon=TRUE,auc.polygon.col="red")       
#美化图
auc(date_roc)    
#显示auc值
Kappa(table)    
 #显示kappa值
