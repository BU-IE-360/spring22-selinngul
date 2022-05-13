library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(RcppRoll)
library(GGally)
library(skimr)
library(forecast)
library(dplyr)
library(data.table)


data_UGS <- read_excel("/Users/Macbook/Desktop/IE360_Spring22_HW2_data.xlsx")

colnames(data_UGS)= c("Quarter","UGS","RNUV","NLPG","PU","PG","NUGV","NDGV",
                       "GNPA","GNPC","GNP")
data_UGS$Quarter= as.Date(as.yearqtr(data_UGS$Quarter,format="%Y_Q%q"))
head(data_UGS)


ggplot(data_UGS, aes(x=Quarter,y=UGS))+geom_line()+geom_point()+
  labs(title = "Unleaded Gasoline Sales Over the Quarters")+theme_classic()+scale_x_date(date_breaks = "1 year", date_labels = "%Y")


meanOfSales=roll_mean(data_UGS$UGS[1:28],4,align='left')
varOfSales=roll_var(data_UGS$UGS[1:28],4,align='left')
plot(meanOfSales,
     type='l',col='pink',
     xlab = "time (t)",
     ylab = "Rolling Mean",
     main = "Mean series")

plot(varOfSales,
     type='l',col='pink',
     xlab = "time (t)",
     ylab = "Rolling Variance",
     main = "Variance series")


acf(data_UGS$UGS,lag.max=12,na.action=na.pass)

Quarter=seq(1,4,by=1)
data_UGS=cbind(data_UGS,Q)
modelQuarters=lm(UGS~Quarter,data=data_UGS)
summary(modelQuarters)

Trend=seq(1,32,by=1)
data_UGS=cbind(data_UGS,Trend)
modelTrend=lm(UGS~Quarter+Trend,data=data_UGS)
summary(modelTrend)

modelRNUV =lm(UGS~Quarter+Trend+RNUV,data=data_UGS)
summary(modelRNUV)

modelNLPG =lm(UGS~Quarter+Trend+RNUV+NLPG,data=data_UGS)
summary(modelNLPG)

modelPU =lm(UGS~Quarter+Trend+RNUV+NLPG+PU,data=data_UGS)
summary(modelPU)

modelPG =lm(UGS~Quarter+Trend+RNUV+NLPG+PG,data=data_UGS)
summary(modelPG)

modelNUGV =lm(UGS~Quarter+Trend+RNUV+NLPG+NUGV,data=data_UGS)
summary(modelNUGV)

modelNDGV =lm(UGS~Quarter+Trend+RNUV+NLPG+NDGV,data=data_UGS)
summary(modelNDGV)

modelGNPA =lm(UGS~Quarter+Trend+RNUV+NLPG+GNPA,data=data_UGS)
summary(modelGNPA)

modelGNPC =lm(UGS~Quarter+Trend+RNUV+NLPG+GNPA+GNPC,data=data_UGS)
summary(modelGNPC)

modelGNP =lm(UGS~Quarter+Trend+RNUV+NLPG+GNPA+GNP,data=data_UGS)
summary(modelGNP)

data_UGS$NUGVlag1=lag(data_UGS$NUGV,1)
modelLag1=lm(UGS~Quarter+Trend+RNUV+NLPG+GNPA+NUGVlag1, data=data_UGS)
summary(modelLag1)

data_UGS$UGSlag1=lag(data_UGS$UGS,1)
modelFinal =lm(UGS~Quarter+Trend+RNUV+NLPG+GNPA+UGSlag1,data=data_UGS)
summary(modelFinal)

checkresiduals(modelFinal$residuals)


tmp=copy(data_UGS)
tmp$actual=data_UGS$UGS
tmp$predicted=predict(modelFinal,tmp)
ggplot(tmp ,aes(x=Quarter))+
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted,color='predicted'))+
  labs(title = "Predicted vs Real UGS Values Over the Quarters",y="UGS (1000 m3)" )


pred_2007= data_UGS[29:32,c("Quarter","Trend","RNUV","NLPG","GNPA","UGSlag1")]
prediction = c(0,0,0,0)

for(i in 1:4) {
  prediction[i] = predict(modelFinal,newdata = pred_2007[i,])
  if(i<4){pred_2007[i+1,"UGSlag1"] = prediction[i] }
}
prediction








