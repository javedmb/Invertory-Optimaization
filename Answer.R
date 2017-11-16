#*********  LOADING OF DATA***************
library(readr)

Product_Master <- read_csv("~/R studio/IA_CampusExercise_2017/Product Master.csv")
Sales_Data <- read_csv("~/R studio/IA_CampusExercise_2017/Sales_Data.csv")
Store_Master <- read_csv("~/R studio/IA_CampusExercise_2017/Store Master.csv")

library(readxl)

Promo_Calendar <- read_excel("~/R studio/IA_CampusExercise_2017/Promo Calendar.xlsx")

#CREATING TEST VARIABLES
Sale_test=Sales_Data
Product_test=Product_Master
Store_test=Store_Master
Promo_test=Promo_Calendar

#  ****converstion of date from interger and find outliers
library("lubridate", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("zoo", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("outliers", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("Hmisc", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("stats", lib.loc="/usr/lib/R/library")

#       TO MAKE DATE YYYY/MM/DD
Sale_test$datekey<-ymd(Sale_test$datekey)
View(Sale_test)

write.csv(Sale_test, file = "Q3_IO.csv")



#REMOVING UNWANTED DATES AND NA VALUES
Sale_test = Sale_test[format(Sale_test$datekey,'%Y') != "2005", ]
Sale_test = Sale_test[format(Sale_test$datekey,'%Y') != "2006", ]
Sale_test = na.omit(Sale_test)
write.csv(Sale_test, file = "Q3_Op.csv")

write.csv(Sale_test, file = "Q2_IO.csv")
#QUANTITY CAN NEVER BE SOLD IN NEGATIVE HENCE ASSIGNING 0 TO ALL SUCH VALUES
Sale_test$Quantity = ifelse(Sale_test$Quantity<0, 0, Sale_test$Quantity)
#ASSUMING VALUES GREATER THAN 90000 AS OUTLIERS COMPARED TO OTHER QUANTITIES
Sale_test$Quantity = ifelse(Sale_test$Quantity>10000, 0, Sale_test$Quantity)

View(Sale_test)
write.csv(Sale_test, file = "Q2_Op.csv")
write.csv(Sale_test, file = "Q1_IO.csv")

# View(Sale_test)
library("VIM", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("lattice", lib.loc="/usr/lib/R/library")
#******** Aggregation for entier data set of sale 
Sale_Aggregate <- aggregate(x = Sale_test["Quantity"], 
                            by = list(product=Sale_test$Product, month = substr(Sale_test$datekey, 1, 7), store=Sale_test$Store), 
                            FUN = max)

View(Sale_Aggregate)
write.csv(Sale_Aggregate, file = "Q1_Op.csv")



library("curl", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("TTR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
View(Sale_test)

View(Promo_test)

#MATCH DATE FORMAT TO YYYY/MM/DD
Promo_test$Week<-ymd(Promo_test$Week)

plot(Sale_test$datekey, Sale_test$Quantity)
ggplot( data = Sale_test, aes( Sale_test$datekey, Sale_test$Quantity )) + geom_line() 




#this is done so that I comapre Quantity and later it can be mapped with sale date 
View(Promo_test)
Promo_test$PromoID = ifelse(Promo_test$PromoID=="No Promo", 0, 1)
View(Promo_test)
write.csv(Promo_test, file = "Q4_IO1.csv")



#this gives us total promo in specfic month

library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

Promo_sum_pormo=Promo_test %>% group_by(month=floor_date(Week, "month")) %>%
  summarize(PromoID=sum(PromoID))
Promo_sum_pormo = Promo_sum_pormo[format(Promo_sum_pormo$month,'%Y') != "2017", ]#removeal of 2017 as we cant bind


View(Promo_sum_pormo)
write.csv(Promo_sum_pormo, file = "Q4_IO2.csv")
Sale_sum_qty=Sale_test %>% group_by(month=floor_date(datekey, "month")) %>%
  summarize(Quantity=sum(Quantity))
write.csv(Sale_sum_qty, file = "Q4_IO2.csv")
View(Sale_sum_qty)

#commbing the data
Sale_Promo=cbind(Sale_sum_qty,Promo_sum_pormo)

View(Sale_Promo)
Sale_Promo <- subset(Sale_Promo, select= c(2,4))#selecting specific column
write.csv(Sale_Promo, file = "Q4_O1.csv")
Sale_Promo
#this is for preditcding the future sale
s.date <- c(2015,1)    #start date - factual
e.date <- c(2016,12)   #end date - factual
f.s.date <- c(2017,1)  #start date - prediction
f.e.date <- c(2017,4) #end date - prediction

#soomthning

dummy <- ts(Sale_sum_qty$Quantity, frequency = 12, start=s.date, end=e.date)
dummy
write.csv(dummy, file = "Q5_I01.csv")
plot(dummy)

dummy2 <- ts(Promo_sum_pormo$PromoID, frequency = 12, start =s.date, end=f.e.date)
write.csv(dummy2, file = "Q5_IO2.csv")
#For fiting the series model
library("RcppArmadillo", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("forecast", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("TSA", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

s.date <- c(2015,1)    #start date - factual
e.date <- c(2016,12)   #end date - factual
f.s.date <- c(2017,1)  #start date - prediction
f.e.date <- c(2017,4) #end date - prediction

past <- window(dummy2, s.date, e.date)
future <- window(dummy2, f.s.date, f.e.date)
write.csv(past, file = "Q5_O1.csv")
write.csv(future, file = "Q5_O2.csv")
#graph ploted
#applying ARIMA for AND GETTING THE MODEL AND PEDICTING THE VALUE OF QUANTITY
write.csv(dummy, file = "Q6_IO.csv")
fit <- auto.arima(dummy, xreg=past, stepwise = FALSE, approximation = F)
forecase <- forecast(fit, xreg = future)
forecase
View(forecase)
write.csv(forecase, file = "Q6_O.csv")
plot(forecase)
summary(forecase)


